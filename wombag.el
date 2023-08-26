;;; wombag.el --- A Wallabag Client      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (emacsql "3.1.1") (request "0.3.3") (compat "29.1.0"))
;; Keywords: multimedia, extensions
;; URL: https://github.com/karthink/wombag

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Wombag is a Wallabag client for Emacs.
;;
;; Wallabag is an (optionally self-hosted) read-it-later or web page archival
;; service.  To use this package you need a Wallabag account, or access to a
;; server running Wallabag.
;;
;; Usage:
;;
;; Set the following parameters:
;; (setq wombag-host "https://app.wallabag.it" ;where you access Wallabag
;;       wombag-username "my-wallabag-username"
;;       wombag-password "my-wallabag-password"
;;       wombag-client-id "abcdefgh1234"
;;       wombag-client-secret "abcdefgh1234"))
;;
;; - Start Wombag with M-x `wombag'.
;;
;; - Sync your reading list with the server using `wombag-sync' (`G' in the
;;   Wombag buffer)
;;
;; - Filter entries with `s', press `?' for help with searching.
;;
;; - You can read, archive, star, tag or delete entries.
;; - You can bookmark Wombag searches/entries, and navigate entries with `imenu'.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'compat)
(require 'map)
(require 'request)
(require 'json)
(require 'wombag-options)
(require 'wombag-db)

(declare-function w-search-buffer 'wombag-search)
(declare-function w-search-mode 'wombag-search)
(declare-function w-search-update--force 'wombag-search)
(defvar w-retrieving)


;;; Utility vars and functions
(defvar w-token-file (file-name-concat w-dir "token")
  "File used to store the token.

NOTE: This is currently not implemented.")
(defvar w-token nil)
(defvar w-version nil)
(defvar w-appname nil)
(defvar w-data nil)

(defvar w--debug t)

(cl-defun w--debug (&key data error-thrown symbol-status response &allow-other-keys)
  "Handle `request' errors when interacting with Wombag.

DATA, ERROR-THROWN, SYMBOL-STATUS and RESPONSE have their usual
meanings in a `request' callback, see `request'."
  (let ((status-code (request-response-status-code response))
        (error-desc (cdr-safe error-thrown))
        (data-desc (map-elt data 'error_description)))
    (unless (eq status-code 401)        ;Handled elsewhere
      (setq w-retrieving
            (concat (format "%S: %S " symbol-status error-desc) data-desc))
      (when w--debug
        (with-current-buffer (get-buffer-create "*Wombag Error*")
          (with-silent-modifications (erase-buffer))
          (insert ";; Request failed with error: \n" (pp-to-string symbol-status)
                  "\n\n;; Error data is:\n" (pp-to-string error-thrown)
                  "\n\n;; Data is:\n" (pp-to-string data))
          (unless (eq major-mode 'emacs-lisp-mode) (emacs-lisp-mode))
          (display-buffer-in-side-window (current-buffer)
                                         '((side . bottom)
                                           (slot . -40)
                                           (window-height . 10))))))))


;;; Fetching data: Remote to local

(cl-defun w-get-token (&key callback args)
"Request a Wallabag token.

If provided, call CALLBACK with ARGS afterwards."
  (interactive)
  (request (format "%s/oauth/v2/token" w-host)
    :parser 'json-read
    :params `(("username"      . ,w-username)
              ("password"      . ,w-password)
              ("client_id"     . ,w-client-id)
              ("client_secret" . ,w-client-secret)
              ("grant_type"    . "password"))
    :headers '(("Content-Type" . "application/json"))
    :sync t
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setf w-token (alist-get 'access_token data))
                (when w--debug (message "Wombag token acquired."))
                (when callback (apply callback args))))
    :error #'w--debug))

(defun w--retry-with-token (func &rest args)
  "Retrieve a Wombag token and call FUNC with ARGS."
  (cl-function
   (lambda (&key data error-thrown &allow-other-keys)
     (if (not (equal (map-elt data 'error) "invalid_grant"))
         (message (format "Request failed with: %S" error-thrown))
       (setq w-retrieving "Authenticating...")
       (w-get-token :callback func :args args)))))

(defsubst w--sync-message (num-total)
  "Adjust Wombag header message stating NUM-TOTAL."
  (setq w-retrieving
        (if (= num-total 0)
            "Retrieving... already up to date"
          (format "Retrieving... %d entries added" num-total))))

;;----------------8<-------------------
;; (defvar w-all-entries nil)
;; (defvar w-local-ids nil)
;;----------------8<-------------------
(cl-defun w-sync (&key since page num-total local-ids full)
  "Synchronize the local Wombag database.

This will update the local state of Wombag to that the server:
- Fetch new entries since the last update
- Update all metadata (archived/starred/annotations etc)

With `prefix-arg' \\[universal-argument], query for date to sync
from.

By default, it will not delete local entries that have been
deleted on the server.

With double prefix-arg \\[universal-argument]
\\[universal-argument], do a full sweep of the database and
delete all entries not on the Server.
NOTE: This is not yet implemented.

Keywords:

SINCE: Unix timestamp or date formatted as \"YYYY-MM-DD\" to sync
upwards from.  (Determined automatically when not provided.)

FULL: If non-nil, perform a full sweep of deleted entries on the
server and delete them locally.

The remaining keywords are for internal use only

PAGE: Page number of entries.
NUM-TOTAL: Running total of new entries
LOCAL-IDS: Ids from SINCE available locally."
  (interactive
   (list :since
         (when (= (prefix-numeric-value current-prefix-arg) 4)
           (read-string "Sync from (2023-09-01): "))
         :full (and (= (prefix-numeric-value current-prefix-arg) 16)
                    (y-or-n-p
                     "Do a full sweep of the database (This will transfer a lot of data)? "))))
  (unless num-total
    (setq w-retrieving "Retrieving..."
          num-total (or num-total 0)))
  ;;----------------8<-------------------
  ;; (setq w-all-entries nil
  ;;       w-local-ids nil)
  ;;----------------8<-------------------
  (if since
      (unless (numberp since)
        (if (string-match-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" since)
            (setq since (floor (float-time (date-to-time since))))
          (user-error "Date %s does not match format YYYY-MM-DD" since)))
    (setq since (or (caar (w-db-query `[:select fetch-all :from last_update])) 1)))
  (request (format "%s/api/entries" w-host)
    :parser 'json-read
    :params `(("access_token" . ,w-token)
              ("sort"         . "created")
              ("order"        . "desc")
              ("page"         . ,(or page 1))
              ("perPage"      . 30)
              ("detail"       . "full")
              ("since"        . ,since))
    :headers '(("Content-Type" . "application/json"))
    :status-code `((401 . ,(w--retry-with-token #'w-sync
                            :page page :since since :full full))
                   (404 . ,(lambda (&rest _)
                             (w-db-update-date (float-time))
                             (w--sync-message num-total)
                             (when full (w--sweep-deleted-entries)))))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       "Update Wombag db if necessary"
       (let ((num-new)
             (all-entries (map-nested-elt data '(_embedded items)))
             (local-ids (or local-ids
                            (apply #'nconc
                                   (w-db-query
                                    `[:select id :from items
                                      :where (>= updated_at ;created_at
                                              ,(format-time-string "%Y-%m-%dT%H:%M:%S" since t))
                                      :order-by (desc id)])))))
         ;;----------------8<-------------------
         ;; (setq w-all-entries
         ;;       (nconc w-all-entries all-entries))
         ;; (setq w-local-ids
         ;;       (nconc w-local-ids local-ids))
         ;;----------------8<-------------------
         (if local-ids
             (let ((server-ids))
               (cl-loop for entry across all-entries
                        for id = (map-elt entry 'id)
                        do (push id server-ids)
                        if (memq id local-ids)
                        collect entry into updated-entries
                        else collect entry into new-entries
                        finally do
                        (when new-entries
                          (w--insert-entries :data (vconcat new-entries) :replace t))
                        (when updated-entries
                          (w--insert-entries :data (vconcat updated-entries) :replace t))
                        (setq num-new (length new-entries)))
               (when-let ((deleted-ids (cl-set-difference local-ids server-ids)))
                 (w-db-delete (vconcat deleted-ids))))
           (unless (= 0 (length all-entries)) (w--insert-entries :data all-entries))
           (setq num-new (length all-entries)))
         (if (>= (length all-entries) 30)
             ;; There might be more entries
             (run-with-idle-timer
              1 nil #'w-sync
              :page (1+ (or page 1))
              :since since
              :num-total (+ num-new num-total)
              :local-ids local-ids
              :full full)
           (w-db-update-date (float-time))
           (when full (w--sweep-deleted-entries)))
         (w--sync-message (+ num-new num-total)))))
    :error #'w--debug))

(defun w--sweep-deleted-entries ()
  ";TODO: Sweeping all deleted entries not implemented yet."
  (message "Sweeping all deleted entries not implemented yet."))

;;;; Updating the database:
(cl-defun w--insert-entries (&key data replace &allow-other-keys)
  "Insert entries in DATA into the Wombag database.

If keyword REPLACE is non-nil, replace entries if they already exist."
  (condition-case-unless-debug parse-error
      (if (not data)
          (message "Parse error! Could not extract entry data.")
        (or (vectorp data) (setq data (vector data)))
        (when w--debug (message "Running insert entry"))
        (prog1 (w-db-insert data replace)
          (let ((inhibit-message t)
                (num (length data)))
            (if (= num 1)
                (message "Entry added to Wombag.")
              (message "%d entries added to Wombag." num)))
          (when-let* (((featurep 'w-search))
                      (buf (w-search-buffer :if-live))
                      (win (get-buffer-window buf))
                      ((window-live-p win)))
            (with-selected-window win
              (w-search-update--force :keep-header)))))
    (error (message "Couldn't insert entries into database: %S" (car parse-error)))))


;;; Sending data: Local to remote

;;;###autoload
(defun w-add-entry (url &optional tags)
  "Add an entry (from URL) to the Wombag database.

Interactively, query for TAGS as well.  TAGS must be a
comma-separated string."
  (interactive
   (list (read-string "URL to add to Wombag: ")
         (split-string (read-string "Tags (comma separated): ") "," t "\\s-+")))
  (request (format "%s/api/entries" w-host)
    :parser 'json-read
    :type "POST"
    :params `(("access_token" . ,w-token))
    :data (json-encode
           `(("url" . ,url)
             ("archive" . 0)
             ("starred" . 0)
             ("tags" . ,(or tags ""))))
    :headers '(("Content-Type" . "application/json"))
    :error #'w--debug
    :status-code `((401 . ,(w--retry-with-token
                            (lambda () (w-add-entry url tags)))))
    :success #'w--insert-entries))


;;; Main command
;;;###autoload
(defun wombag ()
  "Open Wombag."
  (interactive)
  (w-db-ensure)
  (require 'w-search)
  (pop-to-buffer-same-window (w-search-buffer))
  (unless (eq major-mode 'w-search-mode)
    (w-search-mode)))

(provide 'wombag)
;;; wombag.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("w-" . "wombag-"))
;; End:
