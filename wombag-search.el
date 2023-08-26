;;; wombag-search.el --- Wombag search interface     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>

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

;; Wombag search interface

;;; Code:
(require 'wombag-db)
(require 'wombag-options)
(require 'wombag-show)
(require 'bookmark)
(require 'request)

(bookmark-maybe-load-default-file)

(declare-function w-show-entry "wombag-show")
(declare-function wombag "wombag")
(declare-function w-add-entry "wombag")
(declare-function w-sync "wombag")
(declare-function w--debug "wombag")
(declare-function w--retry-with-token "wombag")
(defvar w-token nil)

(defun w-search-buffer (&optional livep)
  "Get or create buffer *wombag-search*.

With optional arg LIVEP, only return the buffer if it is live."
  (if livep
      (get-buffer w-search-buffer-name)
    (get-buffer-create w-search-buffer-name)))

(defvar w-retrieving nil
  "Header message for state of Wombag.")


;;; Bookmark support

;;;###autoload
(defun w-search-bookmark-handler (record)
  "Jump to the wombag-search bookmarked location in RECORD."
  (wombag)
  (setq w-search-filter (bookmark-prop-get record 'filter))
  (w-search-update--force))

(defun w-search-bookmark-make-record ()
  "Return a bookmark record for the current w-search buffer."
  `(,(format "(Wombag search) %s" w-search-filter)
    (filter . ,w-search-filter)
    (handler . w-search-bookmark-handler)))


;;; Searching and live searching
(defvar w-search-filter-active nil)

(defvar w-search-filter-syntax-table
  (let ((table (make-syntax-table)))
    (prog1 table
      (dolist (char (list ?# ?+ ?- ?= ?@ ?! ?* ?~ ?/ ?> ?< ?: ?& ?^ ?.))
        (modify-syntax-entry char "w" table))))
  "Syntax table active when editing the filter in the minibuffer.")

(defun w-search--minibuffer-setup ()
  "Set up the minibuffer for live filtering Wombag."
  (when w-search-filter-active
    (set-syntax-table w-search-filter-syntax-table)
    (when w-search-filter-help (w-search--show-filter-help))
    (when (eq :live w-search-filter-active)
      (add-hook 'post-command-hook (w-search--live-updater) nil :local))))

(defconst w-search--filter-help-string
  (cl-labels ((spc (to) (propertize " " 'display `(space :align-to ,to)))
              (hlp (text) (propertize text 'face 'help-key-binding))
              (box (text) (propertize text 'face '(:underline t :weight semi-bold)))
              (desc (key s1 desc &optional s2)
                (concat (hlp key) (spc s1) desc (and s2 (spc s2)))))
    (concat
     (propertize
      "Filter Syntax [All filters are \"AND\" composed; Regexp support is not available]"
      'face '(inherit shadow :height 1.1))
     (propertize "\n\n" 'face '(inherit :height 0.8))
     (hlp ".") (spc 5) (concat "Currently Reading")
     (spc 31) (box "Match") (spc 55) (box "Date added")
     "\n"
     (desc "#20" 5 "Limit to 20 articles" 30)
     (desc " text" 36 "in content/title" 55)
     (desc "@2020-06-27" 69 "on")
     "\n"
     (desc "<12" 5 "reading time < 12 mins" 30)
     (desc "*text" 36 "in title" 55)
     (desc "@2020-06-27--" 69 "after")
     "\n"
     (desc ">12" 5 "reading time > 12 mins" 30)
     (desc "/text" 36 "in full url" 55)
     (desc "@--2022-06-27" 69 "before")
     "\n"
     (desc "+tag" 5 "tagged as 'tag'" 30)
     (desc "^text" 36 "in domain name" 55)
     (desc "@2022-01-11--2022-06-27" 79 "between")
     "\n"
     (spc 30)
     (desc "=text" 36 "in author's name")
     "\n"
     (box "Flags") (spc 55) (box "Date published")
     "\n"
     (desc "**" 4 "Starred" 30)
     (spc 31) (box "Not match") (spc 55)
     (desc ":2020-06-27" 69 "on")
     "\n"
     (desc "!*" 4 "Not starred" 30)
     (desc "!text" 36 "in content/title" 55)
     (desc ":2020-06-27--" 69 "after")
     "\n"
     (desc "&&" 4 "Archived" 30)
     (desc "~text" 36 "in full url" 55)
     (desc ":--2022-06-27" 69 "before")
     "\n"
     (desc "!&" 4 "Not archived" 30)
     (desc "-tag" 36 "not tagged 'tag'" 55)
     (desc ":2022-01-11--2022-06-27" 79 "between"))))

(defun w-search--show-filter-help ()
  "Display search filter help for Wombag."
  (interactive)
  (let* ((buf (get-buffer-create "*wombag filter help*"))
         (win (get-buffer-window buf)))
    (if (window-live-p win)
        (quit-window nil win)
      (display-buffer
       buf `((display-buffer-in-side-window
              display-buffer-at-bottom)
             (side . bottom)
             (slot . -20)
             (window-height . 14)))
      (with-current-buffer buf
        (when (= (buffer-size buf) 0)
          (setq truncate-lines t)
          (insert w-search--filter-help-string)
          (special-mode)
          (setq-local mode-line-format nil))))))

(defvar w-search--filter-overflow nil
  "Flag when there are more entries than fit on the screen.")

(defun w-search--live-updater (&optional delay)
  "Return a live-update function for filtering with Wombag.

DELAY is the duration for which user input is debounced before
querying the database."
  (let ((prev-filter)
        (debounce-timer)
        (delay (or delay 0.20)))
    (lambda ()
      (if (timerp debounce-timer)
          (timer-set-idle-time debounce-timer delay)
        (unless (eq this-command 'w-search-live-filter)
          (setq debounce-timer
                (run-with-idle-timer
                 delay nil
                 (lambda (buffer)
                   (cancel-timer debounce-timer)
                   (setq debounce-timer nil)
                   (let ((current-filter (string-trim-right (minibuffer-contents-no-properties))))
                     (unless (equal prev-filter current-filter)
                       (with-current-buffer buffer
                         (let* ((user-limit (or (and (string-match "#\\([[:digit:]]+\\)" current-filter)
                                                     (string-to-number (match-string 1 current-filter)))
                                                most-positive-fixnum))
                                (wombag-buf (w-search-buffer))
                                (window (get-buffer-window wombag-buf))
                                (height (window-total-height window))
                                (limiter (if window (format " #%d " height) " #1 "))
                                (w-search-filter (if (> user-limit height)
                                                     (concat current-filter limiter)
                                                   current-filter)))
                           (w-search-update :force)
                           (setq w-search--filter-overflow (>= (car (buffer-line-statistics wombag-buf))
                                                               (window-text-height window))
                                 prev-filter current-filter))))))
                 (current-buffer))))))))

(defun w-search--live-update ()
  ";TODO:"
  (when (eq :live w-search-filter-active)
    (let ((buffer (w-search-buffer))
          (current-filter (minibuffer-contents-no-properties)))
      (when buffer
        (with-current-buffer buffer
          (let* ((user-limit (or (and (string-match "#\\([[:digit:]]+\\)" current-filter)
                                      (string-to-number (match-string 1 current-filter)))
                                 most-positive-fixnum))
                 (window (get-buffer-window (w-search-buffer)))
                 (height (window-total-height window))
                 (limiter (if window
                              (format " #%d " height)
                            " #1 "))
                 (w-search-filter (if (< height user-limit)
                                      (concat current-filter limiter)
                                    current-filter)))
            (w-search-update :force)
            (setq w-search--filter-overflow (< (window-end) (point-max)))))))))

(defvar-keymap w-search--filter-map
  :doc "Keymap for Wombag filter editing."
  :parent minibuffer-local-map
  "?" #'w-search--show-filter-help)

(defvar w-search--filter-history nil)

(defun w-search-live-filter ()
  "Live search the Wombag database."
  (interactive)
  (let ((line (with-current-buffer (w-search-buffer)
                (line-number-at-pos))))
    (unwind-protect
        (let ((w-search-filter-active :live))
          (setq w-search-filter
                (read-from-minibuffer
                 "Filter (? for help): " w-search-filter w-search--filter-map
                 nil w-search--filter-history)))
      (w-search-update :force)
      (setq w-search--filter-overflow nil
            w-retrieving nil)
      (goto-char (point-min))
      (forward-line (1- line))
      (when (eobp) (forward-line -1)))))

(defvar w-search-columns
  '(id title created_at reading_time is_archived
    is_starred tag domain_name url published_by
    published_at))

(defun w-search-update (&optional _)
  ";TODO:"
  (let ((entries (w-db-get-entries
                  (w-search-parse-filter w-search-filter w-search-columns)
                  w-search-columns)))
    (w-search-print-entries entries)
    (length entries)))

(defun w-search--token (token coll)
  (let ((re (substring token 1)))
    (if (string-empty-p re)
        coll (cons re coll))))

(defun w-search--date-token (element)
  (let ((re (substring element 1)))
    (unless (string-empty-p re)
      (pcase-let ((`(,from ,to) (split-string re "--")))
        (when (equal from "") (setq from "1970-01-01"))
        (when (equal to "") (setq to (format-time-string "%Y-%m-%d" (current-time))))
         (cons from to)))))

(defun w-search-parse-filter (filter &optional columns)
  "Parse the elements of a search FILTER into an emacsql query.

Query should ask for COLUMNS, or `wombag-search-columns'."
  (let ((matches) (limit) (urls) (not-urls) (not-matches)
        (have-tag) (not-have-tag) (titles) (under-time) (over-time)
        (domains) (authors) (add-dates) (pub-dates)
        (starred) (archived) (unstarred) (unarchived) (reading))
    (cl-loop for element in (split-string filter)
             for type = (aref element 0)
             do (cl-case type
                  (?+ (setq have-tag (w-search--token element have-tag)))
                  (?- (setq not-have-tag (w-search--token element not-have-tag)))
                  (?/ (setq urls (w-search--token element urls)))
                  (?~ (setq not-urls (w-search--token element not-urls)))
                  (?! (pcase (ignore-errors (aref element 1))
                        ('?* (setq unstarred t))
                        ('?& (setq unarchived t))
                        (_  (setq not-matches (w-search--token element not-matches)))))
                  (?* (if (eq (ignore-errors (aref element 1)) ?*)
                          (setq starred t)
                        (setq titles (w-search--token element titles))))
                  (?& (if (eq (ignore-errors (aref element 1)) ?&)
                          (setq archived t)
                        (push element matches)))
                  (?^ (setq domains (w-search--token element domains)))
                  (?= (setq authors (w-search--token element authors)))
                  (?@ (setq add-dates (w-search--date-token element)))
                  (?: (setq pub-dates (w-search--date-token element)))
                  (?> (setf over-time
                            (list (string-to-number (substring element 1)))))
                  (?< (setf under-time
                            (list (string-to-number (substring element 1)))))
                  (?# (setf limit (string-to-number (substring element 1))))
                  (otherwise (if (string= element ".")
                                 (setq reading t)
                               (push element matches)))))
    (apply #'vector
           (append (if columns
                       `(:select ,(vconcat columns) :from items)
                     '(:select * :from items))
                   `(,@(when (or urls matches titles have-tag under-time over-time
                              not-have-tag not-matches not-urls domains authors add-dates
                              pub-dates starred unstarred archived unarchived reading)
                        (list :where
                         `(and
                           ,@(when starred `((= is_starred 1)))
                           ,@(when archived `((= is_archived 1)))
                           ,@(when unstarred `((= is_starred 0)))
                           ,@(when unarchived `((= is_archived 0)))
                           ,@(when urls
                              (cl-loop for link in urls
                               collect `(like url ,(concat "%" link "%"))))
                           ,@(when matches
                              (cl-loop for text in matches
                               collect `(or (like title ,(concat "%" text "%"))
                                         (like content ,(concat "%" text "%")))))
                           ,@(when titles
                              (cl-loop for title in titles
                               collect `(like title ,(concat "%" title "%"))))
                           ,@(when authors
                              (cl-loop for author in authors
                               collect `(like published_by ',(concat "%" author "%"))))
                           ,@(when have-tag
                              (cl-loop for tag in have-tag
                               collect `(like tags ',(concat "%\"" tag "\"%"))))
                           ,@(when domains
                              (cl-loop for domain in domains
                               collect `(like domain_name ,(concat "%" domain "%"))))
                           ,@(when not-have-tag
                              (cl-loop for tag in not-have-tag
                               collect `(not (like tags ',(concat "%\"" tag "\"%")))))
                           ,@(when add-dates
                              (if (cdr add-dates)
                                  `((<= ,(car add-dates) created_at ,(cdr add-dates)))
                                `((like created_at ,(concat (car add-dates) "%")))))
                           ,@(when pub-dates
                              (if (cdr pub-dates)
                                  `((<= ,(car pub-dates) published_at ,(cdr pub-dates)))
                                `((like published_at ,(concat (car pub-dates) "%")))))
                           ,@(when (or under-time over-time)
                              `((< ,@over-time reading_time ,@under-time)))
                           ,@(when not-urls
                              (cl-loop for link in not-urls
                               collect `(not (like url ,(concat "%" link "%")))))
                           ,@(when not-matches
                              (cl-loop for text in not-matches
                               collect `(not (or (like title ,(concat "%" text "%"))
                                              (like content ,(concat "%" text "%"))))))
                           ,@(when reading
                              (if-let ((ids (hash-table-keys w-show--positions-table)))
                                `((in id ,(vconcat ids)))
                               `((= id 0)))))))
                     :order-by (desc id)
                     ,@(when limit (list :limit limit)))))))


;;; Search buffer display

(defvar w-search-trailing-width 38)

(defvar w-search-min-title-width 30)
(defvar w-search-max-title-width 70)
(defvar w-search-title-width w-search-min-title-width)

(defun w-search-print-entries (entries)
  "Print ENTRIES to the Wombag search buffer."
  (with-current-buffer (w-search-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq w-search-title-width
              (min (max (- (window-width (get-buffer-window "*wombag-search*"))
                           10 w-search-trailing-width)
                        w-search-min-title-width)
                   w-search-max-title-width))
        (mapc #'w-search-print-entry--default entries))))

(defun w-search-format-entry (entry)
  "Wombag ENTRY as string."
  (let* ((title (or (alist-get 'title entry) "NO TITLE"))
         (created-at (alist-get 'created_at entry))
         (published-at (alist-get 'published_at entry))
         (reading-time (alist-get 'reading_time entry))
         (is-archived (alist-get 'is_archived entry))
         (is-starred (alist-get 'is_starred entry))
         (tag (alist-get 'tag entry))
         (domain-name (or (alist-get 'domain_name entry) ""))
         (authors (mapconcat #'identity (alist-get 'published_by entry) ",")))
    (format "%s %s %s %s (%s)"
            ;; (substring created-at 0 10)
            (propertize
             (or (substring created-at 0 10)
                 (and published-at (substring published-at 0 10)))
             'face 'w-date-face)
            (propertize
             (truncate-string-to-width title w-search-title-width nil ?  nil)
             'face (if (= is-archived 1) 'w-archive-face 'w-title-face))
            (propertize (format "%3d min" reading-time) 'face 'w-reading-time-face)
            (propertize ;; (if (string= authors "") domain-name authors)
             domain-name 'face 'w-domain-face)
            (concat
             (and (eq is-starred 1)
                  (concat (propertize "★" 'face 'w-starred-face)
                          (unless (string-empty-p tag) ",")))
             (propertize tag 'face 'w-tag-face)))))

(defun w-search-print-entry--default (entry)
  "Print ENTRY to the buffer."
  (unless (equal entry "")
    (let (beg end)
      (setq beg (point))
      (insert (w-search-format-entry entry))
      (setq end (point))
      ;; format the tag and push into attr alist
      (put-text-property beg end 'w-entry entry)
      (put-text-property beg end 'w-id (alist-get 'id entry))
      (insert "\n"))))

(defun w-search-header ()
  "Return the string to be used as the wombag header."
  (format "%s: %s %s"
          (propertize "Wombag" 'face font-lock-preprocessor-face)
          (if w-retrieving
              (propertize w-retrieving 'face font-lock-warning-face)
            (concat
             (propertize (format "Total %s, " (if w-search--filter-overflow
                                                  "??" (car (buffer-line-statistics))))
                         'face font-lock-warning-face)
             (propertize w-search-filter 'face 'font-lock-keyword-face)
             ;; (propertize (let ((len (length (w-find-marked-candidates))))
             ;;               (if (> len 0)
             ;;                   (concat "Marked: " (number-to-string len)) "")) 'face font-lock-negation-char-face)
             ))
          (concat
           (propertize " " 'display `(space :align-to (- right ,(length w-host))))
           (propertize (format "%s" w-host) 'face font-lock-type-face))))


;;; Search buffer movement and interaction

(defun w-search-update--force (&optional keep-header)
  "Force refresh view of the article listing.

When KEEP-HEADER is non-nil, don't reset the header message."
  (interactive)
  (let ((line (line-number-at-pos)))
    (w-search-update :force)
    (unless keep-header (setq w-retrieving nil))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun w-search-quit-window (&optional arg)
  "Quit Wombag and close the database.

With prefix ARG only quit Wombag."
  (interactive "P")
  (unless arg (w-db--close))
  (when-let ((buf (get-buffer w-show-buffer-name)))
    (if-let ((win (get-buffer-window buf)))
      (quit-window 'kill win)
      (kill-buffer buf)))
  (quit-window 'kill))

(defun w-search-selected (&optional ignore-region-p)
  ";TODO:"
  (let ((use-region (and (not ignore-region-p) (use-region-p))))
    (let ((beg (if use-region (region-beginning) (point)))
          (end (if use-region (region-end) (point)))
          (entries))
      (save-excursion
        (goto-char beg)
        (while (and (not (eobp)) (<= (point) end))
          (push (get-text-property (point) 'w-entry) entries)
          (forward-line 1))
        (if ignore-region-p
            (car entries)
          (nreverse entries))))))

(defun w-search-show-entry (entry)
  "Show Wombag ENTRY at point."
  (interactive (list (w-search-selected :ignore-region)))
  (when entry (w-show-entry entry)))

(defmacro w-search--with-entry (&rest body)
  `(if-let ((entry (get-text-property (point) 'w-entry)))
       ,(macroexp-progn body)
    (message "No Wombag entry at point.")))

(defun w-search-browse-url ()
  "Open Wombag entry at point using `browse-url'."
  (interactive)
  (w-search--with-entry
   (when-let ((url (map-elt entry 'url)))
     (funcall w-browse-url-function url))))

(defun w-search-eww-open ()
  "Open Wombag entry at point in EWW."
  (interactive)
  (w-search--with-entry
   (when-let ((url (map-elt entry 'url)))
     (eww url))))

(defun w-search-copy ()
  "Copy URL of Wombag entry at point."
  (interactive)
  (w-search--with-entry
   (when-let ((url (map-elt entry 'url)))
     (kill-new url)
     (message "Copied to kill-ring: \"%s\"" url))))

(defun w-search--eob ()
  "Go to the last line of the Wombag search buffer."
  (interactive)
  (prog1 (goto-char (point-max))
    (when (eq (point) (line-beginning-position))
      (forward-line -1))))


;;; Resume search session
;;;###autoload
(defun w-resume ()
  "Resume reading Wombag articles.

This limits the Wombag listing to articles that you have begun
reading but not finished."
  (interactive)
  (call-interactively #'wombag)
  (if-let* ((ids (hash-table-keys w-show--positions-table))
            (entries (w-db-get-entries
                      `[:select ,(vconcat w-search-columns) :from items
                        :where (in id ,(vconcat ids))
                        :order-by (desc created_at)]
                      w-search-columns)))
      (let ((line (with-current-buffer (w-search-buffer)
                    (line-number-at-pos))))
        (w-search-print-entries entries)
        (goto-char (point-min))
        (when line
          (forward-line (1- line))
          (when (eobp) (forward-line -1)))
        (length entries))
    (message "No entries to resume.")))


;;; Update/delete entries in search buffer
(defconst w-search--update-fields
  '(("archive" is_archived "Archiving...")
    ("starred" is_starred "Starring...")))

(defun w-search--updater (method)
  "Update Wombag entry at point using METHOD."
  (lambda ()
    "Update Wombag entry at point."
    (interactive)
    (pcase-let* ((id (get-text-property (point) 'w-id))
                (entry (get-text-property (point) 'w-entry))
                (`(,field ,msg) (map-elt w-search--update-fields method))
                (oldval (map-elt entry field))
                (location (point)))
      (let ((newval (pcase oldval
                      ('1 0)
                      ('0 1))))
        (setq w-retrieving msg)
        (request (format "%s/api/entries/%d" w-host id)
          :type "PATCH"
          :params `(("access_token" . ,w-token)
                    ("detail" . "metadata"))
          :parser 'json-read
          :data (json-encode `(("access_token" . ,w-token)
                              (,method . ,newval)))
          :headers '(("Content-Type" . "application/json")
                     ("Prefer" . "return=minimal"))
          :status-code `((401 . ,(w--retry-with-token
                                  (intern-soft (format "wombag-search-%s-entry" method)))))
          :success
          (cl-function
           (lambda (&key data &allow-other-keys)
             (setq w-retrieving nil)
             (with-current-buffer (w-search-buffer)
               (let ((state (map-elt data field))
                     (updated-at (map-elt data 'updated_at))
                     (inhibit-read-only t))
                 ;; (message "%S" data)
                 ;; (message "state: %S" state)
                 (w-db-update field id state)
                 (w-db-update 'updated_at id updated-at)
                 (save-excursion
                   (goto-char location)
                   (delete-line)
                   (w-search-print-entry--default
                    (car (w-db-get-ids id))))))))
          :error #'w--debug)))))

(defalias 'w-search-archive-entry (w-search--updater "archive") "Archive entry at point")
(defalias 'w-search-starred-entry (w-search--updater "starred") "Star entry at point")

(defun w-search-add-tags (addtags remtags)
  "Add or remove tags to Wombag entry at point.

NOTE: Removing tags is not yet implemented.

ADDTAGS and REMTAGS are the tags to be added and removed
respectively."
  (interactive (let ((alltags (split-string
                               (read-string "Tag or untag (+tag1 -tag2 ...): "
                                            (if (equal this-command 'w-search-remove-tags)
                                                "-" "+")))))
                 (cl-loop for tag in alltags
                          when (string-prefix-p "+" tag) collect (substring tag 1) into addtags
                          when (string-prefix-p "-" tag) collect (substring tag 1) into remtags
                          finally return (list addtags remtags))))
  (let ((id (get-text-property (point) 'w-id))
        (location (point)))
    (setq w-retrieving "Tagging...")
    (when addtags
      (request (format "%s/api/entries/%d/tags" w-host id)
        :type "POST"
        :params `(("access_token" . ,w-token)
                  ("detail" . "metadata"))
        :parser 'json-read
        :data (json-encode `(("access_token" . ,w-token)
                             (tags           . ,(mapconcat #'identity addtags ","))))
        :headers '(("Content-Type" . "application/json")
                   ("Prefer" . "return=minimal"))
        :status-code `((401 . ,(w--retry-with-token #'w-search-add-tags addtags remtags)))
        :success
        (cl-function
         (lambda (&key data &allow-other-keys)
           (setq w-retrieving nil)
           (with-current-buffer (w-search-buffer)
             (let ((tags (map-elt data 'tags))
                   (updated-at (map-elt data 'updated_at))
                   (inhibit-read-only t))
               ;; (message "%S" data)
               ;; (message "state: %S" state)
               (print (map-elt data 'tags) (get-buffer "*scratch*"))
               (w-db-update
                'tag  id (mapconcat (lambda (el) (map-elt el 'label)) (map-elt data 'tags) ","))
               (when (vectorp tags) (setq tags `',tags))
               (w-db-update 'tags id tags)
               (w-db-update 'updated_at id updated-at)
               (save-excursion
                 (goto-char location)
                 (delete-line)
                 (w-search-print-entry--default
                  (car (w-db-get-ids id))))))))
        :error #'w--debug))
    (when remtags
      (setq w-retrieving nil)
      (message "Removing tags is not implemented, sorry!"))))

(defalias 'w-search-remove-tags 'w-search-add-tags)

(defun w-search-delete-entry (&optional no-confirm)
  "Delete Wombag entry at point.

When NO-CONFIRM is non-nil, do not ask for confirmation."
  (interactive (list (not (yes-or-no-p "Delete entry at point? "))))
  (when-let (((not no-confirm))
             (id (get-text-property (point) 'w-id))
             (entry (get-text-property (point) 'w-entry))
             (location (point)))
    (setq w-retrieving "Deleting...")
    (request (format "%s/api/entries/%d" w-host id)
      :parser 'json-read
      :type "DELETE"
      :params `(("access_token" . ,w-token))
      :headers '(("Content-Type" . "application/json") ("Prefer" . "return=minimal"))
      :status-code `((401 . ,(w--retry-with-token #'w-search-delete-entry))
                     (404 . ,(cl-function
                              (lambda (&key _data &allow-other-keys)
                                (w-search--remove-from-listing id location)))))
      :success
      (cl-function
       (lambda (&key _data &allow-other-keys)
         (w-search--remove-from-listing id location)))
      :error #'w--debug)))

(defun w-search--remove-from-listing (id location)
  (setq w-retrieving nil)
  (with-current-buffer (w-search-buffer)
    (let ((inhibit-read-only t))
      (w-db-delete id)
      (save-excursion
        (goto-char location)
        (delete-line)))))


;;; Search buffer major mode

(defvar-keymap w-search-mode-map
  :doc "Keymap for `wombag-search-mode'."
  :suppress t
  "<RET>" #'w-search-show-entry
  "<" #'beginning-of-buffer
  ">" #'w-search--eob
  "G" #'w-sync
  "B" #'w-search-eww-open
  "&" #'w-search-browse-url
  "x" #'w-search-browse-url
  "s" #'w-search-live-filter
  "q" #'w-search-quit-window
  "g" #'w-search-update--force
  "R" #'w-add-entry
  "D" #'w-search-delete-entry
  "n" #'next-line
  "p" #'previous-line
  "w" #'w-search-copy
  "+" #'w-search-add-tags
  "-" #'w-search-remove-tags
  "A" #'w-search-archive-entry
  "F" #'w-search-starred-entry)

(define-derived-mode w-search-mode fundamental-mode "wombag-search"
  "Major mode for listing wombag entries.
\\{wombag-search-mode-map}"
  (setq truncate-lines t
        buffer-read-only t
        header-line-format '(:eval (w-search-header)))
  (buffer-disable-undo)
  (w-search-update :force)
  (goto-char (point-min))
  (hl-line-mode 1)
  (add-hook 'minibuffer-setup-hook 'w-search--minibuffer-setup)
  (setq-local bookmark-make-record-function
              #'w-search-bookmark-make-record))

(provide 'wombag-search)
;;; wombag-search.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("w-" . "wombag-"))
;; End:
