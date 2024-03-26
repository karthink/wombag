;;; wombag-db.el --- Wombag database interaction     -*- lexical-binding: t; -*-

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

;; Wombag database interaction

;;; Code:
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'map)
(require 'wombag-options)

(defconst w-db-version 2)
(defvar w-db-connection nil)
(defvar w-search-columns)

(defvar w-db-open-func
  (or (and (fboundp 'sqlite-available-p)
           (sqlite-available-p)
           (require 'emacsql-sqlite-builtin nil t)
           (functionp 'emacsql-sqlite-builtin)
           #'emacsql-sqlite-builtin)
      (progn (require 'emacsql-sqlite)
             #'emacsql-sqlite))
  ;; :type 'function ;; :group 'w-db
  "Function for creating the database connection.")

(defun w-db-ensure ()
  ";TODO:"
  (unless (and w-db-connection (emacsql-live-p w-db-connection))
    (let ((dir (file-name-directory w-db-file)))
      (or (file-directory-p dir) (make-directory dir)))
    (setq w-db-connection (funcall w-db-open-func w-db-file))
    
    (emacsql w-db-connection
             [:create-table
              :if-not-exists
              items ([tag is_archived is_starred
                      user_name user_email user_id tags
                      is_public (id integer :primary-key) uid title
                      url hashed_url origin_url given_url hashed_given_url
                      (archived_at DATE) content (created_at DATE)
                      (updated_at DATE) (published_at DATE) published_by
                      (starred_at DATE) annotations mimetype language reading_time
                      domain_name preview_picture http_status headers _links])])
    
    (emacsql w-db-connection [:create-table :if-not-exists version ([user-version])])
    ;; Note: fetch-new is not currently used for anything
    (emacsql w-db-connection [:create-table :if-not-exists last_update ([ (fetch-new INTEGER) (fetch-all INTEGER) ])])
    (unless (emacsql w-db-connection [:select * :from last_update])
      (let ((epoch 1))
        (emacsql w-db-connection `[:insert :into last_update :values ([,epoch ,epoch])])))
    (unless (emacsql w-db-connection [:select user-version :from version])
      (emacsql w-db-connection `[:insert :into version :values ([,w-db-version])])))

  w-db-connection)

(defun w-db-update-date (date &optional only-new)
  "Update the database with DATE (seconds since epoch) as the last_update time.

With optional arg ONLY-NEW, update the \\='fetch-new\\=' field:
Currently unused."
  (cl-assert (or (floatp date) (integerp date)))
  (setq date (floor date))
  (emacsql (w-db-ensure) `[:update last_update
                           :set (= ,(if only-new 'fetch-new 'fetch-all) ,date)]))

;; (w-db-update-date (time-to-seconds (time-subtract (current-time) (days-to-time 1))))

(defun w-db--sanitize-entry (entry)
  "Transform ENTRY before adding it to the database."
  (let ((alltags
         (mapconcat (lambda (el) (map-elt el 'label))
                    (map-elt entry 'tags) ",")))
    ;; Use 0 and 1 instead of :json-false and t for is_public
    (when-let ((public (map-elt entry 'is_public)))
      (map-put! entry 'is_public (if (eq public t) 1 0)))
    (map-put! entry 'headers nil)
    ;; ;; Remove timezone info if not in the form "+03:30" or "Z"
    ;; (dolist (datetype '(created_at updated_at published_at starred_at archived_at))
    ;;   (when-let* ((date (map-elt entry datetype))
    ;;               (_ (string-match-p "\\+[[:digit:]]\\{4\\}$" date)))
    ;;     (map-put! entry datetype (substring date 0 -5))))
    ;; Add all tags concatenated with a comma
    (cons `(tag . ,alltags) entry)))

(defun w-db-query (query &rest args)
  "Run QUERY with ARGS against the Wombag database."
  (if (stringp query)
      (emacsql (w-db-ensure) (apply #'format query args))
    (apply #'emacsql (w-db-ensure) query args)))

(defconst w-db-schema
  '(tag is_archived is_starred user_name user_email user_id tags is_public
    id uid title url hashed_url origin_url given_url hashed_given_url
    archived_at content created_at updated_at published_at published_by
    starred_at annotations mimetype language reading_time domain_name
    preview_picture http_status headers _links))

(defun w-db-get-entries (query &optional columns)
  "Select COLUMNS from the Wombag DB for QUERY.

If COLUMNS is nil select the full record.

Return a list of alists of the column names and data."
  (setq columns (or (ensure-list columns)
                    w-db-schema))
  (mapcar (lambda (sel) (cl-pairlis columns sel))
          (w-db-query query)))

(defun w-db-get-ids (ids)
  (cl-typecase ids
   (number
    (w-db-get-entries
     `[:select ,(vconcat w-search-columns) :from items :where (= id ,ids)]
     w-search-columns))
   (vector
    (w-db-get-entries
     `[:select ,(vconcat w-search-columns) :from items :where (in id ,(vconcat ids))]))
   (list
    (w-db-get-entries
     `[:select ,(vconcat w-search-columns) :from items :where (in id ,(vconcat ids))]))))

(defun w-db--close ()
  (emacsql-close (w-db-ensure)))

(defun w-db-insert (entries &optional replace)
  "Return a list of the ids of ENTRIES inserted into the database.

If REPLACE is non-nil, replace the entry in place instead."
  (let* ((ids '())                      ;byte-compiler complains otherwise
         (entries
          (cl-loop for entry across entries
                   collect (map-elt entry 'id) into ids
                   collect (apply #'vector
                                  (mapcar 'cdr (w-db--sanitize-entry entry))))))
    (w-db-query
     (if replace
         `[:insert-or-replace :into items :values ,entries]
       `[:insert-or-ignore :into items :values ,entries]))
    ids))

(defun w-db-delete (ids)
  (cond ((vectorp ids)
         (w-db-query `[:delete :from items :where (in id ,ids)]) )
        ((numberp ids) (w-db-query `[:delete :from items :where (= id ,ids)]))
        (t nil)))

(defun w-db-update (field id new)
  ";TODO: Update FIELD with ID in database with NEW data.

FIELD is a symbol, ID the primary key integer and NEW is any data."
  (w-db-query `[:update items :set (= ,field ,new) :where (= id ,id)]))

(provide 'wombag-db)
;;; wombag-db.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("w-" . "wombag-"))
;; End:
