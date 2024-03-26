;;; wombag-show.el --- Wombag article interface  -*- lexical-binding: t; -*-

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

;; Wombag article interface

;;; Code:
(require 'cl-lib)
(require 'shr)
(require 'browse-url)
(require 'bookmark)
(require 'wombag-options)
(require 'wombag-heading)
(require 'wombag-db)

(declare-function w-search-buffer "wombag-search")

(bookmark-maybe-load-default-file)

(defun w-show-buffer ()
  "Return the Wombag entry buffer."
  (get-buffer "*wombag-entry*"))

(defvar-local w-show-entry nil
  "The Wombag entry displayed in this buffer.")


;;; Bookmarks
;;;###autoload
(defun w-show-bookmark-handler (record)
  "Show the bookmarked entry saved in the `RECORD'."
  (let* ((id (bookmark-prop-get record 'id))
         (entry (car (w-db-get-ids id)))
         (position (bookmark-get-position record)))
    (set-buffer (w-show-entry entry))
    (goto-char position)))

(defun w-show-bookmark-make-record ()
  "Save the current position and the entry into a bookmark."
  (let ((id (alist-get 'id w-show-entry))
        (position (point))
        (title (alist-get 'title w-show-entry)))
    `(,(format "(Wombag) \"%s\"" title)
      (id . ,id)
      ;; (location . ,title)
      (position . ,position)
      (handler . w-show-bookmark-handler))))


;;; Saving entry positions
(defvar w-show--positions-file
  (file-name-concat w-dir "positions.eld"))

(defvar w-show--positions-table
  (if (file-exists-p w-show--positions-file)
      (condition-case nil
          (let ((coding-system-for-read 'utf-8))
            (with-temp-buffer
              (insert-file-contents w-show--positions-file)
              (goto-char (point-min))
              (read (current-buffer))))
        (error (message "Could not sync Wombag positions, starting over.")
               (make-hash-table :size 1024)))
    (make-hash-table :size 1024)))

(defun w-show--positions-save ()
  (when-let ((id (alist-get 'id w-show-entry)))
    (if (or (bobp) (eobp))
        (remhash id w-show--positions-table)
      (puthash id (point) w-show--positions-table))))

(defun w-show--positions-write ()
  (when (and (boundp 'w-show--positions-table)
             (hash-table-p w-show--positions-table)
             (not (hash-table-empty-p w-show--positions-table)))
    (let ((write-region-inhibit-fsync t)
          (coding-system-for-write 'utf-8)
          (print-level nil)
          (print-length nil))
      (with-temp-file w-show--positions-file
        (insert ";;; -*- lisp-data -*-\n"
                (prin1-to-string w-show--positions-table))))))

(add-hook 'kill-emacs-hook #'w-show--positions-write)


;;; Rendering the entry buffer
(defun w-show-render-html (begin end)
  "Render HTML in current buffer with shr.

Render from positions BEGIN to END."
  (run-hooks 'w-pre-html-render-hook)
  (shr-render-region begin end)
  (run-hooks 'w-post-html-render-hook))

(defun w-show-entry (entry)
  "Read Wombag ENTRY at point."
  (let ((buf (get-buffer-create w-show-buffer-name))
        (title (alist-get 'title entry "(UNTITLED)"))
        (reading-time (alist-get 'reading_time entry))
        (created-at (alist-get 'created_at entry))
        ;; (tag (alist-get 'tag entry))
        (domain-name (or (alist-get 'domain_name entry) ""))
        (id (alist-get 'id entry))
        (url (alist-get 'url entry))
        (beg) (end))
    (with-current-buffer buf
      (delay-mode-hooks
        (w-show-mode)
        (funcall w-show-entry-switch buf)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize title 'face 'w-show-title-face)
                  "\n")
          (insert (format "%s  %s  %s\n\n\n"
                          (propertize (format "%d min" reading-time)
                                      'face 'w-reading-time-face)
                          (propertize (substring created-at 0 10)
                                      'face 'w-date-face)
                          ;; No DATA or HELP-ECHO args, the latter is not supported on Emacs 28.2
                          (propertize (button-buttonize
                                       domain-name (lambda (_) (browse-url url)))
                                      'face 'w-domain-face)))
          (if-let ((content (car
                             (car-safe
                              (w-db-query `[:select content :from items :where (= id ,id)])))))
              (progn (setq beg (point))
                     (insert content)
                     (setq end (point))
                     (w-show-render-html beg end))
            (insert (propertize "(empty)" 'face 'warning)))
          (if-let ((pos (gethash id w-show--positions-table)))
              (prog1 (goto-char pos)
                ;; (recenter next-screen-context-lines)
                (recenter)
                )
            (goto-char (point-min)))))
      (setq w-show-entry entry)
      (run-mode-hooks)
      (current-buffer))))

;;; Actions in the entry buffer
(defun w-show-quit-window ()
  "Close this Wombag article."
  (interactive)
  (quit-window 'kill)
  (when-let* ((buf (w-search-buffer))
              (win (get-buffer-window buf)))
    (if (window-live-p win)
        (select-window win))))

(defun w-show-disable-images ()
  "Disable images in this Wombag buffer."
  (interactive)
  (when-let ((entry w-show-entry)
             (shr-inhibit-images t))
    (w-show-entry entry)))


;;; Major mode
(defvar-keymap w-show-mode-map
  :doc "Keymap for `wombag-show-mode'."
  "TAB"       #'shr-next-link
  "<backtab>" #'shr-previous-link
  "SPC"       #'scroll-up-command
  "S-SPC"     #'scroll-down-command
  "DEL"       #'scroll-down-command
  "<"         #'beginning-of-buffer
  ">"         #'end-of-buffer
  "q"         #'w-show-quit-window
  "I"         #'w-show-disable-images
  "C-c C-n"   #'w-heading-next
  "C-c C-p"   #'w-heading-previous)

(define-derived-mode w-show-mode fundamental-mode
  "wombag-entry"
  "Mode for displaying wombag entry details.
\\{wombag-show-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq-local bookmark-make-record-function
       #'w-show-bookmark-make-record)
  (w-heading-setup-imenu)
  (add-hook 'kill-buffer-hook
            #'w-show--positions-save nil 'local))

(provide 'wombag-show)
;;; wombag-show.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("w-" . "wombag-"))
;; End:
