;;; wombag-options.el --- Customization options for Wombag  -*- lexical-binding: t; -*-

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

;; Customization options for Wombag.

;;; Code:
(require 'shr)


;;; Groups
(defgroup wombag nil
  "Wombag client group."
  :group 'comm)

(defgroup w-db nil
  "Wombag database group."
  :group 'wombag)

(defgroup w-search nil
  "Wombag search buffer group."
  :group 'wombag)

(defgroup w-show nil
  "Wombag search buffer group."
  :group 'wombag)

(defgroup w-faces nil
  "Wombag faces group."
  :group 'wombag)

(define-obsolete-variable-alias
  'w-clientid 'w-client-id "0.1.0")
(define-obsolete-variable-alias
  'w-secret 'w-client-secret "0.1.0")


;;; Remote and authentication
(defcustom w-host ""
  "Wombag host."
  :type 'string
  :group 'wombag)

(defcustom w-username nil
  "User name for Wombag."
  :type 'string
  :group 'wombag)

(defcustom w-password ""
  "Password for Wombag"
  :type '(choice
          (string :tag "Password")
          (function :tag "Function that returns the password."))
  :group 'wombag)

(defcustom w-client-id ""
  "Client ID for Wombag."
  :type 'string
  :group 'wombag)

(defcustom w-client-secret ""
  "Client secret for Wombag."
  :type 'string
  :group 'wombag)


;;; Local database
(defcustom w-dir (file-name-concat
                  (or (getenv "XDG_CACHE_HOME") user-emacs-directory)
                  "wombag")
  "Wombag data directory."
  :type 'directory
  :set (lambda (sym val)
         (condition-case nil
             (progn
               (unless (file-directory-p val)
                 (make-directory val :parents))
               (set-default-toplevel-value sym val))
           (error (user-error "Could not create directory: %s" val))))
  :group 'w-db)

(defcustom w-db-file
  (file-name-concat w-dir "wombag.sqlite")
  "Sqlite database used to store Wombag data."
  :type 'file
  :group 'w-db)


;;; Wombag search
(defcustom w-search-buffer-name "*wombag-search*"
  "Buffer name for the Wombag search buffer."
  :type 'string
  :group 'w-search)

(defcustom w-search-filter "#30 "
  "Default search filter for Wombag."
  :type 'string
  :group 'w-search)

(defcustom w-search-filter-help nil
  "When non-nil, pop up a help window showing Wombag's search filter syntax.

This window can be toggled manually using \\`?'."
  :type 'boolean
  :group 'w-search)


;;; Wombag show
(defcustom w-show-entry-switch #'pop-to-buffer-same-window
  "Function used to display Wombag entry window."
  :type 'function
  :group 'w-show)

(defcustom w-browse-url-function #'browse-url
  "Function called with URLs to open."
  :type 'function
  :group 'w-show)

(defcustom w-pre-html-render-hook nil
  "Hook run before rendering Wombag articles."
  :type 'hook
  :group 'w-show)

(defcustom w-post-html-render-hook nil
  "Hook run after rendering Wombag articles."
  :type 'hook
  :group 'w-show)

(defcustom w-show-buffer-name "*wombag-entry*"
  "Buffer name for the Wombag search buffer."
  :type 'string
  :group 'w-show)


;;; Faces
(defface w-title-face '((t :inherit bold))
  "Face used for title on compact view."
  :group 'w-faces)

(defface w-tag-face
  '((((class color) (background light))
     :foreground "brown")
    (((class color) (background dark))
     :foreground "#EBCB8B")
    (t :inherit default))
  "Face used for tag."
  :group 'w-faces)

(defface w-domain-face
  '((((class color) (background light))
     :foreground "#3B6EA8")
    (((class color) (background dark))
     :foreground "#d9c6d6")
    (t :inherit default))
  "Face used for author."
  :group 'w-faces)

(defface w-reading-time-face
  '((((class color) (background light))
     :foreground "#8b94a5")
    (((class color) (background dark))
     :foreground "#6f7787")
    (t :inherit default))
  "Face used for size."
  :group 'w-faces)

(defface w-date-face
  '((((class color) (background light))
     :foreground "#29838D")
    (((class color) (background dark))
     :foreground "#8FBCBB")
    (t :inherit default))
  "Face for the date (last_modified)."
  :group 'w-faces)

(defface w-show-title-face
  '((t (:inherit (shr-h1 variable-pitch))))
  "Face used for title."
  :group 'w-faces)

(defface w-archive-face
  '((((class color) (background light))
     :foreground "grey"
     :weight light)
    (((class color) (background dark))
     :foreground "dim grey"
     :weight light)
    (t :inherit default))
  "Face used for archive."
  :group 'w-faces)

(defface w-starred-face
  '((((class color) (background light))
     :foreground "red3")
    (((class color) (background dark))
     :foreground "yellow")
    (t :inherit default))
  "Face used for title."
  :group 'w-faces)

(provide 'wombag-options)
;;; wombag-options.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("w-" . "wombag-"))
;; End:
