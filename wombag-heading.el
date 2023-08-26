;;; wombag-heading.el --- Wombag heaading navigation  -*- lexical-binding: t; -*-

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

;; Wombag heading navigation support

;;; Code:
(require 'shr)
(require 'imenu)

(eval-and-compile
  (if (require 'shr-heading nil t)
      (progn
        (defalias 'w-heading-next 'shr-heading-next)
        (defalias 'w-heading--line-at-point 'shr-heading--line-at-point)
        (defalias 'w-heading-previous 'shr-heading-previous)
        (defalias 'w-heading-setup-imenu 'shr-heading-setup-imenu))
    (defun w-heading-next (&optional arg)
      "Move forward by ARG headings (any h1-h5).
If ARG is negative move backwards, ARG defaults to 1."
      (interactive "p")
      (unless arg (setq arg 1))
      (catch 'return
        (dotimes (_ (abs arg))
          (when (> arg 0) (end-of-line))
          (if-let ((match
                    (funcall (if (> arg 0)
                                 #'text-property-search-forward
                               #'text-property-search-backward)
                             'face '(shr-h1 shr-h2 shr-h3 shr-h4)
                             (lambda (tags face)
                               (cl-loop for x in (if (consp face) face (list face))
                                        thereis (memq x tags))))))
              (goto-char
               (if (> arg 0) (prop-match-beginning match) (prop-match-end match)))
            (throw 'return nil))
          (when (< arg 0) (beginning-of-line)))
        (beginning-of-line)
        (point)))

    (defun w-heading-previous (&optional arg)
      "Move backward by ARG headings (any h1-h5).
If ARG is negative move forwards instead, ARG defaults to 1."
      (interactive "p")
      (w-heading-next (- (or arg 1))))

    (defun w-heading--line-at-point ()
      "Return the current line."
      (buffer-substring (line-beginning-position) (line-end-position)))

    (defun w-heading-setup-imenu ()
      "Setup imenu for h1-h5 headings in eww buffer.
Add this function to appropriate major mode hooks such as
`eww-mode-hook' or `elfeed-show-mode-hook'."
      (setq-local
       imenu-prev-index-position-function #'w-heading-previous
       imenu-extract-index-name-function  #'w-heading--line-at-point))))

(provide 'wombag-heading)
;;; wombag-heading.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("w-" . "wombag-"))
;; End:
