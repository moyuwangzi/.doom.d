;;; org-expose-link-markers.el --- Show hidden org link markers at point -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name <your-email@example.com>

;; Author: Your Name <your-email@example.com>
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "26.1") (org "9.0"))
;; Version: 0.3

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Show hidden link markers ([[ ]]) when cursor is on a link in Org mode.
;; Works only when `org-descriptive-links' is t.
;;
;; Usage:
;;   (add-hook 'org-mode-hook #'org-expose-link-markers-mode)

;;; Code:

(require 'org)

(defvar-local org-expose-link-markers--current-bounds nil
  "Bounds of the last exposed link area.")

(defun org-expose-link-markers--expose-function ()
  "Expose hidden link markers ([[ ]]) at point."
  (when (and org-descriptive-links (not buffer-read-only))
    (let ((bounds (and (eq (get-text-property (point) 'face) 'org-link)
                       (org-find-text-property-region (point) 'face))))
      ;; Reset previous exposed area if changed
      (when (and org-expose-link-markers--current-bounds
                 (not (equal org-expose-link-markers--current-bounds bounds)))
        (font-lock-flush (car org-expose-link-markers--current-bounds)
                         (cdr org-expose-link-markers--current-bounds))
        (font-lock-ensure (car org-expose-link-markers--current-bounds)
                          (cdr org-expose-link-markers--current-bounds)))
      ;; Expose current link
      (when bounds
        (with-silent-modifications
          (remove-text-properties (max (1- (car bounds)) (point-min))
                                  (min (1+ (cdr bounds)) (point-max))
                                  '(invisible t))))
      (setq org-expose-link-markers--current-bounds bounds))))

;;;###autoload
(define-minor-mode org-expose-link-markers-mode
  "Minor mode to auto-expose hidden link markers in Org mode."
  :init-value nil
  (unless (derived-mode-p 'org-mode)
    (user-error "This mode only works in Org mode"))
  (if org-expose-link-markers-mode
      (if org-descriptive-links
          (add-hook 'post-command-hook #'org-expose-link-markers--expose-function -90 t)
        (org-expose-link-markers-mode -1)
        (message "This mode requires `org-descriptive-links' to be t"))
    (remove-hook 'post-command-hook #'org-expose-link-markers--expose-function t)
    (when org-expose-link-markers--current-bounds
      (font-lock-flush (car org-expose-link-markers--current-bounds)
                       (cdr org-expose-link-markers--current-bounds))
      (font-lock-ensure (car org-expose-link-markers--current-bounds)
                        (cdr org-expose-link-markers--current-bounds))
      (setq org-expose-link-markers--current-bounds nil))))

(provide 'org-expose-link-markers)

;;; org-expose-link-markers.el ends here

