;;; hook.el --- Utilities for generating hooks.
;;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Provide utilities for generating hooks.
;;;
;;; Code:

(defun hook--configure-post-command ()
  "Initialize a `post-command-hook' for the current `major-mode'."
  (let* ((mm-post-command-name (intern (concat (symbol-name major-mode) "-post-command-hook")))
         (mm-post-command-hook (when (boundp mm-post-command-name) (symbol-value mm-post-command-name)))
         (runner1 `(lambda () (run-hooks ',mm-post-command-name)))
         (runner2 `(lambda () (remove-hook 'post-command-hook ,runner1 t))))
    (add-hook 'post-command-hook runner1 nil t)
    (add-hook 'change-major-mode-hook runner2 nil t)))

(add-hook 'after-change-major-mode-hook #'hook--configure-post-command)

(provide 'hook)
;;; hook.el ends here
