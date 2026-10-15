;;; helm-term-toggle.el --- Helm source for term-toggle buffers  -*- lexical-binding: t; -*-

;; Filename: helm-term-toggle.el
;; Description: Helm source listing every live term-toggle buffer
;; Author: Arthur <arthur.miller@live.com>
;; Version: 1.0.0
;; URL: https://github.com/amno1/emacs-term-toggle
;; Keywords: helm term toggle shell
;; Package-Requires: ((emacs "25.1") (helm "3.0") (term-toggle "2.2.0"))
;; Compatibility: (Test on GNU Emacs 28.0.50).

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; Helm source for `term-toggle' (see term-toggle.el).  Provides
;; `helm-term-toggle', which lists every live term-toggle buffer and
;; toggles the chosen one.
;;
;; Toggle buffers are discovered by scanning `buffer-list' for a
;; non-nil `tt--terminal-key', the same mechanism used by
;; `term-toggle-cycle'.  No additional bookkeeping is required.

;;; Code:

(require 'helm)
(require 'seq)
(require 'term-toggle)

;;; Internal functions

;; (defun tt--helm-toggle (buffer)
;;   "Toggle term-toggle buffer BUFFER from a helm session.

;; Helm's minibuffer owns the selection while a session is active,
;; so the window helm was started from is re-selected first; without
;; this, `tt--toggle' would split the minibuffer window instead of
;; the window that was current when `helm-term-toggle' was called."
;;   (let ((window (get-buffer-window helm-current-buffer)))
;;     (when window
;;       (select-window window)))
;;   (tt--toggle buffer))

(defun tt--helm-candidates ()
  "Return alist of (DISPLAY . BUFFER) for live term-toggle buffers.

Sorted by buffer name so the order is stable across invocations."
  (mapcar
   (lambda (buf)
     (with-current-buffer buf
       (cons (format "%-20s  %-8s  %s"
                     (buffer-name)
                     (cdr tt--terminal-key)
                     (abbreviate-file-name (car tt--terminal-key)))
             buf)))
   (sort (seq-filter
          (lambda (buf)
            (and (buffer-live-p buf)
                 (with-current-buffer buf tt--terminal-key)))
          (buffer-list))
         (lambda (a b)
           (string< (buffer-name a) (buffer-name b))))))

(defun tt--helm-at-current-window (fn buf)
  "Call FN with BUF after reselecting helm's originating window.

During a helm session the minibuffer owns the selection, so FN
would otherwise operate on the wrong window.  `helm-current-buffer'
is the buffer helm was started from."
  (when-let* ((win (get-buffer-window helm-current-buffer)))
    (select-window win))
  (funcall fn buf))

(defun tt--helm-toggle (buf)
  "Toggle term-toggle buffer BUF from a helm session."
  (tt--helm-at-current-window #'tt--toggle buf))

(defun tt--helm-replace (buf)
  "Make BUF the only visible term-toggle, from a helm session."
  (tt--helm-at-current-window #'tt--replace buf))

;;; Helm source
(defvar helm-source-term-toggle
  (helm-build-sync-source "Term Toggles"
    :candidates #'tt--helm-candidates
    :action '(("Toggle" . tt--helm-toggle)
              ("Replace current" . tt--helm-replace)
              ("Switch to buffer" . pop-to-buffer)
              ("Kill buffer" . kill-buffer))
    :requires-pattern 0
    :volatile t)
  "Helm source listing every live term-toggle buffer.")

;;;###autoload
(defun helm-term-toggle ()
  "Choose a term-toggle buffer with helm and toggle it."
  (interactive)
  (helm :sources helm-source-term-toggle
        :buffer "*helm term-toggle*"))

(defvar helm-source-term-toggle-replace
  (helm-build-sync-source "Term Toggles (replace)"
    :candidates #'tt--helm-candidates
    :action #'tt--helm-replace
    :requires-pattern 0
    :volatile t))

;;;###autoload
(defun helm-term-toggle-replace ()
  "Choose a term-toggle buffer with helm and replace the current one.

Unlike `helm-term-toggle', the chosen toggle becomes the only
visible toggle: any other term-toggle window is hidden first."
  (interactive)
  (helm :sources helm-source-term-toggle-replace
        :buffer "*helm term-toggle*"))

;;;###autoload
(defvar helm-term-toggle-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'helm-term-toggle)
    (define-key map (kbd "T") #'helm-term-toggle-replace)
    map))

(provide 'helm-term-toggle)
;;; helm-term-toggle.el ends here
