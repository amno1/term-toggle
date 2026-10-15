;;; term-toggle.el --- Toggle a dedicated terminal per directory  -*- lexical-binding: t; -*-

;; Filename: term-toggle.el
;; Description: Toggle a dedicated terminal per directory
;; Author: Joseph <jixiuf@gmail.com>, Yatao <yatao.li@live.com>, Arthur <arthur.miller@live.com>
;; Created: 2011-03-02
;; Version: 3.2.0
;; URL: https://github.com/amno1/emacs-term-toggle
;; Keywords: term toggle shell slime
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
;; `term-toggle' pops up a terminal-like buffer at the bottom of the
;; selected window.  Each directory gets its own dedicated terminal:
;; the terminal buffer records which directory (and shell) it serves,
;; and is found again by scanning `buffer-list'.  No global registry
;; is needed -- killing a terminal removes it from consideration
;; automatically.
;;
;; `term-toggle' launches `ansi-term' by default.  Customize
;; `term-toggle-default-shell' to change the default, or call one of
;; the dedicated commands `term-toggle-ansi', `term-toggle-term',
;; `term-toggle-shell', `term-toggle-eshell', `term-toggle-ielm'
;; or `term-toggle-vterm' or `term-toggle-ghostel'.
;;
;; `term-toggle-slime' toggles a SLIME REPL backed by SBCL (or the
;; Lisp in `term-toggle-slime-lisp').  SLIME maintains a single REPL
;; buffer per connection, so this command is directory-independent
;; and starts SLIME on first use.

;;; Code:

(require 'seq)

;;; Customizable Options:
(defgroup term-toggle nil
  "Quake style console toggle in current working directory.
Support toggle for shell, term, ansi-term, eshell, ielm and slime."
  :prefix "term-toggle-"
  :group 'applications)

(defcustom term-toggle-default-shell 'ansi-term
  "Default shell used by `term-toggle'.
Must be one of `ansi-term', `term', `shell', `eshell', `ielm' or
`vterm'.  The `vterm' choice requires the optional vterm package
to be installed."
  :type '(choice (const :tag "ansi-term" ansi-term)
                 (const :tag "term" term)
                 (const :tag "shell" shell)
                 (const :tag "eshell" eshell)
                 (const :tag "ielm" ielm)
                 (const :tag "vterm" vterm)
                 (const :tag "ghostel" ghostel))
  :group 'term-toggle)

(defcustom term-toggle-scope 'directory
  "Scope of a term-toggle terminal.

`file'       One toggle per visited file (falls back to
             `directory' for buffers not visiting a file).

`directory'  One toggle per directory (the default).  Each
             subdirectory gets its own terminal.

`project'    One toggle per project, as determined by
             `project-current'.  Falls back to `directory' when
             the buffer is not in a project.

`global'     One toggle per shell, shared across every buffer.
             The shell starts in whichever directory was current
             when the toggle was first opened."
  :type '(choice (const :tag "Per file" file)
                 (const :tag "Per directory" directory)
                 (const :tag "Per project"   project)
                 (const :tag "Global"        global))
  :group 'term-toggle)

;; (defcustom term-toggle-slime-lisp "sbcl"
;;   "Lisp executable used by `term-toggle-slime'.
;; Only consulted when starting a new SLIME connection."
;;   :type 'string
;;   :group 'term-toggle)

(defcustom term-toggle-confirm-exit nil
  "Ask to confirm exit if there is a running bash process in terminal."
  :type 'boolean
  :group 'term-toggle)

(defcustom term-toggle-kill-buffer-on-process-exit t
  "Kill buffer when shell process has exited."
  :type 'boolean
  :group 'term-toggle)

(defcustom term-toggle-minimum-split-height 10
  "The minimum height of a splittable window."
  :type 'fixnum
  :group 'term-toggle)

(defcustom term-toggle-default-height 15
  "The default height of a splitted window."
  :type 'fixnum
  :group 'term-toggle)


(defcustom term-toggle-default-width 80
  "Default width of the pop-up when the split is horizontal."
  :type 'fixnum
  :group 'term-toggle)

(defcustom term-toggle-split-side 'below
  "Where the term-toggle pop-up appears.

`below' is the default Quake-style layout: the pop-up takes the
bottom of the frame, the current window the top.  `above', `left'
and `right' place it on the corresponding side.  The names match
the SIDE argument of `split-window'."
  :type '(choice (const :tag "Above" above)
                 (const :tag "Below" below)                 
                 (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'term-toggle)


;;; Internals

(declare-function vterm "vterm" (&optional buffer-name))
(declare-function ghostel "ghostel" ())
(defvar ghostel-buffer-name)

(defun tt--vterm-available-p ()
  "Return non-nil if the vterm package is available."
  (or (fboundp 'vterm)
      (require 'vterm nil t)))

(defun tt--ghostel-available-p ()
  "Return non-nil if the ghostel package is available."
  (or (fboundp 'ghostel)
      (require 'ghostel nil t)))

(defvar-local tt--terminal-scope nil
  "Value of `term-toggle-scope' when this toggle was created.
Used by the scope-aware cycle commands to keep the ring inside a
single scope.")

(defvar-local tt--terminal-key nil
  "Identify a term-toggle terminal buffer.
When non-nil, a cons of the form (DIR . SHELL) describing which
directory and shell this buffer serves.  Because this variable is
buffer-local, a killed terminal simply disappears from
`buffer-list' without any explicit cleanup.")

(defun tt--directory ()
  "Return the current buffer's directory, expanded.
Uses `dired-current-directory' in Dired buffers."
  (expand-file-name
   (if (derived-mode-p 'dired-mode)
       (dired-current-directory)
     default-directory)))

(defun tt--project-root ()
  "Return the current project's root, or nil if not in a project."
  (when-let* ((proj (project-current)))
    (expand-file-name
     (if (fboundp 'project-root)
         (project-root proj)
       (car (project-root proj))))))

(defun tt--file-key ()
  "Return a key identifying the current file, or nil.
Return the file name for a file-visiting buffer, or the
directory for a Dired buffer."
  (cond
   (buffer-file-name (expand-file-name buffer-file-name))
   ((derived-mode-p 'dired-mode) (tt--directory))))

(defun tt--key ()
  "Return the scope key for the current buffer's toggle.

The key is what identifies a toggle: two buffers with the same
key and shell share a terminal.  Under `directory' scope it is
the current directory; under `project' scope it is the project
root; under `global' scope it is the constant string \"global\"."
  (pcase term-toggle-scope
    ('file (or (tt--file-key) (tt--directory)))
    ('global "global")
    ('project (or (tt--project-root) (tt--directory)))
    (_ (tt--directory))))

(defun tt--shell-dir (key)
  "Return the directory a shell identified by KEY should start in.

Under `file' scope KEY is a file path, so the shell starts in the
file's directory.  Under `global' scope KEY is not a path at all,
so the current directory is used instead.  Otherwise KEY is a
directory."
  (cond
   ((equal key "global") (tt--directory))
   ((file-directory-p key) key)
   ((string-match-p "/" key) (file-name-directory key))
   (t (tt--directory))))

(defun tt--setup-process (buffer)
  "Set up process flags and sentinel for terminal BUFFER."
  (let ((proc (get-buffer-process buffer)))
    (when proc
      (set-process-query-on-exit-flag proc term-toggle-confirm-exit)
      (when term-toggle-kill-buffer-on-process-exit
        (set-process-sentinel
         proc (lambda (_proc evt)
                (when (string-match-p "\\(?:exited\\|finished\\)" evt)
                  (when (buffer-live-p buffer)
                    (kill-buffer buffer)))))))))

(defun tt--make-buffer-name (shell dir)
  "Return a candidate buffer name for a SHELL terminal rooted at DIR."
  (format "tt-*%s*<%s>"
          (if (memq shell '(term ansi-term)) "terminal" (symbol-name shell))
          (file-name-nondirectory (directory-file-name dir))))

(defun tt--start (shell key)
  "Start a SHELL terminal identified by KEY and return its buffer."
  (let* ((default-directory (tt--shell-dir key))
         (name (generate-new-buffer-name (tt--make-buffer-name shell key)))
         (shell-program (or (getenv "SHELL") "/bin/sh"))
         buffer)
    (save-window-excursion
      (cond
       ((memq shell '(term ansi-term))
        (funcall shell shell-program name))
       ((eq shell 'vterm)
        (unless (tt--vterm-available-p)
          (user-error "vterm is not installed; cannot start a vterm terminal"))
        (condition-case nil
            (vterm name)
          (wrong-number-of-arguments
           (vterm)
           (rename-buffer name t))))
       ((eq shell 'ghostel)
        (unless (tt--ghostel-available-p)
          (user-error "ghostel is not installed; cannot start a ghostel terminal"))
        ;; Ghostel takes no buffer-name argument; it reads
        ;; `ghostel-buffer-name' instead.  Bind it so the buffer gets
        ;; the name term-toggle expects.
        (let ((ghostel-buffer-name name))
          (ghostel)))
       ((eq shell 'shell)
        (shell name))
       ((eq shell 'eshell)
        (eshell))
       ((eq shell 'ielm)
        (ielm))
       (t
        (user-error "Unsupported shell: %S" shell)))
      (setq buffer (current-buffer)))
    (with-current-buffer buffer
      (setq default-directory (tt--shell-dir key))
      (unless (equal (buffer-name) name)
        (rename-buffer name t))
      (setq-local tt--terminal-key (cons key shell))
      (setq-local tt--terminal-scope term-toggle-scope)
      (term-toggle-minor-mode 1))
    (tt--setup-process buffer)
    buffer))

(defun tt--find-buffer (shell key)
  "Return an existing term-toggle terminal buffer for SHELL in KEY."
  (seq-find
   (lambda (buf)
     (and (buffer-live-p buf)
          (with-current-buffer buf
            (equal tt--terminal-key (cons key shell)))))
   (buffer-list)))

(defun tt--toggle (buffer)
  "Toggle a pop-up window showing terminal BUFFER.

The pop-up is placed according to `term-toggle-split-side'.  The
window is resized before being marked dedicated, so that
`shrink-window' is not refused by the dedicated flag.  The shrink
runs inside `with-selected-window' so it targets the new window,
and the new window is re-selected at the end, because
`with-selected-window' restores the previous selection on exit."
  (let ((window (get-buffer-window buffer)))
    (if window
        (progn
          (set-window-dedicated-p window nil)
          (bury-buffer buffer)
          (delete-window window))
      (let* ((side term-toggle-split-side)
             (horizontal (memq side '(left right)))
             (new-window (split-window nil nil side)))
        (set-window-buffer new-window buffer)
        (if horizontal
            (let ((delta (- (window-width new-window)
                            term-toggle-default-width)))
              (when (> delta 0)
                (with-selected-window new-window
                  (shrink-window delta t))))
          (let ((delta (- (window-height new-window)
                          term-toggle-default-height)))
            (when (> delta 0)
              (with-selected-window new-window
                (shrink-window delta)))))
        (set-window-dedicated-p new-window t)
        (select-window new-window)))))

(defun tt--replace (buffer)
  "Make BUFFER the only visible term-toggle window.

Every other visible term-toggle is hidden first, then BUFFER is
shown if it is not already displayed.  When BUFFER is already
visible, it is simply re-selected."
  (dolist (win (window-list))
    (let ((wbuf (window-buffer win)))
      (when (and (not (eq wbuf buffer))
                 (buffer-local-value 'tt--terminal-key wbuf))
        (tt--toggle wbuf))))
  (if-let* ((win (get-buffer-window buffer)))
      (select-window win)
    (tt--toggle buffer)))

(defun tt--live-toggles ()
  "Return all live term-toggle buffers, sorted by buffer name."
  (sort (seq-filter
         (lambda (buf)
           (and (buffer-live-p buf)
                (with-current-buffer buf tt--terminal-key)))
         (buffer-list))
        (lambda (a b)
          (string< (buffer-name a) (buffer-name b)))))

(defun tt--scope-key (scope)
  "Return the toggle key for the current context under SCOPE.

Inside a toggle that was itself created under SCOPE, return that
toggle's own key so the ring stays anchored to it; otherwise
compute the key the way `tt--key' would for SCOPE."
  (if (and tt--terminal-key
           (eq tt--terminal-scope scope))
      (car tt--terminal-key)
    (let ((term-toggle-scope scope))
      (tt--key))))

(defun tt--scope-toggles (scope)
  "Return live toggles created under SCOPE for the current context."
  (let ((key (tt--scope-key scope)))
    (seq-filter
     (lambda (buf)
       (with-current-buffer buf
         (and (eq tt--terminal-scope scope)
              (equal (car tt--terminal-key) key))))
     (tt--live-toggles))))

;;; Minor mode

(defvar term-toggle-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f1>")    #'term-toggle-close)
    (define-key map (kbd "<f10>")   #'term-toggle-cycle)
    (define-key map (kbd "<S-f10>") #'term-toggle-cycle-backward)
    map)
  "Keymap for `term-toggle-minor-mode'.")

(define-minor-mode term-toggle-minor-mode
  "Minor mode for term-toggle terminal buffers.

Provides two convenience bindings:

  \\[term-toggle-close]  Hide the current term-toggle buffer.
  \\[term-toggle-cycle]  Cycle through the ring of term-toggle buffers."
  :lighter " TT"
  :keymap term-toggle-minor-mode-map
  :group 'term-toggle)

(defun term-toggle-close (&optional replace)
  "Hide the current term-toggle buffer.

With a prefix argument REPLACE, hide any other visible
term-toggle first, so the current toggle becomes the only one on
screen.  If no other toggle is visible, this is a no-op beyond
re-selecting the current toggle's window."
  (interactive "P")
  (unless tt--terminal-key
    (user-error "Not a term-toggle buffer"))
  (if replace
      (tt--replace (current-buffer))
    (tt--toggle (current-buffer))))

(defun tt--get-buffer (shell key &optional new)
  "Return the terminal buffer for SHELL in KEY.

When NEW is non-nil, always start a fresh terminal instead of
reusing the one already associated with SHELL and KEY."
  (or (and (not new) (tt--find-buffer shell key))
      (tt--start shell key)))

(defun tt--cycle-buffers (buffers &optional backward)
  "Cycle through BUFFERS, hiding the currently visible one first.

BUFFERS is expected to be sorted by buffer name.  With BACKWARD,
step to the previous buffer instead of the next."
  (unless buffers
    (user-error "No matching term-toggle buffers"))
  (let* ((n (length buffers))
         (current (seq-find #'get-buffer-window buffers))
         (next (cond
                ((null current)
                 (if backward (car (last buffers)) (car buffers)))
                (backward
                 (nth (mod (1- (seq-position buffers current)) n) buffers))
                (t
                 (nth (mod (1+ (seq-position buffers current)) n) buffers)))))
    (when current (tt--toggle current))
    (unless (eq current next) (tt--toggle next))))


;;; Commands

;;;###autoload
(defun term-toggle-cycle (&optional backward)
  "Cycle through all live term-toggle buffers.

With no argument, show the next toggle in alphabetical order.  With
a non-nil BACKWARD argument, show the previous one instead."
  (interactive "P")
  (tt--cycle-buffers (tt--live-toggles) backward))

;;;###autoload
(defun term-toggle-cycle-backward ()
  "Cycle through term-toggle buffers in reverse order."
  (interactive)
  (term-toggle-cycle t))

;;;###autoload
(defun term-toggle-cycle-file (&optional backward)
  "Cycle through toggles for the current file.

Only toggles created under `file' scope for the same file are
considered.  With a prefix argument, cycle backwards."
  (interactive "P")
  (tt--cycle-buffers (tt--scope-toggles 'file) backward))

;;;###autoload
(defun term-toggle-cycle-directory (&optional backward)
  "Cycle through toggles for the current directory.

Only toggles created under `directory' scope for the same
directory are considered.  With a prefix argument, cycle backwards."
  (interactive "P")
  (tt--cycle-buffers (tt--scope-toggles 'directory) backward))

;;;###autoload
(defun term-toggle-cycle-project (&optional backward)
  "Cycle through toggles for the current project.

Only toggles created under `project' scope for the same project
root are considered.  With a prefix argument, cycle backwards."
  (interactive "P")
  (tt--cycle-buffers (tt--scope-toggles 'project) backward))

;;;###autoload
(defun term-toggle-cycle-global (&optional backward)
  "Cycle through every toggle created under `global' scope.

With a prefix argument, cycle backwards."
  (interactive "P")
  (tt--cycle-buffers (tt--scope-toggles 'global) backward))

(defun term-toggle (&optional shell arg)
  "Toggle a terminal for the current scope.

The scope is determined by `term-toggle-scope': one toggle per
directory, per project, or shared globally.  With no argument,
use `term-toggle-default-shell'.

Interactively, a prefix argument asks for a brand-new terminal
instead of reusing the one already associated with this scope."
  (interactive "p")
  (when (integerp shell)          ; prefix arrives in SHELL
    (setq arg shell shell nil))
  (let* ((shell (or shell term-toggle-default-shell))
         (key (tt--key)))
    (tt--toggle (tt--get-buffer shell key arg))))

;;;###autoload
(defun term-toggle-term    (&optional arg)
  (interactive "P") (term-toggle 'term arg))
;;;###autoload
(defun term-toggle-aterm    (&optional arg)
  (interactive "P") (term-toggle 'ansi-term arg))
;;;###autoload
(defun term-toggle-shell   (&optional arg)
  (interactive "P") (term-toggle 'shell arg))
;;;###autoload
(defun term-toggle-eshell  (&optional arg)
  (interactive "P") (term-toggle 'eshell arg))
;;;###autoload
(defun term-toggle-ielm    (&optional arg)
  (interactive "P") (term-toggle 'ielm arg))
;;;###autoload
(defun term-toggle-vterm   (&optional arg)
  (interactive "P") (term-toggle 'vterm arg))
;;;###autoload
(defun term-toggle-ghostel (&optional arg)
  (interactive "P") (term-toggle 'ghostel arg))

(global-set-key (kbd "C-c t nf") #'term-toggle-cycle-file)
(global-set-key (kbd "C-c t nd") #'term-toggle-cycle-directory)
(global-set-key (kbd "C-c t np") #'term-toggle-cycle-project)
(global-set-key (kbd "C-c t ng") #'term-toggle-cycle-global)
(global-set-key (kbd "C-c t d") #'term-toggle)        ; default shell
(global-set-key (kbd "C-c t a") #'term-toggle-ansi)
(global-set-key (kbd "C-c t t") #'term-toggle-term)
(global-set-key (kbd "C-c t s") #'term-toggle-shell)
(global-set-key (kbd "C-c t e") #'term-toggle-eshell)
(global-set-key (kbd "C-c t i") #'term-toggle-ielm)
(global-set-key (kbd "C-c t v") #'term-toggle-vterm)
(global-set-key (kbd "C-c t g") #'term-toggle-ghostel)

(provide 'term-toggle)
;;; term-toggle.el ends here
