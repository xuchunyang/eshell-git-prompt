;;; eshell-git-prompt.el --- An informative and fancy Eshell prompt for Git users  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/eshell-git-prompt
;; Package-Requires: ((emacs "24.1") (dash "2.11.0"))
;; Keywords: eshell git
;; Version: 0.1
;; Created: 09/11/2015

;; This program is free software; you can redistribute it and/or modify
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
;;
;; Usage:
;; You can call `eshell-git-prompt-use-theme' to pick up a theme then launch
;; Eshell.
;;
;; Add the following to your initialization file to let Eshell to use it every
;; time:
;;   (eshell-git-prompt-use-theme 'robbyrussell)
;;
;; TODO
;; - [ ] Acquire more meta data like [[https://github.com/michaeldfallen/git-radar][michaeldfallen/git-radar]]
;; - [ ] Tweak faces
;; - [ ] Add one or two themes
;; - [ ] How to add more extra themes?
;; - [ ] Make accessing basic meta data more easier, for example, shorten
;;       function name
;;
;; Note
;; 1 You must kill all Eshell buffers and re-enter Eshell to make your new
;;   prompt take effect.
;; 2 You must set `eshell-prompt-regexp' to FULLY match your Eshell prompt,
;;   sometimes it is impossible, especially when you don't want use any special
;;   characters (e.g., '$') to state the end of your Eshell prompt
;; 3 If you set `eshell-prompt-function' or `eshell-prompt-regexp' incorrectly,
;;   Eshell may crashes, if it happens, you can M-x `eshell-git-prompt-use-theme'
;;   to revert to the default Eshell prompt, then read Note #1.

;;; Code:

(require 'cl-lib)
(require 'dash)

;;; * Customization


;;; * Internal

(cl-defun eshell-git-prompt--git-root-dir
    (&optional (directory default-directory))
  "Return Git root directory name if exist, otherwise, return nil."
  (let ((root (locate-dominating-file directory ".git")))
    (and root (file-name-as-directory root))))

(cl-defun eshell-git-prompt--shorten-directory-name
    (&optional (directory default-directory))
  "Return only current directory name. DIRECTORY must end with slash.

For example:

  \"~/foo/bar\" => \"bar\"
  \"~\" => \"~\"
  \"/\" => \"/\""
  (let ((dir (abbreviate-file-name directory)))
    (if (> (length dir) 1)
        (file-name-nondirectory (substring dir 0 -1))
      dir)))

(defconst eshell-git-prompt---git-global-arguments
  '("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true")
  "Global git arguments.")

(defun eshell-git-prompt--process-git-arguments (args)
  "Prepare ARGS for a function that invokes Git."
  (setq args (-flatten args))
  (append eshell-git-prompt---git-global-arguments args))

(defun eshell-git-prompt--git-insert (&rest args)
  "Execute Git with ARGS, inserting its output at point."
  (setq args (eshell-git-prompt--process-git-arguments args))
  (apply #'process-file "git" nil (list t nil) nil args))

(defun eshell-git-prompt--git-string (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string."
  (with-temp-buffer
    (apply #'eshell-git-prompt--git-insert args)
    (unless (bobp)
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun eshell-git-prompt--git-lines (&rest args)
  "Execute Git with ARGS, returning its output as a list of lines.
Empty lines anywhere in the output are omitted."
  (with-temp-buffer
    (apply #'eshell-git-prompt--git-insert args)
    (split-string (buffer-string) "\n" t)))

(defun eshell-git-prompt--collect-status ()
  "Return working directory status as a plist.
If working directory is clean, return nil."
  (let ((untracked 0)                   ; next git-add, then git-commit
        (modified 0)                    ; next git-commit
        (modified-updated 0)            ; next git-commit
        (new-added 0)                   ; next git-commit
        (deleted 0)                     ; next git-rm, then git-commit
        (deleted-updated 0)             ; next git-commit
        (renamed-updated 0)             ; next git-commit
        )
    (-when-let (status-items (eshell-git-prompt--git-lines "status" "--porcelain"))
      (--each status-items
        (pcase (substring it 0 2)
          ("??" (cl-incf untracked))
          ("MM" (progn (cl-incf modified)
                       (cl-incf modified-updated)))
          ("A " (cl-incf new-added))
          (" D" (cl-incf deleted))
          ("D " (cl-incf deleted-updated))
          ("R " (cl-incf renamed-updated))))
      (list :untracked untracked
            :modified modified
            :modified-updated  modified-updated
            :new-added new-added
            :deleted deleted
            :deleted-updated deleted-updated
            :renamed-updated renamed-updated))))

(defun eshell-git-prompt--current-branch ()
  (eshell-git-prompt--git-string "symbolic-ref" "HEAD" "--short"))

(defun eshell-git-prompt-last-command-status ()
  "Return Eshell last command execution status.
When Eshell just launches, `eshell-last-command-status' is not defined yet,
return 0 (i.e., success)."
  (if (not (boundp 'eshell-last-command-status))
      0
    eshell-last-command-status))


;;; * Themes

;; oh-my-zsh's robbyrussell theme
;; see https://github.com/robbyrussell/oh-my-zsh/wiki/Themes#robbyrussell
(defun eshell-git-prompt-robbyrussell-func ()
  "Eshell Git prompt.

It should be set as value of `eshell-prompt-function', at the same time,
`eshell-prompt-regexp' also MUST to be set to match the return of
`eshell-prompt-function'."
  ;; FIXME: This is too ugly.
  (concat
   (propertize "➜" 'face
               `(:foreground
                 ,(if (zerop (eshell-git-prompt-last-command-status))
                      "green" "red")))
   " "
   (propertize (eshell-git-prompt--shorten-directory-name)
               'face '(:foreground "cyan"))
   ;; Yo, we are in a Git repo, display some information about it
   (when (eshell-git-prompt--git-root-dir)
     (concat
      " "
      (propertize "git:(" 'face '(:foreground "blue"))
      (propertize (eshell-git-prompt--current-branch)
                  'face '(:foreground "red"))
      (propertize ")" 'face '(:foreground "blue"))
      (when (eshell-git-prompt--collect-status)
        (concat
         " "
         (propertize "✗" 'face '(:foreground "yellow"))))))
   ;; To make it possible to let `eshell-prompt-regexp' to match the full prompt
   (propertize "$" 'invisible t) " "))

(defvar eshell-git-prompt-robbyrussell-regexp "^[^$\n]*\\\$ ")

(require 'em-prompt)

(defvar eshell-git-prompt-themes
  `((robbyrussell
     eshell-git-prompt-robbyrussell-func
     eshell-git-prompt-robbyrussell-regexp)
    (default
        ,(symbol-value 'eshell-prompt-function)
        ,eshell-prompt-regexp))
  "All available themes.")

;;;###autoload
(defun eshell-git-prompt-use-theme (theme)
  (interactive
   (let ((theme
          (completing-read "Use theme:"
                           (--map (symbol-name (car it))
                                  eshell-git-prompt-themes)
                           nil t)))
     (list (intern theme))))
  (when (stringp theme)
    (setq theme (intern theme)))
  (-if-let (func-regexp (assoc-default theme eshell-git-prompt-themes))
      (setq eshell-prompt-function (car func-regexp)
            eshell-prompt-regexp (cdr func-regexp))
    (user-error "Theme \"%s\" is not available." theme)))

(provide 'eshell-git-prompt)

;; Local Variables:
;; lisp-indent-function: common-lisp-indent-function
;; End:

;;; eshell-git-prompt.el ends here
