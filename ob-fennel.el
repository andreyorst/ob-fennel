;;; ob-fennel.el --- Babel Functions for Fennel -*- lexical-binding: t; -*-

;; Author: Andrey Listopadov
;; Homepage: https://gitlab.com/andreyorst/ob-fennel
;; Package-Requires: ((emacs "26.1"))
;; Keywords: outlines, literate programming, reproducible research
;; Prefix: ob-fennel
;; Version: 0.0.7

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ob-fennel.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for evaluating fennel code

;; Requirements:
;;
;; - fennel (at least 1.0.0)
;; - fennel-mode, particularly fennel-scratch module

;;; Code:

(require 'ob)
(require 'inf-lisp)
(require 'ansi-color)

(defvar fennel-mode-repl-prompt-regexp)
(defvar fennel-program)
(declare-function fennel-scratch--eval-to-string "ext:fennel-scratch" (sexp))
(declare-function fennel-repl-mode "ext:fennel-mode" ())

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("fennel" . "fnl"))

(defcustom ob-fennel-collapse-code 'oneline
  "How to collapse code bofore sending it to the process.

- 'strings - collapse strings with literal newlines into
  one-line strings with `\\n'.
- 'comments - remove comments, and lines completely consisting
  out of comments.
- 'both - collapse strings and remove comments.
- 'oneline - collapse strings, remove commens, and remove any
  newlines from the code."
  :group 'ob-fennel
  :type '(choice
	  (const :tag "collapse strings" strings)
	  (const :tag "remove comments" comments)
	  (const :tag "collapse strings and remove comments" both)
          (const :tag "collapse to one line" oneline)))

(defvar org-babel-default-header-args:fennel '())

(defvar ob-fennel--hline-to "(setmetatable [] {:__fennelview #:hline})"
  "Replace hlines in incoming tables with this when translating to Fennel.")

(defun ob-fennel--check-fennel-proc (buffer)
  "Check if BUFFER has a running `inferior-lisp-proc'."
  (let ((inferior-lisp-buffer buffer))
    (condition-case nil (inferior-lisp-proc)
      (error nil))))

(defun ob-fennel--initialize-repl (name params)
  "Create a Fennel REPL buffer with given NAME according to PARAMS."
  (let* ((cmd (or (cdr (assq :fennel-cmd params))
                  fennel-program))
         (cmdlist (when (fboundp 'split-string-shell-command)
                    (split-string-shell-command cmd)
                    (split-string cmd)))
         (buffer (apply #'make-comint-in-buffer name name
                        (car cmdlist) nil (cdr cmdlist))))
    (with-current-buffer buffer
      (fennel-repl-mode)
      (setq-local fennel-program cmd)
      (setq inferior-lisp-buffer name))
    buffer))

(defun ob-fennel--get-create-repl-buffer (session params)
  "Get or create Fennel REPL buffer for SESSION according to PARAMS.

Raises a `user-error' in case there was no REPL buffer."
  (let ((fmt "*Fennel REPL:%s*")
        (uninitiolized-err "Please re-evaluate when Fennel REPL is initialized"))
    (cond ((and (or (string= "none" session)
                    (null session))
                (ob-fennel--check-fennel-proc (format fmt "default")))
           (get-buffer (format fmt "default")))
          ((or (string= "none" session)
               (null session))
           (ob-fennel--initialize-repl (format fmt "default") params)
           (user-error uninitiolized-err))
          ((ob-fennel--check-fennel-proc (format fmt session))
           (get-buffer (format fmt session)))
          (t (ob-fennel--initialize-repl (format fmt session) params)
             (user-error uninitiolized-err)))))

(defun ob-fennel--send-to-repl (repl-buffer body)
  "Send BODY to the REPL-BUFFER and retrieve the result."
  (unless (ob-fennel--check-fennel-proc repl-buffer)
    (user-error "%S buffer doesn't have an active process"
                repl-buffer))
  (let ((inferior-lisp-buffer repl-buffer))
    (string-trim
     (replace-regexp-in-string
      "^[[:space:]]+" ""
      (replace-regexp-in-string
       fennel-mode-repl-prompt-regexp ""
       (fennel-scratch--eval-to-string
        body))))))

(defun ob-fennel-var-to-fennel (var)
  "Convert an elisp value to a fennel variable.
Convert an elisp value, VAR, into a string of fennel source code
specifying a variable of the same value."
  (cond ((listp var)
         (concat "[" (mapconcat #'ob-fennel-var-to-fennel var " ") "]"))
        ((eq var 'hline)
         ob-fennel--hline-to)
        (t (format
            (if (stringp var) "%S" "%s")
            (if (stringp var) (substring-no-properties var) var)))))

(defun ob-fennel-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or tuple, then convert them into an
Emacs-lisp table, otherwise return the result as a string."
  (org-babel-script-escape results))

(defun org-babel-variable-assignments:fennel (params)
  "Return a list of Fennel let bindings assigning the block's PARAMS."
  (mapcar
   (lambda (pair)
     (format "%s %s"
	     (car pair)
	     (ob-fennel-var-to-fennel (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-expand-body:fennel (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel-variable-assignments:fennel params))
         (body
          (with-temp-buffer
            (insert (if (null vars) body
                      (format "(let [%s]\n %s)" (string-join vars "\n") body)))
            (when (memq ob-fennel-collapse-code '(comments oneline both))
              ;; remove comments
              (goto-char (point-min))
              (while (re-search-forward ";" nil 'noerror)
                (unless (or (nth 3 (syntax-ppss)) (looking-at-p "\""))
                  (delete-region (1- (point)) (progn (end-of-line) (point)))
                  (beginning-of-line)
                  (when (looking-at-p "[[:blank:]]*$")
                    (delete-line)))))
            (when (memq ob-fennel-collapse-code '(strings oneline both))
              ;; collapse strings
              (goto-char (point-min))
              (while (re-search-forward "\"" nil 'noerror)
                (when (nth 3 (syntax-ppss))
                  (let ((start (1- (point))))
                    (when (re-search-forward "\"" nil 'noerror)
                      (replace-regexp-in-region "\n" "\\\\n" start (point)))))))
            (when (eq ob-fennel-collapse-code 'oneline)
              (delete-indentation nil (point-min) (point-max)))
            (buffer-substring-no-properties (point-min) (point-max)))))
    (concat (string-trim body) "\n")))

(defun org-babel-execute:fennel (body params)
  "Evaluate a block of Fennel code with Babel.

Sends BODY to the `fennel-repl' process according to PARAMS.
Supports `:session' parameter, and automatically creates a new
REPL buffer, named after the session.  Includes an additional
parameter, exclusive to Fennel src blocks `:fennel-cmd' used to
specify how to start the REPL process.

For example:

#+begin_src fennel :session foo :fennel-cmd \"fennel --lua luajit --repl\"
\(+ 1 2 3)
#+end_src

automatically creates buffer \"foo\", starts Fennel REPL in it,
by using \"fennel --lua luajit --repl\" as a command.  Sessions
are isolated, and repl-local variables with the same names can
co-exist in different sessions, since they're different
processes."
  (condition-case nil (require 'fennel-scratch)
    (error (user-error "`fennel-scratch' is unavailable")))
  (let* ((repl-buffer (ob-fennel--get-create-repl-buffer (cdr (assq :session params)) params))
         (body (org-babel-expand-body:fennel body params))
         (eval-result (ansi-color-apply (ob-fennel--send-to-repl repl-buffer body))))
    (org-babel-result-cond (cdr (assq :result-params params))
      eval-result
      (ob-fennel-table-or-string (org-trim eval-result)))))

(provide 'ob-fennel)
;;; ob-fennel.el ends here
