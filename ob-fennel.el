;;; ob-fennel.el --- Babel Functions for Fennel -*- lexical-binding: t; -*-

;; Author: Andrey Listopadov
;; Homepage: https://gitlab.com/andreyorst/ob-fennel
;; Package-Requires: ((emacs "26.1"))
;; Keywords: outlines, literate programming, reproducible research
;; Prefix: ob-fennel
;; Version: 0.0.2

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with isayt.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for evaluating fennel code

;; Requirements:
;;
;; - fennel (at least 1.0.0)
;; - fennel-mode, particularly fennel-scratch module

;;; Code:
(require 'ob)
(require 'inf-lisp)

(defvar fennel-mode-repl-prompt-regexp)
(defvar fennel-repl--buffer)
(defvar fennel-program)
(declare-function fennel-scratch--eval-to-string "ext:fennel-scratch" (sexp))
(declare-function fennel-repl--start "ext:fennel-mode" (&optional ask-for-command?))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("fennel" . "fnl"))

(defvar org-babel-default-header-args:fennel'())

;; TODO?
;; (defun org-babel-expand-body:fennel (body params)
;;   "Expand BODY according to PARAMS, return the expanded body."
;;   )

(defun ob-fennel--check-fennel-proc (buffer)
  "Check if BUFFER has a running `inferior-lisp-proc'."
  (let ((inferior-lisp-buffer buffer))
    (condition-case nil (inferior-lisp-proc)
      (error nil))))

(defun ob-fennel--initialize-repl (name params)
  "Create a Fennel REPL buffer with given NAME according to PARAMS."
  (let* ((fennel-program (or (cdr (assq :fennel-cmd params))
                             fennel-program))
         (buffer (fennel-repl--start nil)))
    (with-current-buffer buffer
      (rename-buffer name))
    buffer))

(defun ob-fennel--get-create-repl-buffer (params)
  "Get or create Fennel REPL buffer according to PARAMS.

Can return symbol `uninitialized' in case there was no REPL buffer."
  (let ((session-name (cdr (assq :session params)))
        (fmt "*Fennel REPL:%s*")
        (uninitiolized-err "Please reevaluate when Fennel REPL is connected"))
    (cond ((and (string= "none" session-name)
                (ob-fennel--check-fennel-proc fennel-repl--buffer))
           (get-buffer fennel-repl--buffer))
          ((string= "none" session-name)
           (ob-fennel--initialize-repl fennel-repl--buffer params)
           (user-error uninitiolized-err))
          ((ob-fennel--check-fennel-proc (format fmt session-name))
           (get-buffer (format fmt session-name)))
          (t (ob-fennel--initialize-repl (format fmt session-name) params)
             (user-error uninitiolized-err)))))

(defun ob-fennel--send-to-repl (repl-buffer body)
  "Send BODY to the `inferior-lisp-proc' and retrieve the result."
  (let ((inferior-lisp-buffer repl-buffer))
    (condition-case nil (inferior-lisp-proc)
      (error
       (user-error "%S buffer doesn't have an active process"
                   inferior-lisp-buffer)))
    (string-trim
     (replace-regexp-in-string
      "^[[:space:]]+" ""
      (replace-regexp-in-string
       fennel-mode-repl-prompt-regexp ""
       (fennel-scratch--eval-to-string
        body))))))

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
are isolated, and repl-local variables with same names can
co-exist in different sessions, since they're different
processes."
  (condition-case nil (require 'fennel-scratch)
    (error (user-error "`fennel-scratch' is unavailable")))
  (let* ((repl-buffer (ob-fennel--get-create-repl-buffer params))
         (eval-result (ob-fennel--send-to-repl repl-buffer body)))
    (org-babel-result-cond (cdr (assq :result-params params))
      eval-result
      (condition-case nil (org-babel-script-escape eval-result)
        (error eval-result)))))

(provide 'ob-fennel)
;;; ob-fennel.el ends here
