;;; raku-repl -- Repl for support Raku

;;; Commentary:
;; Basic repl support for Raku

;;; Code:
(require 'comint)
(require 'raku-font-lock)

(defcustom raku-exec-path "raku"
  "Raku executable path."
  :type 'string
  :group 'raku)

(defcustom raku-exec-arguments ""
  "Raku command line arguments."
  :type 'string
  :group 'raku)

(defvar raku-prompt-regexp "^> "
  "Prompt for `run-raku'.")

(defvar raku-buffer-name "Raku REPL"
  "Buffer name for `run-raku.")

(define-derived-mode raku-repl-mode comint-mode "Raku"
  "Major mode for `run-raku'."
  ;; Set up the prompt and make it read only.
  (setq-local comint-prompt-regexp raku-prompt-regexp)
  (setq-local comint-prompt-readonly t)
  ;; See raku-mode.el.
  (setq-local syntax-propertize-function #'raku-syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions #'syntax-propertize-multiline nil 'local)
  (setq-local font-lock-syntactic-face-function #'raku-font-lock-syntactic-face)
  (setq-local font-lock-defaults '(raku-font-lock-keywords nil nil))
  (add-hook 'raku-mode-hook 'imenu-add-menubar-index)
  (setq imenu-generic-expression raku-imenu-generic-expression
      imenu-case-fold-search nil)
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  ;; Don't jump beyond the prompt with M-{ or M-}.
  (setq-local paragraph-start raku-prompt-regexp))

(defun run-raku ()
  "Run an inferior instance of `raku' inside Emacs."
  (interactive)
  (let* ((raku-program raku-exec-path)
         (check-proc (comint-check-proc raku-buffer-name))
         (buffer (apply 'make-comint-in-buffer
                        raku-buffer-name
                        check-proc
                        raku-exec-path
                        '()
                        (split-string raku-exec-arguments))))
    (with-current-buffer buffer
      (raku-repl-mode))
    (display-buffer buffer)))

(defun raku-comint-get-process ()
  "Raku process name."
  (get-process raku-buffer-name))

(defun raku-send-string-to-repl (str)
  "Send STR to the repl."
  (comint-send-string (raku-comint-get-process)
                      (concat str "\n")))

(defun raku-send-line-to-repl ()
  "Send a line to the repl."
  (interactive)
  (run-raku)
  (let ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (raku-send-string-to-repl str)))

(defun raku-send-region-to-repl ()
  "Send a region to the repl."
  (interactive)
  (run-raku)
  (if (region-active-p)
      (let ((buf (buffer-substring-no-properties (region-beginning)
                                                 (region-end))))
        (raku-send-string-to-repl buf))
    (message "No region selected")))

(defun raku-send-buffer-to-repl ()
  "Send a buffer to the repl."
  (interactive)
  (run-raku)
  (let ((buf (buffer-substring-no-properties (point-min)
                                             (point-max))))
    (raku-send-string-to-repl buf)))

(provide 'raku-repl)
;;; raku-repl.el ends here
