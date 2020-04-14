;;; perl6-repl -- Repl for support Raku

;;; Commentary:
;; Basic repl support for Raku

;;; Code:
(require 'comint)

(defcustom perl6-exec-path "raku"
  "Raku executable path."
  :type 'string
  :group 'perl6)

(defcustom perl6-exec-arguments ""
  "Raku command line arguments."
  :type 'string
  :group 'perl6)

(defvar perl6-prompt-regexp "^> "
  "Prompt for `run-perl6'.")

(defvar perl6-buffer-name "Raku REPL"
  "Buffer name for `run-perl6.")

(defun run-perl6 ()
  "Run an inferior instance of `raku' inside Emacs."
  (interactive)
  (let* ((perl6-program perl6-exec-path)
         (check-proc (comint-check-proc perl6-buffer-name))
         (buffer (apply 'make-comint-in-buffer
                        perl6-buffer-name
                        check-proc
                        perl6-exec-path
                        '()
                        (split-string perl6-exec-arguments))))
    (display-buffer buffer)))

(defun perl6-comint-get-process ()
  "Raku process name."
  (get-process perl6-buffer-name))

(defun perl6-send-string-to-repl (str)
  "Send STR to the repl."
  (comint-send-string (perl6-comint-get-process)
                      (concat str "\n")))

(defun perl6-send-line-to-repl ()
  "Send a line to the repl."
  (interactive)
  (run-perl6)
  (let ((str (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (perl6-send-string-to-repl str)))

(defun perl6-send-region-to-repl ()
  "Send a region to the repl."
  (interactive)
  (run-perl6)
  (if (region-active-p)
      (let ((buf (buffer-substring-no-properties (region-beginning)
                                                 (region-end))))
        (perl6-send-string-to-repl buf))
    (message "No region selected")))

(defun perl6-send-buffer-to-repl ()
  "Send a buffer to the repl."
  (interactive)
  (run-perl6)
  (let ((buf (buffer-substring-no-properties (point-min)
                                             (point-max))))
    (perl6-send-string-to-repl buf)))

(provide 'perl6-repl)
;;; perl6-repl.el ends here
