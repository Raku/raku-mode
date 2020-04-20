;;; raku-repl -- Repl for support Raku

;;; Commentary:
;; Basic repl support for Raku

;;; Code:
(require 'comint)

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
