(require 'term)

(defun perl6-repl-send-line (astring)
  ;;send home if we have a regular perl6 prompt this line
  ;;otherwise, simulate "C-b" until we get to the beginning
  (if (equal (buffer-substring (- (line-beginning-position) 2) (line-beginning-position)) "> ") (term-send-home)
    (progn (while (> (- (point) (line-beginning-position)) 2) (term-send-string "\^b"))))
  (term-send-raw-string astring) ;send the argument as perl6 input
  (term-send-raw-string "\^M") ;sends a return to execute astring
  ;;(term-send-raw-string "\^y") ;sends a C-y, which yanks the killed line
  )

(setq perl6-repl--buffer-name "Raku REPL")

(defun perl6-repl--buffer-name-earmuf ()
  (concat "*" perl6-repl--buffer-name "*"))

(defcustom perl6-exec-path "raku"
  "Raku executable path."
  :type 'string
  :group 'perl6)

(defun perl6-exec-path-exists-p ()
  (or (file-executable-p perl6-exec-path)
      (executable-find perl6-exec-path)))

(defun perl6-repl-other-window ()
  "Runs Perl6 in a `term' buffer in another window."
  (interactive)
  (let ((termbuf (apply 'make-term perl6-repl--buffer-name perl6-exec-path nil)))
    (set-buffer termbuf)
    (term-mode)
    (term-set-escape-char 24) ;this sets the escape char to C-x instead of C-c
    (term-char-mode)
    (switch-to-buffer-other-window termbuf)))


(defun perl6-repl-ready-p ()
  (or (< (point) 2)
      (not (equal (buffer-substring (- (point)2)
                                    (point))
                  "> "))))

(defun perl6-create-new-repl ()
  (progn (perl6-repl-other-window)
         (while (perl6-repl-ready-p)
           (sit-for 0.1))))

(defun perl6-send-line-to-repl (&optional line)
  (interactive)

  (if (perl6-exec-path-exists-p)
      (let ((jbuf (get-buffer (perl6-repl--buffer-name-earmuf)))
            (cbuf (current-buffer))
            (cwin (selected-window))
            (pos (point))
            (linecontents
             (progn (when line ;if a line is passed to the function, go there
                      (goto-char (point-min))
                      (forward-line (- line 1)))
                    (buffer-substring (line-beginning-position) (line-end-position))))
            ) ;save pos of start of next line
        (if jbuf (switch-to-buffer jbuf)
          ;;if there is not a perl6 REPl open, open it and wait for prompt

          (perl6-create-new-repl))
        (perl6-repl-send-line linecontents)
        (select-window cwin)
        (switch-to-buffer cbuf)
        (goto-char pos))
    (message "Cannot execute %s" perl6-exec-path)))

(defun perl6-send-region-to-repl (begin end)
  (interactive (if (use-region-p)
                   (list (region-beginning)
                         (region-end))
                 (list nil nil)))
  (if (and begin end (perl6-exec-path-exists-p))
      (let ((jbuf (get-buffer (perl6-repl--buffer-name-earmuf)))
            (cbuf (current-buffer))
            (cwin (selected-window))
            (pos (mark))
            (contents (buffer-substring-no-properties begin end)))
        (if jbuf (switch-to-buffer jbuf)
          ;;if there is not a perl6 REPl open, open it and wait for prompt
          (perl6-create-new-repl))
        (mapc 'perl6-repl-send-line (split-string contents "\n+"))
        (select-window cwin)
        (switch-to-buffer cbuf))
    (message "Cannot execute %s" perl6-exec-path)))

(defun perl6-send-buffer-to-repl ()
  (interactive)
  (if (perl6-exec-path-exists-p)
      (let ((jbuf (get-buffer (perl6-repl--buffer-name-earmuf)))
            (cbuf (current-buffer))
            (cwin (selected-window))
            (contents (buffer-string)))
        (if jbuf (switch-to-buffer jbuf)
          (perl6-create-new-repl))
        ;; Send te line to the repl
        (set-text-properties 0 (length contents) nil contents)
        (mapc 'perl6-repl-send-line (split-string contents "\n+"))
        (select-window cwin)
        (switch-to-buffer cbuf))
    (message "Cannot execute %s" perl6-exec-path)))

(provide 'perl6-repl)
