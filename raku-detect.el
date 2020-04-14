;;; raku-detect.el --- Raku detection -*- lexical-binding: t; -*-

;;; Commentary:

;; Yes, we are adding to `magic-mode-alist' here. Raku uses the same
;; file extensions as Perl 5, and we want the mode to work out of the box.
;; So for those files we look at the first line of code to determine
;; whether to call `raku-mode' instead of `perl-mode'.

;;; Code:

;;;###autoload
(add-to-list 'interpreter-mode-alist '("perl6" . raku-mode))
(add-to-list 'interpreter-mode-alist '("raku" . raku-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.p[lm]?6\\'" . raku-mode))
(add-to-list 'auto-mode-alist '("\\.nqp\\'"     . raku-mode))
(add-to-list 'auto-mode-alist '("\\.raku\\'"    . raku-mode))
(add-to-list 'auto-mode-alist '("\\.rakumod\\'" . raku-mode))
(add-to-list 'auto-mode-alist '("\\.rakutest\\'" . raku-mode))

;;;###autoload
(defconst raku-magic-pattern
  (rx line-start
      (0+ space)
      (or (and "use" (1+ space) "v6")
          (and (opt (and (or "my" "our") (1+ space)))
               (or "module" "class" "role" "grammar" "enum" "slang" "subset")))))

;;;###autoload
(defun raku-magic-matcher ()
  "Check if the current buffer is a Raku file.

Only looks at a buffer if it has a file extension of .t, .pl, or .pm.

Scans the buffer (to a maximum of 4096 chars) for the first non-comment,
non-whitespace line.  Returns t if that line looks like Raku code,
nil otherwise."
  (let ((case-fold-search nil))
    (when (and (stringp buffer-file-name)
               (string-match "\\.\\(?:t\\|p[lm]\\)\\'" buffer-file-name))
      (let ((keep-going t)
            (found-raku nil)
            (max-pos (min 4096 (point-max))))
        (while (and (< (point) max-pos)
                    keep-going)
          (if (looking-at "^ *\\(?:#.*\\)?$")
              (beginning-of-line 2)
            (setq keep-going nil
                  found-raku (looking-at raku-magic-pattern))))
        found-raku))))

;;;###autoload
(add-to-list 'magic-mode-alist '(raku-magic-matcher . raku-mode))

(provide 'raku-detect)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; raku-detect.el ends here
