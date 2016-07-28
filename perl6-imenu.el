;;; perl6-imenu.el --- Imenu support Perl 6 -*- lexical-binding: t; -*-

;;; Commentary:


;; Imenu functions and variables are defined here.
(defvar perl6-imenu-generic-expression
  '(
    ;; the names are in reverse desired order since they are evaluated here last first
    ("Variables" "^\\s-*\\(?:my\\|our\\)\\s-+\\(\\(?:\\$\\|@\\|%\\)\\(?:[_[:alnum:]]+\\)\\)" 1)
    ("Subs/Methods" "^\\s-*\\(?:my\\|our\\)\\s-+\\(?:sub\\|method\\|multi\\|multi\\s-+sub\\|multi\\s-+method:\\)\\s-+\\(.+)\\)" 1)
    )
  "Define interesting points in the Perl 6 buffer for `imenu'.

This is used to set `imenu-generic-expression' when Perl 6 mode is
entered.  Subsequent changes to `perl6-imenu-generic-expression' will
not affect existing Perl 6 buffers because imenu-generic-expression is
a local variable.")

;; Add imenu support for perl6-mode.  Note that imenu-generic-expression
;; is buffer-local, so we don't need a local-variable for it.
(setq imenu-generic-expression perl6-imenu-generic-expression
      imenu-case-fold-search nil)

;; Imenu support
(setq imenu-auto-rescan t)
(add-hook 'perl6-mode-hook 'imenu-add-menubar-index)
(add-hook 'perl6-mode-hook
	  (lambda ()
	    (setq imenu-generic-expression perl6-imenu-generic-expression)))

;;===========================
(provide 'perl6-imenu)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; perl6-imenu.el ends here
