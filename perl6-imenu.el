;;; perl6-imenu.el --- Imenu support Perl 6 -*- lexical-binding: t; -*-

;; Imenu functions and variables are defined here.

;; TODO: tighten regexes to distinguish correct naming for Perl 6
;; vs. NQP (make a minor mode?)

;; Regex definitions:
(defvar perl6-vars-regex
  (concat
   "^\\s-*"                      ; leading ws allowed
   "\\(?:my\\|our\\)\\s-+"       ; scope of var, followed by mandatory ws
   "\\("                         ; start capture group 1 for the var name
        "\\(?:\\$\\|@\\|%\\)"    ;   sigil for type of var
        "\\(?:[-_[:alnum:]]+\\)" ;   the var name ends with ws
   "\\)"                         ; end of capture group 1
   ))

(defvar perl6-subs-regex
  (concat
    "^\\s-*"                      ; leading ws allowed
    "\\(?:my\\s-+\\|our\\s-+\\)?" ; optional specific scope followed by at least one space
        ; must have one of the five type identifiers followed by at least one space:
    "\\(?:multi\\s-+sub\\|multi\\s-+method\\|sub\\|method\\|multi\\|proto\\)\\s-+"
    "\\([-_[:alnum:]]+\\)"        ; the capture group of the sub name
   ))

(defvar perl6-classes-regex
  (concat
    "^\\s-*"                      ; leading ws allowed
        ; must have one of the four type identifiers followed by at least one space:
    "class\\s-+"
    "\\([-_[:alnum:]]+\\)"                   ; the capture group of the sub name
    ;"[\\n\\s\\-{]+"             ; ended by whitespace or an opening curly brace'
   ))

(defvar perl6-imenu-generic-expression
  `(
    ;; the names are in reverse desired order since they are evaluated here last first
    ;("Variables" "^\\s-*\\(?:my\\|our\\)\\s-+\\(\\(?:\\$\\|@\\|%\\)\\(?:[_[:alnum:]]+\\)\\)" 1)
    ("Classes" ,perl6-classes-regex 1)
    ("Variables" ,perl6-vars-regex 1)
    ;;("Subs/Methods" "^\\s-*\\(?:my\\s-+\\|our\\s-+\\)?\\(?:multi\\s-+sub\\|multi\\s-+method\\|sub\\|method\\|multi\\)\\s-+\\(.+)\\)" 1)
    ("Subs/Methods" ,perl6-subs-regex 1)
    )
      "Define interesting points in the Perl 6 buffer for `imenu'.

This is used to set `imenu-generic-expression' when Perl 6 mode is
entered.  Subsequent changes to `perl6-imenu-generic-expression' will
not affect existing Perl 6 buffers because imenu-generic-expression is
a local variable.")

;;===========================
(provide 'perl6-imenu)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; perl6-imenu.el ends here
