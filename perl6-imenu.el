;;; perl6-imenu.el --- Imenu support Perl 6 -*- lexical-binding: t; -*-

;; Imenu functions and variables are defined here.

;; Definition of "identifiers" (names) from
;;   https://docs.perl6.org/language/syntax#Identifiers
;;
;; Identifiers are a grammatical building block that occur in several
;; places. An identifier is a primitive name, and must start with an
;; alphabetic character (or an underscore), followed by zero or more
;; word characters (alphabetic, underscore or number). You can also
;; embed dashes - or single quotes ' in the middle, but not two in a
;; row, and only if followed immediately by an alphabetic character.
;;
;; For NQP names, no embedded hyphens or single quotes are allowed.

;; Regex definitions:
(defvar perl6-name-regex
  (concat
   "[_[:alpha:]]"           ; mandatory leading character
   "\\(?:[-']?[[:alpha:]]"  ; rest of the name allowing embedded hyphens or single quotes or '::'
   "\\|[_[:alnum:]]"
   "\\|\\:\\:[_[:alnum:]]"
   "\\)*"
   ))

(defvar nqp-name-regex
  (concat
   "[_[:alpha:]]"   ; mandatory leading character
   "[_[:alnum:]]*"  ; rest of the name (stricter than Perl 6 name)
   ))

(defvar perl6-vars-regex
  (concat
   "^\\s-*"                 ; leading ws allowed
   "\\(?:my\\|our\\|state\\)\\s-+"  ; scope of var, followed by mandatory ws
   "\\("                    ; start capture group 1 for the var name
   "\\(?:\\$\\|@\\|%\\)"    ; sigil for type of var
   "\\(?:"                  ; start shy group for choice of one type name
   perl6-name-regex
   "\\|"
   nqp-name-regex
   "\\)"                    ; end shy group
   "\\)"                    ; end of capture group 1
   ))

(defvar perl6-subs-regex
  (concat
   "^\\s-*"                      ; leading ws allowed
   "\\(?:my\\s-+\\|our\\s-+\\)?" ; optional specific scope followed by at least one space
                                 ; must have one of the five type identifiers
                                 ; followed by at least one space:
   "\\(?:multi\\s-+sub\\|multi\\s-+method\\|sub\\|method\\|multi\\|proto\\)\\s-+"
   "\\!?"                        ; optional private marker
   "\\("                         ; start capture group 1 for the sub name
   perl6-name-regex
   "\\|"
   nqp-name-regex
   "\\)"                         ; end of capture group 1
   ))

(defvar perl6-classes-regex
  (concat
   "^\\s-*"           ; leading ws allowed
                      ; must have one of the four type identifiers followed by at least one space:
   "class\\s-+"
   "\\("              ; start capture group 1 of the class name
   perl6-name-regex
   "\\|"
   nqp-name-regex
   "\\)"              ; end of capture group 1
   ))

(defvar perl6-regexes-regex
  (concat
   "^\\s-*"           ; leading ws allowed
                      ; must have an identifier followed by at least one space:
   "regex\\s-+"
   "\\("              ; start capture group 1 of the regex name
   perl6-name-regex
   "\\|"
   nqp-name-regex
   "\\)"              ; end of capture group 1
   ))

(defvar perl6-tokens-regex
  (concat
   "^\\s-*"           ; leading ws allowed
                      ; must have an identifier followed by at least one space:
   "token\\s-+"
   "\\("              ; start capture group 1 of the regex name
   perl6-name-regex
   "\\|"
   nqp-name-regex
   "\\)"              ; end of capture group 1
   ))

(defvar perl6-rules-regex
  (concat
   "^\\s-*"           ; leading ws allowed
                      ; must have an identifier followed by at least one space:
   "rule\\s-+"
   "\\("              ; start capture group 1 of the regex name
   perl6-name-regex
   "\\|"
   nqp-name-regex
   "\\)"              ; end of capture group 1
   ))

(defvar perl6-grammars-regex
  (concat
   "^\\s-*"           ; leading ws allowed
                      ; must have an identifier followed by at least one space:
   "grammar\\s-+"
   "\\("              ; start capture group 1 of the regex name
   perl6-name-regex
   "\\|"
   nqp-name-regex
   "\\)"              ; end of capture group 1
   ))

(defvar perl6-imenu-generic-expression
  `(
    ;; the names are in reverse desired order since they are evaluated here last first
    ("Rules" ,perl6-rules-regex 1)
    ("Tokens" ,perl6-tokens-regex 1)
    ("Regexes" ,perl6-regexes-regex 1)
    ("Grammars" ,perl6-grammars-regex 1)
    ("Classes" ,perl6-classes-regex 1)
    ("Variables" ,perl6-vars-regex 1)
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
