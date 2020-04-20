;;; raku-imenu.el --- Imenu support Raku -*- lexical-binding: t; -*-

;; Imenu functions and variables are defined here.

;; Definition of "identifiers" (names) from
;;   https://docs.raku.org/language/syntax#Identifiers
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
(defvar raku-name-regex
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
   "[_[:alnum:]]*"  ; rest of the name (stricter than Raku name)
   ))

(defvar raku-vars-regex
  (concat
   "^\\s-*"                 ; leading ws allowed
   "\\(?:my\\|our\\|state\\)\\s-+"  ; scope of var, followed by mandatory ws
   "\\("                    ; start capture group 1 for the var name
   "\\(?:\\$\\|@\\|%\\)"    ; sigil for type of var
   "\\(?:"                  ; start shy group for choice of one type name
   raku-name-regex
   "\\|"
   nqp-name-regex
   "\\)"                    ; end shy group
   "\\)"                    ; end of capture group 1
   ))

(defvar raku-subs-regex
  (concat
   "^\\s-*"                      ; leading ws allowed
   "\\(?:my\\s-+\\|our\\s-+\\)?" ; optional specific scope followed by at least one space
                                 ; must have one of the five type identifiers
                                 ; followed by at least one space:
   "\\(?:multi\\s-+sub\\|multi\\s-+method\\|sub\\|method\\|multi\\|proto\\)\\s-+"
   "\\!?"                        ; optional private marker
   "\\("                         ; start capture group 1 for the sub name
   raku-name-regex
   "\\|"
   nqp-name-regex
   "\\)"                         ; end of capture group 1
   ))

(defvar raku-classes-regex
  (concat
   "^\\s-*"           ; leading ws allowed
                      ; must have one of the four type identifiers followed by at least one space:
   "class\\s-+"
   "\\("              ; start capture group 1 of the class name
   raku-name-regex
   "\\|"
   nqp-name-regex
   "\\)"              ; end of capture group 1
   ))

(defvar raku-regexes-regex
  (concat
   "^\\s-*"           ; leading ws allowed
                      ; must have an identifier followed by at least one space:
   "regex\\s-+"
   "\\("              ; start capture group 1 of the regex name
   raku-name-regex
   "\\|"
   nqp-name-regex
   "\\)"              ; end of capture group 1
   ))

(defvar raku-tokens-regex
  (concat
   "^\\s-*"           ; leading ws allowed
                      ; must have an identifier followed by at least one space:
   "token\\s-+"
   "\\("              ; start capture group 1 of the regex name
   raku-name-regex
   "\\|"
   nqp-name-regex
   "\\)"              ; end of capture group 1
   ))

(defvar raku-rules-regex
  (concat
   "^\\s-*"           ; leading ws allowed
                      ; must have an identifier followed by at least one space:
   "rule\\s-+"
   "\\("              ; start capture group 1 of the regex name
   raku-name-regex
   "\\|"
   nqp-name-regex
   "\\)"              ; end of capture group 1
   ))

(defvar raku-grammars-regex
  (concat
   "^\\s-*"           ; leading ws allowed
                      ; must have an identifier followed by at least one space:
   "grammar\\s-+"
   "\\("              ; start capture group 1 of the regex name
   raku-name-regex
   "\\|"
   nqp-name-regex
   "\\)"              ; end of capture group 1
   ))

(defvar raku-imenu-generic-expression
  `(
    ;; the names are in reverse desired order since they are evaluated here last first
    ("Rules" ,raku-rules-regex 1)
    ("Tokens" ,raku-tokens-regex 1)
    ("Regexes" ,raku-regexes-regex 1)
    ("Grammars" ,raku-grammars-regex 1)
    ("Classes" ,raku-classes-regex 1)
    ("Variables" ,raku-vars-regex 1)
    ("Subs/Methods" ,raku-subs-regex 1)
    )
      "Define interesting points in the Raku buffer for `imenu'.

This is used to set `imenu-generic-expression' when Raku mode is
entered.  Subsequent changes to `raku-imenu-generic-expression' will
not affect existing Raku buffers because imenu-generic-expression is
a local variable.")

;;===========================
(provide 'raku-imenu)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; raku-imenu.el ends here
