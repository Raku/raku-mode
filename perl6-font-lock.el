;;; perl6-font-lock.el --- Font locking and syntax propertizing for Perl 6 -*- lexical-binding: t; -*-

;;; Commentary:

;; All syntaxification and font locking takes place here.

;;; Code:

(defface perl6-identifier '((t :inherit default))
  "Face for identifiers in Perl 6."
  :group 'perl6-faces)

(defface perl6-number '((t :inherit font-lock-constant-face))
  "Face for number literals in Perl 6."
  :group 'perl6-faces)

(defface perl6-number-addition '((t :inherit font-lock-type-face))
  "Face for additional characters attached to numbers."
  :group 'perl6-faces)

(defface perl6-string '((t :inherit font-lock-string-face))
  "Face for strings in Perl 6."
  :group 'perl6-faces)

(defface perl6-comment '((t :inherit font-lock-comment-face))
  "Face for comments in Perl 6."
  :group 'perl6-faces)

(defface perl6-operator '((t :inherit font-lock-builtin-face))
  "Face for operators in Perl 6."
  :group 'perl6-faces)

(defface perl6-type '((t :inherit font-lock-type-face))
  "Face for types in Perl 6."
  :group 'perl6-faces)

(defface perl6-phaser '((t :inherit font-lock-preprocessor-face))
  "Face for phasers in Perl 6."
  :group 'perl6-faces)

(defface perl6-exception '((t :inherit font-lock-keyword-face))
  "Face for exception keywords in Perl 6."
  :group 'perl6-faces)

(defface perl6-module '((t :inherit font-lock-keyword-face))
  "Face for module keywords in Perl 6."
  :group 'perl6-faces)

(defface perl6-routine '((t :inherit font-lock-keyword-face))
  "Face for routine keywords in Perl 6."
  :group 'perl6-faces)

(defface perl6-include '((t :inherit font-lock-keyword-face))
  "Face for include keywords in Perl 6."
  :group 'perl6-faces)

(defface perl6-conditional '((t :inherit font-lock-keyword-face))
  "Face for conditional keywords in Perl 6."
  :group 'perl6-faces)

(defface perl6-scope '((t :inherit font-lock-keyword-face))
  "Face for scope keywords in Perl 6."
  :group 'perl6-faces)

(defface perl6-loop '((t :inherit font-lock-keyword-face))
  "Face for loop keywords in Perl 6."
  :group 'perl6-faces)

(defface perl6-flow-control '((t :inherit font-lock-keyword-face))
  "Face for flow control keywords in Perl 6."
  :group 'perl6-faces)

(defface perl6-pragma '((t :inherit font-lock-keyword-face))
  "Face for pragmas in Perl 6."
  :group 'perl6-faces)

(defface perl6-type-constraint '((t :inherit font-lock-preprocessor-face))
  "Face for type constraint keywords in Perl 6."
  :group 'perl6-faces)

(defface perl6-type-property '((t :inherit font-lock-builtin-face))
  "Face for type constraint properties in Perl 6."
  :group 'perl6-faces)

(defface perl6-sigil '((t :inherit font-lock-variable-name-face))
  "Face for variable sigils in Perl 6."
  :group 'perl6-faces)

(defface perl6-twigil '((t :inherit font-lock-type-face))
  "Face for variable twigils in Perl 6."
  :group 'perl6-faces)

(defface perl6-var-package '((t :inherit font-lock-constant-face))
  "Face for variable names in Perl 6."
  :group 'perl6-faces)

(defface perl6-var-name '((t :inherit font-lock-variable-name-face))
  "Face for variable names in Perl 6."
  :group 'perl6-faces)

(defface perl6-version '((t :inherit font-lock-constant-face))
  "Face for version literals in Perl 6."
  :group 'perl6-faces)

(defface perl6-label '((t :inherit font-lock-constant-face))
  "Face for block labels in Perl 6."
  :group 'perl6-faces)

(eval-when-compile
  (require 'rx)

  (defun perl6-rx-symbol (form)
    "Translate FORM into a regular expression."
    (let ((body (cdr form)))
      (rx-to-string `(and symbol-start ,@body symbol-end) 'no-group)))

  (defconst perl6-rx-constituents
    `((symbol perl6-rx-symbol 0 nil)
      (routine
       . ,(rx (or "macro" "sub" "submethod" "method" "multi" "proto" "only"
                  "category")))
      (module
       . ,(rx (or "module" "class" "role" "package" "enum" "grammar" "slang"
                  "subset")))
      (rule . ,(rx (or "regex" "rule" "token")))
      (include . ,(rx (or "use" "require")))
      (conditional . ,(rx (or "if" "else" "elsif" "unless")))
      (scope . ,(rx (or "let" "my" "our" "state" "temp" "has" "constant")))
      (loop . ,(rx (or "for" "loop" "repeat" "while" "until" "gather" "given")))
      (flow-control
       . ,(rx (or "take" "do" "when" "next" "last" "redo" "return" "contend"
                  "maybe" "defer" "start" "default" "exit" "make" "continue"
                  "break" "goto" "leave" "async" "lift")))
      (phaser
       . ,(rx (or "BEGIN" "CHECK" "INIT" "START" "FIRST" "ENTER" "LEAVE" "KEEP"
                  "UNDO" "NEXT" "LAST" "PRE" "POST" "END" "CATCH" "CONTROL"
                  "TEMP")))
      (exception . ,(rx (or "die" "fail" "try" "warn")))
      (pragma . ,(rx (or "oo" "fatal")))
      (type-constraint
       . ,(rx (or "does" "as" "but" "trusts" "of" "returns" "handles" "where"
                  "augment" "supersede")))
      (type-property
       . ,(rx (or "signature" "context" "also" "shape" "prec" "irs" "ofs" "ors"
                  "export" "deep" "binary" "unary" "reparsed" "rw" "parsed"
                  "cached" "readonly" "defequiv" "will" "ref" "copy" "inline"
                  "tighter" "looser" "equiv" "assoc" "required")))
      (operator-word
       . ,(rx (or "div" "xx" "x" "mod" "also" "leg" "cmp" "before" "after" "eq"
                  "ne" "le" "lt" "not" "gt" "eqv" "ff" "fff" "and" "andthen"
                  "or" "xor" "orelse" "extra" "lcm" "gcd")))
      (operator-char . ,(rx (any "-:+/*~?|=^!%&,<>».;\\∈∉∋∌∩∪≼≽⊂⊃⊄⊅⊆⊇⊈⊉⊍⊎⊖∅")))
      (set-operator
       . ,(rx (opt "R")
              "\("
              (or (char "-^.+|&")
                  (and (char "<>") (opt (char "=+")))
                  "cont"
                  "elem")
              "\)"))
      (rsxz-operator
       . ,(rx
           symbol-start
           (any "RSXZ")
           (or symbol-end
               (or (and (or "div" "mod" "gcd" "lcm" "xx" "x" "does" "but" "cmp"
                            "leg" "eq" "ne" "gt" "ge" "lt" "le" "before" "after"
                            "eqv" "min" "max" "not" "so" "andthen" "and" "or"
                            "orelse")
                        symbol-end)
                   (any ".,")
                   (1+ (regex "[^:\[.,[:space:][:alnum:]]"))))))
      (reduce-operator
       . ,(rx (0+ (any "RSXZ\["))
              (opt (any "RSXZ&"))
              (1+ "\[")
              (opt "\(")
              (or (and (regex "[^[:digit:]@%$]")
                       (0+ (regex "[^\[\{\('\"[:space:]]")))
                  (and (any "@%$")
                       (regex "[^.?^=_[:alpha:]]\[\{\('\"[:space:]]")
                       (0+ (regex "[^\[\{\('\"[:space:]]"))))
              (opt "\)")
              (1+ "\]")))
      (low-type
       . ,(rx (or "int" "int1" "int2" "int4" "int8" "int16" "int32" "int64"
                  "rat" "rat1" "rat2" "rat4" "rat8" "rat16" "rat32" "rat64"
                  "buf" "buf1" "buf2" "buf4" "buf8" "buf16" "buf32" "buf64"
                  "uint" "uint1" "uint2" "uint4" "uint8" "uint16" "uint32"
                  "uint64" "utf8" "utf16" "utf32" "bit" "bool" "bag" "set"
                  "mix" "num" "complex")))
      (high-type
       . ,(rx (or "Object" "Any" "Junction" "Whatever" "Capture" "Match"
                  "Signature" "Proxy" "Matcher" "Package" "Module" "Class"
                  "Grammar" "Scalar" "Array" "Hash" "KeyHash" "KeySet" "KeyBag"
                  "Pair" "List" "Seq" "Range" "Set" "Bag" "Mapping" "Void"
                  "Undef" "Failure" "Exception" "Code" "Block" "Routine" "Sub"
                  "Macro" "Method" "Submethod" "Regex" "Str" "Blob" "Char"
                  "Byte" "Parcel" "Codepoint" "Grapheme" "StrPos" "StrLen"
                  "Version" "Num" "Complex" "Bit" "True" "False" "Order" "Same"
                  "Less" "More" "Increasing" "Decreasing" "Ordered" "Callable"
                  "AnyChar" "Positional" "Associative" "Ordering" "KeyExtractor"
                  "Comparator" "OrderingPair" "IO" "KitchenSink" "Role" "Int"
                  "Rat" "Buf" "UInt" "Abstraction" "Numeric" "Real" "Nil" "Mu")))
      (identifier . ,(rx (regex "[_[:alpha:]]")
                         (0+ (regex "[_[:alnum:]]"))
                         (0+ (any "-'")
                             (regex "[_[:alpha:]]") (0+ (regex "[_[:alnum:]]")))))
      (version . ,(rx "v" (1+ digit) (0+ "." (or "*" (1+ digit))) (opt "+")))
      (number
       . ,(rx
           (group-n 1
             (opt (1+ digit) (opt "_" (1+ digit)))
             (opt ".")
             (1+ digit))
           (opt (group-n 2 (any "Ee"))
                (group-n 3 (opt "-") (1+ digit) (opt "_" (1+ digit))))
           (opt (group-n 4 "i"))))
      (base-number
       . ,(rx symbol-start
              (group-n 1 "0")
              (or (and
                   (group-n 2 "o")
                   (group-n 3 (any "0-7") (0+ (any "0-7_"))))
                  (and
                   (group-n 2 "b")
                   (group-n 3 (any "0-1") (0+ (any "0-1_"))))
                  (and
                   (group-n 2 "x")
                   (group-n 3 (regex "[[:xdigit:]]") (0+ (regex "[[:xdigit:]_]"))))
                  (and
                   (group-n 2 "d")
                   (group-n 3 (regex "[[:digit:]]") (0+ (regex "[[:digit:]_]")))))))))

  (defmacro perl6-rx (&rest sexps)
    "Specialized `rx' variant for perl6-mode."
    (let ((rx-constituents (append perl6-rx-constituents rx-constituents)))
      (cond ((null sexps)
             (error "No regexp"))
            ((cdr sexps)
             (rx-to-string `(and ,@sexps) t))
            (t
             (rx-to-string (car sexps) t))))))

(defconst perl6-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; single-line comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; single-quoted strings
    (modify-syntax-entry ?' "\"" table)
    ;; these are all punctuation chars, not word or symbol chars
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "." table)
    table)
  "The top level syntax table for Perl 6.")

(defconst perl6-bracket-syntax-table
  (let ((table (make-syntax-table perl6-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (modify-syntax-entry ?« "(»" table)
    (modify-syntax-entry ?» ")«" table)
    table)
  "Syntax table for bracketing constructs.")

(defun perl6-syntax-context (&optional state)
  "Return the syntactic context at the parse state of STATE.

If STATE is not provided, the return value of `syntax-ppss' will be used."
    (let* ((state (or state (syntax-ppss)))
           (in-string (nth 3 state))
           (in-comment (nth 4 state)))
      (cond
       (in-string 'string)
       (in-comment 'comment)
       (t nil))))

(defun perl6-forward-brackets (open close length)
  "Move point past the end of a bracketed structure.

Skips over any nested balanced brackets.

OPEN and CLOSE are the bracketing characters (e.g. ?< and ?>).
LENGTH is the length of the brackets (e.g. 2 for a <<foo>>)."
  (let ((pattern (rx-to-string `(and (group (0+ "\\"))
                                     (or (group (= ,length ,open))
                                         (group (= ,length ,close))))))
        (found-closing nil)
        (depth 1))
    (while (and (not found-closing)
                (< (point) (point-max)))
      (re-search-forward pattern (point-max) 'noerror)
      (when (or (not (match-string 1))
                (= 0 (% (length (match-string 1)) 2)))
        (cond ((match-string 2)
               (if (looking-at (rx-to-string open))
                   (re-search-forward (rx-to-string `(1+ ,open)))
                 (setq depth (1+ depth))))
              ((match-string 3)
               (setq depth (1- depth))
               (when (eq depth 0)
                 (setq found-closing t))))))))

(defun perl6-syntax-propertize-embedded-comment ()
  "Add syntax properties to embedded comments \(#`<<foo>>\)."
  (with-syntax-table perl6-bracket-syntax-table
    (when (and (following-char)
               (eq ?\( (char-syntax (following-char))))
      (let* ((comment-beg (- (point) 2))
             (open-delim (following-char))
             (close-delim (matching-paren open-delim)))
        (put-text-property comment-beg (1+ comment-beg)
                             'syntax-table (string-to-syntax "!"))
        (re-search-forward (rx-to-string `(1+ ,open-delim)))
        (let ((delim-length (length (match-string 0))))
          (perl6-forward-brackets open-delim close-delim delim-length)
          (let ((comment-end (point)))
            (put-text-property comment-beg comment-end 'syntax-multiline t)
            (put-text-property (- comment-end 1) comment-end
                               'syntax-table (string-to-syntax "!"))))))))

(defun perl6-syntax-propertize-angles (open-angles)
  "Add syntax properties to angle-bracketed quotes (e.g. <foo> and «bar»).

OPEN-ANGLES is the opening delimiter (e.g. \"«\" or \"<<\")."
  (with-syntax-table perl6-bracket-syntax-table
    (let* ((angle-length (length open-angles))
           (open-angle (string-to-char (car (split-string open-angles "" t))))
           (close-angle (matching-paren open-angle))
           (quote-beg (- (point) angle-length))
           (line-beg (point-at-bol)))
      (unless (or (looking-at "[-=]")
                  (looking-back (rx-to-string `(and (char "+~=!") ,open-angle)) 2))
        (when (or (or (not (looking-at "[\s\n]"))
                      (not (looking-back (rx-to-string `(and (char "\s\n") ,open-angle)) 2)))
                  (looking-at (rx-to-string `(and ,open-angle (1+ (char "\s\n")) ,close-angle)))
                  (looking-back (rx-to-string `(and "=" (1+ space) ,open-angle)) line-beg)
                  (looking-back (rx-to-string `(and "\(" (0+ space) ,open-angle)) line-beg)
                  (or (looking-at "\s*$")
                      (looking-back (rx-to-string `(and line-start (0+ space) ,open-angle)) line-beg))
                  (looking-back (rx-to-string `(and symbol-start (or "enum" "for" "any" "all" "none")
                                          (0+ space) (opt "\)") (0+ space) ,open-angle)) line-beg))
          (put-text-property quote-beg (1+ quote-beg)
                             'syntax-table (string-to-syntax "|"))
          (perl6-forward-brackets open-angle close-angle angle-length)
          (let ((quote-end (- (point) 1)))
            (put-text-property quote-beg quote-end 'syntax-multiline t)
            (put-text-property quote-end (1+ quote-end)
                               'syntax-table (string-to-syntax "|"))))))))

(defun perl6-syntax-propertize-backslash ()
  (when (eq (perl6-syntax-context) nil)
    (put-text-property (match-beginning 0) (match-end 0)
                       'syntax-table (string-to-syntax "."))))

(defun perl6-add-font-lock-hint (property &optional group)
  (let ((beg (match-beginning (or group 1)))
        context)
    (put-text-property beg (1+ beg) property
                       (cons context (match-data)))))

(defun perl6-syntax-propertize (start end)
  "Add context-specific syntax properties to code.

Takes arguments START and END which delimit the region to propertize."
  (let ((case-fold-search nil))
    (goto-char start)
    (remove-text-properties start end '(perl6-metaoperator))
    (funcall
     (syntax-propertize-rules
      ;; [-'] between identifiers are symbol chars
      ((rx (regex "[_[:alnum:]]") (group (any "-'")) (regex "[_[:alpha:]]"))
       (1 "_"))
      ;; same for "::" around identifiers
      ((rx (or (and "::" symbol-start)
               (and symbol-end "::")))
       (0 "_"))
      ((rx "#" (any "`|="))
       (0 (ignore (perl6-syntax-propertize-embedded-comment))))
      ((rx "#" (0+ not-newline))
       (0 (ignore)))
      ((perl6-rx (or set-operator rsxz-operator))
       (0 (ignore (perl6-add-font-lock-hint 'perl6-metaoperator 0))))
      ((rx (1+ (char "<«")))
       (0 (ignore (perl6-syntax-propertize-angles (match-string 0)))))
      ((rx "\\")
       (0 (ignore (perl6-syntax-propertize-backslash)))))
      start end)))

(defun perl6-font-lock-syntactic-face (state)
  "Specify font lock faces based on syntax table entries.

Takes STATE, the parse state."
  (pcase (perl6-syntax-context state)
    (`string 'perl6-string)
    (`comment 'perl6-comment)))

(defun perl6-search-when (regex condition limit)
  "Search forward for REGEX if the match satisfies CONDITION.

CONDITION should be a lambda that will be called after REGEX
matches.  If CONDITION returns non-nil, this function will set the
match data, then move point forward and return its position, like
`re-search-forward' would.

If CONDITION returns nil, further searches for REGEX will be
performed until CONDITION returns non-nil or REGEX fails to
match.

LIMIT can be used to bound the search."
  (let ((limit (or limit (point-max)))
        (keep-searching t)
        (new-match-data))
    (save-excursion
      (save-match-data
        (while keep-searching
          (if (re-search-forward regex limit t)
              (when (save-excursion (save-match-data (funcall condition)))
                (setq new-match-data (match-data)
                      keep-searching nil))
            (setq keep-searching nil)))))
    (when new-match-data
      (set-match-data new-match-data)
      (goto-char (match-end 0)))))

(defun perl6-match-type-constraint (limit)
  (perl6-search-when
   (perl6-rx (or (group (symbol type-constraint))
                 (and (group (symbol "is"))
                      (1+ space)
                      (opt (group (symbol type-property))))))
   (lambda ()
     (goto-char (match-beginning 0))
     (let ((context (perl6-syntax-context)))
       (and
        (eq context nil)
        (not (looking-back (rx (or (char ".^")
                                   (and line-start (0+ space)))))))))
   limit))

(defun perl6-match-property (property context limit)
  (when (symbolp context)
    (setq context (list context)))
  (let ((pos (next-single-char-property-change (point) property nil limit)))
    (when (> pos (point))
      (let ((pos-with-match-data (if (and (= pos (1+ (point)))
                                          (= (point) (point-at-bol))
                                          (not (get-text-property pos property)))
                                     (point)
                                   pos)))
        (goto-char pos)
        (let ((value (get-text-property pos-with-match-data property)))
          (if (and value (memq (car value) context))
              (progn (set-match-data (cdr value)) t)
            (perl6-match-property property context limit)))))))

(defun perl6-match-metaoperator (limit)
  (perl6-match-property 'perl6-metaoperator nil limit))

(defconst perl6-font-lock-keywords
  `(
    (,(perl6-rx reduce-operator) 0 'perl6-operator)
    (perl6-match-metaoperator 0 'perl6-operator)
    (,(perl6-rx (group (any "@$%&"))
                (or
                 "<"
                 (and (0+ space)
                      (or (any ",\)\}") (symbol "where")))))
     1 'perl6-sigil)
    (,(perl6-rx (group (1+ (char "@$%&")))
                (group (opt (char ".^*?=!~")))
                (group (opt (or (and "::" (0+ (and identifier "::")))
                                (1+ identifier "::"))))
                (group (or (1+ digit)
                           (and identifier symbol-end))))
     (1 'perl6-sigil)
     (2 'perl6-twigil)
     (3 'perl6-var-package)
     (4 'perl6-var-name))
    (,(perl6-rx symbol-start version) 0 'perl6-version)
    (perl6-match-type-constraint
     (1 'perl6-type-constraint nil t)
     (2 'perl6-type-constraint nil t)
     (3 'perl6-type-property nil t))
    (,(perl6-rx (group (any ".^")) (group identifier symbol-end))
     (1 'perl6-operator)
     (2 'perl6-identifier))
    (,(perl6-rx (group (symbol identifier)) (1+ space) (group "=>"))
     (1 'perl6-string)
     (2 'perl6-operator))
    (,(perl6-rx (group symbol-start rule) (1+ space) (group identifier))
     (1 'perl6-routine)
     (2 'perl6-identifier))
    (,(rx (group "$") (group (any "0-9/!¢")))
     (1 'perl6-sigil)
     (2 'perl6-var-name))
    (,(perl6-rx (group symbol-start high-type) "(") 1 'perl6-type)
    (,(perl6-rx (group symbol-start identifier) "(") 1 'perl6-identifier)
    (,(perl6-rx
       (or (and (group-n 1 (any "$@%&")) "(")
           (group-n 2 (symbol (or "item" "list" "hash")))))
     (1 'perl6-operator nil t)
     (2 'perl6-operator nil t))
    (,(perl6-rx (symbol (or low-type high-type))) 0 'perl6-type)
    (,(perl6-rx (group ":") (group (symbol identifier)))
     (1 'perl6-operator)
     (2 'perl6-string))
    (,(perl6-rx (symbol operator-word)) 0 'perl6-operator)
    (,(perl6-rx (symbol phaser)) 0 'perl6-phaser)
    (,(perl6-rx (symbol exception)) 0 'perl6-exception)
    (,(perl6-rx (symbol module)) 0 'perl6-module)
    (,(perl6-rx (symbol scope)) 0 'perl6-scope)
    (,(perl6-rx (symbol conditional)) 0 'perl6-conditional)
    (,(perl6-rx (symbol routine)) 0 'perl6-routine)
    (,(perl6-rx (symbol include)) 0 'perl6-include)
    (,(perl6-rx (symbol loop)) 0 'perl6-loop)
    (,(perl6-rx (symbol flow-control)) 0 'perl6-flow-control)
    (,(perl6-rx (symbol pragma)) 0 'perl6-pragma)
    (,(perl6-rx (symbol (or "Inf" "NaN")))
     0 'perl6-number)
    (,(perl6-rx line-start (0+ space) (group identifier ":") (or space line-end))
     1 'perl6-label)
    (,(perl6-rx (symbol (or "goto" "next" "last" "redo"))
                (0+ space)
                (group (symbol identifier)))
     1 'perl6-label)
    (,(perl6-rx
       (or symbol-start "::")
       identifier
       (opt (0+ "::" identifier))
       (opt "::"))
     0 'perl6-identifier)
    (,(perl6-rx number)
     (1 'perl6-number)
     (2 'perl6-number-addition nil t)
     (3 'perl6-number nil t)
     (4 'perl6-number-addition nil t))
    (,(perl6-rx operator-char) 0 'perl6-operator)
    (,(perl6-rx base-number)
     (1 'perl6-number)
     (2 'perl6-number-addition)
     (3 'perl6-number)))
  "Font lock keywords for Perl 6.")

(provide 'perl6-font-lock)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; perl6-font-lock.el ends here
