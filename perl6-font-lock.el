;;; perl6-font-lock.el --- Font locking and syntax propertizing for Perl 6 -*- lexical-binding: t; -*-

;;; Commentary:

;; All syntaxification and font locking takes place here.

;;; Code:

(defface perl6-identifier
  '((t :inherit default))
  "Face for identifiers in Perl 6."
  :group 'perl6)

(defface perl6-string
  '((t :inherit font-lock-string-face))
  "Face for strings in Perl 6."
  :group 'perl6)

(defface perl6-comment
  '((t :inherit font-lock-comment-face))
  "Face for comments in Perl 6."
  :group 'perl6)

(defface perl6-operator
  '((t :inherit font-lock-builtin-face))
  "Face for operators in Perl 6."
  :group 'perl6)

(defface perl6-type
  '((t :inherit font-lock-type-face))
  "Face for types in Perl 6."
  :group 'perl6)

(defface perl6-phaser
  '((t :inherit font-lock-preprocessor-face))
  "Face for phasers in Perl 6."
  :group 'perl6)

(defface perl6-exception
  '((t :inherit font-lock-exit-face))
  "Face for exception keywords in Perl 6."
  :group 'perl6)

(defface perl6-module
  '((t :inherit font-lock-keyword-face))
  "Face for module keywords in Perl 6."
  :group 'perl6)

(defface perl6-routine
  '((t :inherit font-lock-keyword-face))
  "Face for routine keywords in Perl 6."
  :group 'perl6)

(defface perl6-include
  '((t :inherit font-lock-keyword-face))
  "Face for include keywords in Perl 6."
  :group 'perl6)

(defface perl6-conditional
  '((t :inherit font-lock-keyword-face))
  "Face for conditional keywords in Perl 6."
  :group 'perl6)

(defface perl6-scope
  '((t :inherit font-lock-keyword-face))
  "Face for scope keywords in Perl 6."
  :group 'perl6)

(defface perl6-loop
  '((t :inherit font-lock-keyword-face))
  "Face for loop keywords in Perl 6."
  :group 'perl6)

(defface perl6-flow-control
  '((t :inherit font-lock-keyword-face))
  "Face for flow control keywords in Perl 6."
  :group 'perl6)

(defface perl6-pragma
  '((t :inherit font-lock-keyword-face))
  "Face for pragmas in Perl 6."
  :group 'perl6)

(eval-when-compile
  (require 'rx)

  (defun perl6-rx-symbol (form)
    "Translate FORM into a regular expression."
    (let ((body (cdr form)))
      (rx-to-string `(and symbol-start ,@body symbol-end) 'no-group)))

  (defconst perl6-rx-constituents
    `((symbol perl6-rx-symbol 0 nil)
      (routine . ,(rx (or "macro" "sub" "submethod" "method"
                                  "multi" "proto" "only" "category")))
      (module . ,(rx (or "module" "class" "role" "package"
                                 "enum" "grammar" "slang" "subset")))
      (include . ,(rx (or "use" "require")))
      (conditional . ,(rx (or "if" "else" "elsif" "unless")))
      (scope . ,(rx (or "let" "my" "our" "state" "temp" "has"
                              "constant")))
      (loop . ,(rx (or "for" "loop" "repeat" "while" "until" "gather"
                       "given")))
      (flow-control . ,(rx (or "take" "do" "when" "next" "last" "redo"
                               "return" "contend" "maybe" "defer" "start"
                               "default" "exit" "make" "continue" "break"
                               "goto" "leave" "async" "lift")))
      (phaser . ,(rx (or "BEGIN" "CHECK" "INIT" "START" "FIRST" "ENTER"
                         "LEAVE" "KEEP" "UNDO" "NEXT" "LAST" "PRE" "POST"
                         "END" "CATCH" "CONTROL" "TEMP")))
      (exception . ,(rx (or "die" "fail" "try" "warn")))
      (pragma . ,(rx (or "oo" "fatal")))
      (operator-word . ,(rx (or "div" "xx" "x" "mod" "also" "leg" "cmp"
                           "before" "after" "eq" "ne" "le" "lt" "not"
                           "gt" "eqv" "ff" "fff" "and" "andthen" "or"
                           "xor" "orelse" "extra" "lcm" "gcd")))
      (operator-char . ,(rx (any "-+/*~?|=^!%&,<>».;\\∈∉∋∌∩∪≼≽⊂⊃⊄⊅⊆⊇⊈⊉⊍⊎⊖∅")))
      (low-type . ,(rx (or "int" "int1" "int2" "int4" "int8" "int16"
                           "int32" "int64" "rat" "rat1" "rat2" "rat4"
                           "rat8" "rat16" "rat32" "rat64" "buf" "buf1"
                           "buf2" "buf4" "buf8" "buf16" "buf32" "buf64"
                           "uint" "uint1" "uint2" "uint4" "uint8"
                           "uint16" "uint32" "uint64" "utf8" "utf16"
                           "utf32" "bit" "bool" "bag" "set" "mix" "num"
                           "complex")))
      (high-type . ,(rx (or "Object" "Any" "Junction" "Whatever"
                            "Capture" "Match" "Signature" "Proxy"
                            "Matcher" "Package" "Module" "Class"
                            "Grammar" "Scalar" "Array" "Hash" "KeyHash"
                            "KeySet" "KeyBag" "Pair" "List" "Seq"
                            "Range" "Set" "Bag" "Mapping" "Void" "Undef"
                            "Failure" "Exception" "Code" "Block"
                            "Routine" "Sub" "Macro" "Method" "Submethod"
                            "Regex" "Str" "Blob" "Char" "Byte" "Parcel"
                            "Codepoint" "Grapheme" "StrPos" "StrLen"
                            "Version" "Num" "Complex" "Bit" "True"
                            "False" "Order" "Same" "Less" "More"
                            "Increasing" "Decreasing" "Ordered"
                            "Callable" "AnyChar" "Positional"
                            "Associative" "Ordering" "KeyExtractor"
                            "Comparator" "OrderingPair" "IO"
                            "KitchenSink" "Role" "Int" "Rat" "Buf" "UInt"
                            "Abstraction" "Numeric" "Real" "Nil"
                            "Mu")))
      (identifier . ,(rx (and (any "A-Za-z")
                              (1+ (or (any "A-Za-z0-9")
                                      (and (any "-'") (any "A-Za-z")))))))))
  (defmacro perl6-rx (&rest sexps)
    "Specialized `rx' variant for perl6-mode."
    (let ((rx-constituents (append perl6-rx-constituents rx-constituents)))
      (cond ((null sexps)
             (error "No regexp"))
            ((cdr sexps)
             (rx-to-string `(and ,@sexps) t))
            (t
             (rx-to-string (car sexps) t))))))

(defvar perl6-mode-syntax-table
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

(defun perl6-syntax-propertize (start end)
  "Add context-specific syntax properties to code.

Takes arguments START and END which delimit the region to propertize."
  (let ((case-fold-search nil))
    (goto-char start)
    (funcall
     (syntax-propertize-rules
      ;; [-'] between identifiers are symbol chars
      ((rx (any "A-Za-z") (group (any "-'")) (any "A-Za-z"))
       (1 "_"))
      ;; multiline comments
      ((rx (group "#`<") (*? anything) (group ">"))
       (1 "< b")
       (2 "> b")))
      start end)))

(defun perl6-font-lock-syntactic-face (state)
  "Specify font lock faces based on syntax table entries.

Takes STATE, the parse state."
  (let ((in-string (nth 3 state))
        (in-comment (nth 4 state)))
    (cond
     (in-string 'perl6-string)
     (in-comment 'perl6-comment))))

(defvar perl6-font-lock-keywords
  `(
    (,(perl6-rx (group (any ".^")) (group identifier symbol-end))
     (1 'perl6-operator)
     (2 'perl6-identifier))
    (,(perl6-rx (group symbol-start high-type) "(") 1 'perl6-type)
    (,(perl6-rx (group symbol-start identifier) "(") 1 'perl6-identifier)
    (,(perl6-rx (symbol (or low-type high-type))) 0 'perl6-type)
    (,(perl6-rx (or (symbol operator-word) operator-char)) 0 'perl6-operator)
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
    (,(perl6-rx (symbol operator-word)) 0 'perl6-operator)
    (,(perl6-rx (symbol identifier)) 0 'perl6-identifier))
  "Font lock keywords for Perl 6.")

(provide 'perl6-font-lock)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; perl6-font-lock.el ends here
