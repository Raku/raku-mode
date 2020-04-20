;;; raku-mode-test.el --- Raku Mode: Unit test suite  -*- lexical-binding: t; -*-

;;; Commentary:

;; The unit test suite of Raku Mode.

;;; Code:

(require 'raku-mode)
(require 'ert)


;;;; Utilities

(defmacro raku-test-with-temp-buffer (content &rest body)
  "In the temporary buffer CONTENT, evaluate BODY."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (raku-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(defun raku-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.

If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (raku-test-with-temp-buffer content
        (get-text-property pos 'face))
    (get-text-property pos 'face)))

(defconst raku-test-syntax-classes
  [whitespace punctuation word symbol open-paren close-paren expression-prefix
              string-quote paired-delim escape character-quote comment-start
              comment-end inherit generic-comment generic-string]
  "Readable symbols for syntax classes.

Each symbol in this vector corresponding to the syntax code of
its index.")

(defun raku-test-syntax-at (pos)
  "Get the syntax at POS.

Get the syntax class symbol at POS, or nil if there is no syntax a
POS."
  (let ((code (syntax-class (syntax-after pos))))
    (aref raku-test-syntax-classes code)))


;;;; Font locking
(ert-deftest raku-syntax-propertize/colons-identifier ()
  :tags '(syntax-table syntax-properties)
  (raku-test-with-temp-buffer "class Foo::Bar"
                                (should (eq (raku-test-syntax-at 10) 'symbol))
                                (should (eq (raku-test-syntax-at 11) 'symbol))))

(ert-deftest raku-syntax-propertize/angles ()
  :tags '(syntax-table syntax-properties)
  (raku-test-with-temp-buffer "my @foo = <bar>; for @foo <-> $bar {}"
                                (should (eq (raku-test-syntax-at 11) 'generic-string))
                                (should (eq (raku-test-syntax-at 15) 'generic-string))
                                (should (eq (raku-test-syntax-at 27) 'punctuation))
                                (should (eq (raku-test-syntax-at 29) 'punctuation))))

(ert-deftest raku-syntax-propertize/dq-words ()
  :tags '(syntax-table syntax-properties)
  (raku-test-with-temp-buffer "foo «bar1 bar2» bla <<baz1 baz2>> quux"
                               (should (eq (raku-test-syntax-at 1) 'word))
                               (should (eq (raku-test-syntax-at 5) 'generic-string))
                               (should (eq (raku-test-syntax-at 15) 'generic-string))
                               (should (eq (raku-test-syntax-at 21) 'generic-string))
                               (should (eq (raku-test-syntax-at 22) 'punctuation))
                               (should (eq (raku-test-syntax-at 32) 'punctuation))
                               (should (eq (raku-test-syntax-at 33) 'generic-string))))

(ert-deftest raku-mode-syntax-table/fontify-dq-string ()
  :tags '(fontification syntax-table)
  (should (eq (raku-test-face-at 9 "$foo = \"bar\"") 'raku-string)))

(ert-deftest raku-mode-syntax-table/fontify-set-operator ()
  :tags '(fontification syntax-table)
  (should (eq (raku-test-face-at 6 "$mh (<+) $m") 'raku-operator)))

(ert-deftest raku-mode-syntax-table/fontify-sq-string ()
  :tags '(fontification syntax-table)
  (should (eq (raku-test-face-at 9 "$foo = 'bar'") 'raku-string)))

(ert-deftest raku-mode-syntax-table/fontify-line-comment ()
  :tags '(fontification syntax-table)
  (raku-test-with-temp-buffer "# class
bar #`<foo> baz"
    (should (eq (raku-test-face-at 3) 'raku-comment))
    (should (eq (raku-test-face-at 7) 'raku-comment))
    (should (eq (raku-test-face-at 8) 'raku-comment))
    (should (eq (raku-test-face-at 9) 'raku-identifier))
    (should (eq (raku-test-face-at 16) 'raku-comment))))

(ert-deftest raku-font-lock-keywords/phaser ()
  :tags '(fontification font-lock-keywords)
  (raku-test-with-temp-buffer "BEGIN {"
    (should (eq (raku-test-face-at 1) 'raku-phaser))))

(ert-deftest perl5-font-lock-keywords/variable ()
  :tags '(fontification syntax-table)
  (raku-test-with-temp-buffer "$::foo @!bar"
    (should (eq (raku-test-face-at 3) 'raku-var-package))
    (should (eq (raku-test-face-at 4) 'raku-var-name))
    (should (eq (raku-test-face-at 8) 'raku-sigil))
    (should (eq (raku-test-face-at 9) 'raku-twigil))
    (should (eq (raku-test-face-at 10) 'raku-var-name))))

(provide 'raku-mode-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; raku-mode-test.el ends here
