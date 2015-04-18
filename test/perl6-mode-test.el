;;; perl6-mode-test.el --- Perl 6 Mode: Unit test suite  -*- lexical-binding: t; -*-

;;; Commentary:

;; The unit test suite of Perl 6 Mode.

;;; Code:

(require 'perl6-mode)
(require 'ert)


;;;; Utilities

(defmacro perl6-test-with-temp-buffer (content &rest body)
  "In the temporary buffer CONTENT, evaluate BODY."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (perl6-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(defun perl6-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.

If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (perl6-test-with-temp-buffer content
        (get-text-property pos 'face))
    (get-text-property pos 'face)))

(defconst perl6-test-syntax-classes
  [whitespace punctuation word symbol open-paren close-paren expression-prefix
              string-quote paired-delim escape character-quote comment-start
              comment-end inherit generic-comment generic-string]
  "Readable symbols for syntax classes.

Each symbol in this vector corresponding to the syntax code of
its index.")

(defun perl6-test-syntax-at (pos)
  "Get the syntax at POS.

Get the syntax class symbol at POS, or nil if there is no syntax a
POS."
  (let ((code (syntax-class (syntax-after pos))))
    (aref perl6-test-syntax-classes code)))


;;;; Font locking
(ert-deftest perl6-syntax-propertize/colons-identifier ()
  :tags '(syntax-table syntax-properties)
  (perl6-test-with-temp-buffer "class Foo::Bar"
                                (should (eq (perl6-test-syntax-at 10) 'symbol))
                                (should (eq (perl6-test-syntax-at 11) 'symbol))))

(ert-deftest perl6-syntax-propertize/dq-words ()
  :tags '(syntax-table syntax-properties)
  (perl6-test-with-temp-buffer "foo «bar» bla <<baz>> quux"
                               (should (eq (perl6-test-syntax-at 1) 'word))
                               (should (eq (perl6-test-syntax-at 5) 'generic-string))
                               (should (eq (perl6-test-syntax-at 9) 'generic-string))
                               (should (eq (perl6-test-syntax-at 15) 'generic-string))
                               (should (eq (perl6-test-syntax-at 16) 'punctuation))
                               (should (eq (perl6-test-syntax-at 20) 'punctuation))
                               (should (eq (perl6-test-syntax-at 21) 'generic-string))))

(ert-deftest perl6-mode-syntax-table/fontify-dq-string ()
  :tags '(fontification syntax-table)
  (should (eq (perl6-test-face-at 8 "$foo = \"bar\"") 'perl6-string)))

(ert-deftest perl6-mode-syntax-table/fontify-sq-string ()
  :tags '(fontification syntax-table)
  (should (eq (perl6-test-face-at 8 "$foo = 'bar'") 'perl6-string)))

(ert-deftest perl6-mode-syntax-table/fontify-line-comment ()
  :tags '(fontification syntax-table)
  (perl6-test-with-temp-buffer "# class
bar #`<foo> baz"
    (should (eq (perl6-test-face-at 3) 'perl6-comment))
    (should (eq (perl6-test-face-at 7) 'perl6-comment))
    (should (eq (perl6-test-face-at 8) 'perl6-comment))
    (should (eq (perl6-test-face-at 9) 'perl6-identifier))
    (should (eq (perl6-test-face-at 16) 'perl6-comment))
    (should (eq (perl6-test-face-at 21) 'perl6-identifier))))

(ert-deftest perl6-font-lock-keywords/phaser ()
  :tags '(fontification font-lock-keywords)
  (perl6-test-with-temp-buffer "BEGIN {"
    (should (eq (perl6-test-face-at 1) 'perl6-phaser))))

(provide 'perl6-mode-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; perl6-mode-test.el ends here
