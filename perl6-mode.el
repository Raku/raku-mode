;;; perl6-mode.el --- Major mode for editing Perl 6 code -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hinrik Örn Sigurðsson <hinrik.sig@gmail.com>

;; Author: Hinrik Örn Sigurðsson <hinrik.sig@gmail.com>
;; URL: https://github.com/hinrik/perl6-mode
;; Keywords: languages
;; Version: 0.1-git
;; Package-Requires: ((emacs "24.4") (pkg-info "0.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; GNU Emacs 24 major mode for editing Perl 6 code.

;; Currently only provides very basic syntax highlighting.

;;; Code:

(declare-function pkg-info-version-info "pkg-info" (library))

(defgroup perl6 nil
  "Major mode for editing Perl 6 code."
  :prefix "perl6-"
  :group 'language)

(require 'perl6-detect)
(require 'perl6-font-lock)


(require 'smie)

(defconst perl6-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2 '((assoc ";") (assoc ",") (left ":")))))

(defun perl6-smie--not-interpolation-p ()
  (save-excursion
    (forward-char -1)
    (or (zerop (skip-chars-backward "-[:alnum:]"))
        (not (looking-back "#{\\$" (- (point) 3))))))



(defun perl6-smie--forward-token ()
  (cond
   ((and (eq (char-before) ?\})
	    (perl6-smie--not-interpolation-p)
	    ;; FIXME: If the next char is not whitespace, what should we do?
	    (or (memq (char-after) '(?\s ?\t ?\n))
		(looking-at comment-start-skip)))
       (if (memq (char-after) '(?\s ?\t ?\n))
	   (forward-char 1) (forward-comment 1))
       ";")
   ((progn (forward-comment (point-max))
           (looking-at "[;,:]"))
    (forward-char 1) (match-string 0))
   (t (smie-default-forward-token))))

(defun perl6-smie--backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ;; FIXME: If the next char is not whitespace, what should we do?
     ((and (eq (char-before) ?\}) (perl6-smie--not-interpolation-p)
           (> pos (point))) ";")
     ((memq (char-before) '(?\; ?\, ?\:))
      (forward-char -1) (string (char-after)))
     (t (smie-default-backward-token)))))

(defun perl6-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) perl6-indent-offset)
    (`(:elem . arg) 0)
    (`(:list-intro . ,(or `";" `"")) t) ;"" stands for BOB (bug#15467).
    (`(:before . "{")
     (when (smie-rule-hanging-p)
       (smie-backward-sexp ";")
       (smie-indent-virtual)))
    (`(:before . ,(or "{" "("))
     (if (smie-rule-hanging-p) (smie-rule-parent 0)))))

(defcustom perl6-indent-offset 4
  "Basic size of one indentation step."
  :version "22.2"
  :type 'integer)


;;;###autoload
(define-derived-mode perl6-mode prog-mode "Perl6"
  "Major mode for editing Perl 6 code."
  ;; Syntaxification and font locking
  (setq-local syntax-propertize-function 'perl6-syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions 'syntax-propertize-multiline nil 'local)
  (setq-local font-lock-syntactic-face-function 'perl6-font-lock-syntactic-face)
  (setq-local font-lock-defaults '(perl6-font-lock-keywords nil nil))
  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (smie-setup perl6-smie-grammar #'perl6-smie-rules
              :forward-token #'perl6-smie--forward-token
              :backward-token #'perl6-smie--backward-token)

  )

(provide 'perl6-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; perl6-mode.el ends here
