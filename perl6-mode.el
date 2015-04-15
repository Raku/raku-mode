;;; perl6-mode.el --- Major mode for editing Perl 6 code -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hinrik Örn Sigurðsson <hinrik.sig@gmail.com>

;; Author: Hinrik Örn Sigurðsson <hinrik.sig@gmail.com>
;; URL: https://github.com/hinrik/perl6-mode
;; Keywords: languages
;; Version: 0.1-git
;; Package-Requires: ((emacs "24.3") (pkg-info "0.1"))

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

(require 'perl6-font-lock)

;;;###autoload
(define-derived-mode perl6-mode prog-mode "Perl6"
  "Major mode for editing Perl 6 code."
  ;; Syntaxification and font locking
  (setq-local syntax-propertize-function 'perl6-syntax-propertize)
  (setq-local font-lock-syntactic-face-function 'perl6-font-lock-syntactic-face)
  (setq-local font-lock-defaults '(perl6-font-lock-keywords nil nil))
  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-use-syntax t)
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("perl6" . perl6-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.p[lm]?6\\'" . perl6-mode))

(defvar perl6-magic-pattern
  (rx line-start
      (0+ space)
      (or (and "use" (0+ space) "v6")
          (and (opt (and (or "my" "our") (0+ space)))
               (or "module" "class" "role" "grammar")))))

(defun perl6-magic-matcher ()
  "Return non-nil if the current buffer is probably a Perl 6 file."
  (let ((case-fold-search nil)
        (keep-going t)
        (found-perl6 nil))
    (while keep-going
      (cond ((looking-at "^ *\\(?:#.*\\)?$")
             nil)
            ((looking-at perl6-magic-pattern)
             (setq keep-going nil
                   found-perl6 t))
            (t
             (setq keep-going nil)))
      (beginning-of-line 2))
    found-perl6))

;;;###autoload
(add-to-list 'magic-mode-alist '(perl6-magic-matcher . perl6-mode))

(provide 'perl6-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; perl6-mode.el ends here
