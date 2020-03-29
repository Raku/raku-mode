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
(require 'perl6-indent)
(require 'perl6-imenu)
(require 'perl6-repl)

(defvar perl6-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'perl6-send-line-to-repl)
    (define-key map (kbd "C-c C-r") 'perl6-send-region-to-repl)
    (define-key map (kbd "C-c C-h") 'perl6-send-buffer-to-repl)
    map)
  "Keymap for `perl6-mode'.")

(easy-menu-define perl6-mode-menu perl6-mode-map
  "Menu for `perl6-mode'"
  '("Raku"
    ["Send line to repl" perl6-send-line-to-repl]
    ["Send region to repl" perl6-send-region-to-repl]
    ["Send buffer to repl" perl6-send-buffer-to-repl]))

;;;###autoload
(define-derived-mode perl6-mode prog-mode "Perl6"
  "Major mode for editing Perl 6 code."
  ;; Syntaxification and font locking
  (setq-local syntax-propertize-function #'perl6-syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions #'syntax-propertize-multiline nil 'local)
  (setq-local font-lock-syntactic-face-function #'perl6-font-lock-syntactic-face)
  (setq-local font-lock-defaults '(perl6-font-lock-keywords nil nil))
  ;; Add imenu support for perl6-mode.  Note that imenu-generic-expression
  ;; is buffer-local, so we don't need a local-variable for it.
  (add-hook 'perl6-mode-hook 'imenu-add-menubar-index)
  (setq imenu-generic-expression perl6-imenu-generic-expression
      imenu-case-fold-search nil)
  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
   ;; REPL
  (setq comint-prompt-regexp perl6-prompt-regexp)
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) perl6-prompt-regexp)
  ;; Indentation (see SMIE in the Emacs manual)
  ;; TODO add rules for HEREDOC indentation
  (smie-setup perl6-smie-grammar #'perl6-smie-rules
              :forward-token #'perl6-smie--forward-token
              :backward-token #'perl6-smie--backward-token)
  (use-local-map perl6-mode-map))



(provide 'perl6-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; perl6-mode.el ends here
