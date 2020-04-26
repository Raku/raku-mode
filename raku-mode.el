;;; raku-mode.el --- Major mode for editing Raku code -*- lexical-binding: t; -*-

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

;; GNU Emacs 24 major mode for editing Raku code.

;; Currently only provides very basic syntax highlighting.

;;; Code:

(declare-function pkg-info-version-info "pkg-info" (library))

(defgroup raku nil
  "Major mode for editing Raku code."
  :prefix "raku-"
  :group 'languages)

(require 'raku-detect)
(require 'raku-font-lock)
(require 'raku-indent)
(require 'raku-imenu)
(require 'raku-repl)

(defvar raku-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'raku-send-line-to-repl)
    (define-key map (kbd "C-c C-r") 'raku-send-region-to-repl)
    (define-key map (kbd "C-c C-h") 'raku-send-buffer-to-repl)
    map)
  "Keymap for `raku-mode'.")

(easy-menu-define raku-mode-menu raku-mode-map
  "Menu for `raku-mode'"
  '("Raku"
    ["Send line to repl" raku-send-line-to-repl]
    ["Send region to repl" raku-send-region-to-repl]
    ["Send buffer to repl" raku-send-buffer-to-repl]))

;;;###autoload
(define-derived-mode raku-mode prog-mode "Raku"
  "Major mode for editing Raku code."
  ;; Syntaxification and font locking
  (setq-local syntax-propertize-function #'raku-syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions #'syntax-propertize-multiline nil 'local)
  (setq-local font-lock-syntactic-face-function #'raku-font-lock-syntactic-face)
  (setq-local font-lock-defaults '(raku-font-lock-keywords nil nil))
  ;; Add imenu support for raku-mode.  Note that imenu-generic-expression
  ;; is buffer-local, so we don't need a local-variable for it.
  (add-hook 'raku-mode-hook 'imenu-add-menubar-index)
  (setq imenu-generic-expression raku-imenu-generic-expression
      imenu-case-fold-search nil)
  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  ;; Indentation (see SMIE in the Emacs manual)
  ;; TODO add rules for HEREDOC indentation
  (smie-setup raku-smie-grammar #'raku-smie-rules
              :forward-token #'raku-smie--forward-token
              :backward-token #'raku-smie--backward-token)
  (use-local-map raku-mode-map))

(defalias 'perl6-mode 'raku-mode)

(provide 'raku-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; raku-mode.el ends here
