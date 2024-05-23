;;; raku-abbrev.el --- Abbreviations for Raku mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tim Van den Langenbergh

;; Author: Tim Van den Langenbergh <tmtvl@terra>
;; Keywords: abbrev, convenience, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Abbreviations for Raku mode, currently set to replace Texas ops,
;; <https://docs.raku.org/language/unicode_ascii>.

;;; Code:

(when (boundp 'raku-mode-abbrev-table)
  (clear-abbrev-table 'raku-mode-abbrev-table))

(define-abbrev-table 'raku-mode-abbrev-table
  '(
    ("pi" "π")
    ("tau" "τ")
    ("set()" "∅")
    ("(elem)" "∈")
    ("!(elem)" "∉")
    ("(cont)" "∋")
    ("!(cont)" "∌")
    ("Inf" "∞")
    ("(&)" "∩")
    ("(|)" "∪")))

(provide 'raku-abbrev)
;;; raku-abbrev.el ends here
