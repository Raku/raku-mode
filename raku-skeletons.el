;;; raku-skeletons.el --- Skeletons for Raku file auto insertion.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tim Van den Langenbergh

;; Author: Tim Van den Langenbergh <tmt_vdl@gmx.com>
;; URL: https://github.com/raku/raku-mode
;; Keywords: languages, convenience, files
;; Version: 0.1

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

;; Some skeletons for auto-insertion in Raku files.

;;; Code:
(require 'raku-mode)

(defgroup raku-skeletons nil
  "Skeletons for Raku files."
  :prefix "raku-skeleton-"
  :group 'raku)

(defcustom full-raku-path "/usr/bin/raku"
  "Path to the Raku executable."
  :type 'string
  :group 'raku-skeletons)

(defcustom auth-id user-login-name
  "Author information."
  :type 'string
  :group 'raku-skeletons)

(define-skeleton raku-script-skeleton
  "Skeleton for Raku scripts."
  nil
  "#!" (progn full-raku-path) \n
  "use v6;" \n
  \n
  "sub MAIN () {" \n
  > _ \n
  "}" \n)

(defvar module-name "Foo"
  "Variable for holding a new module name.")

(define-skeleton raku-module-skeleton
  "Skeleton for Raku modules."
  nil
  "use v6;" \n
  \n
  "unit module "
  (let ((given-name (skeleton-read "Module name: " module-name)))
    (setq module-name given-name)
    given-name)
  ":ver<0.0.1>:auth<" (progn auth-id) ">;" \n
  \n
  _ \n
  \n
  "=begin pod" \n
  \n
  "=head1 NAME" \n
  \n
  (progn module-name) " - " (skeleton-read "Short description: ") \n
  \n
  "=head1 SYNOPSIS" \n
  \n
  "\tuse " (progn module-name) ";\n"
  \n
  "=head1 DESCRIPTION" \n
  \n
  (progn module-name) " is..." \n
  \n
  "=head1 AUTHOR" \n
  \n
  (progn auth-id) \n
  \n
  "=head1 COPYRIGHT AND LICENSE" \n
  \n
  "This library is free software; "
  "you can redistribute it and/or modify it under the Artistic License 2.0."
  "\n\n"
  "=end pod" \n)

(provide 'raku-skeletons)
;;; raku-skeletons.el ends here
