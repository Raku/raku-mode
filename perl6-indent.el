;;; perl6-indent.el --- Indentation support Perl 6 -*- lexical-binding: t; -*-

;;; Commentary:

;; SMIE grammar and various (auto-)indenting functions and variables are
;; defined here. Currently it's mostly borrowed from css-mode.

;;; Code:

(require 'smie)

(defconst perl6-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2 '((assoc ";") (assoc ",") (left ":")))))

(defcustom perl6-indent-offset 4
  "Basic size of one indentation step."
  :type 'integer
  :group 'perl6)

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

(provide 'perl6-indent)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; perl6-indent.el ends here
