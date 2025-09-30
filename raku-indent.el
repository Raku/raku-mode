;;; raku-indent.el --- Indentation support Raku -*- lexical-binding: t; -*-

;;; Commentary:

;; SMIE grammar and various (auto-)indenting functions and variables are
;; defined here. Currently it's mostly borrowed from css-mode.

;;; Code:

(require 'smie)

(defconst raku-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2
    '((assoc ";") (assoc "=") (assoc ",") (left ":") (left ".")))))

(defcustom raku-indent-offset 4
  "Basic size of one indentation step."
  :type 'integer
  :group 'raku)

(defun raku-smie--not-interpolation-p ()
  (save-excursion                                      ;; Prepare for excursion
    (forward-char -1)                                  ;; Retreat one char

    (or
     ;; Backtrack til we hit something that _isn't_ alnum
     ;; If we did not backtrack, we're not in interpolation
     (zerop (skip-chars-backward "-[:alnum:]"))

     ;; If we did backtrack, see if #{\$ (EOL) occurs three
     ;; or more characters prior to point (??????????)
     ;; if this does NOT match, we are not in an interpolation
     (not (looking-back "#{\\$" (- (point) 3))))))

(defun raku-smie--method-chain-p ()
  "Check if current line starts with a method call (dot operator)."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (eq (char-after) ?\.)))

(defun raku-smie--find-chain-root-indentation ()
  "Find the indentation of the root line in a method chain."
  (save-excursion
    ;; Move to previous line and scan backward to find the chain root
    (forward-line -1)
    (end-of-line)
    ;; Keep going back while we see lines starting with dots
    (while (and (not (bobp))
                (save-excursion
                  (beginning-of-line)
                  (skip-chars-forward " \t")
                  (eq (char-after) ?\.)))
      (forward-line -1)
      (end-of-line))
    ;; Now we're at the root line, return its indentation
    (beginning-of-line)
    (current-indentation)))

(defun raku-smie--forward-token ()
   (cond
   ;; Return `;` to fudge end-of-block indentation (I think), as ; is optional after a block
   ((and (eq (char-before) ?\})                 ;; Character immediately prior to point is `}`
         (raku-smie--not-interpolation-p)       ;; And, not in an interpolation
         ;; FIXME: If the next char is not whitespace, what should we do?
         (or (memq (char-after) '(?\s ?\t ?\n)) ;; And, point is followed by \s, \t, or \n
             (looking-at comment-start-skip)))  ;;      or point is looking-at /#+ */

    (if (memq (char-after) '(?\s ?\t ?\n))      ;; If the above is true, and point is followed by /[\s\t\n]/
        (forward-char 1) (forward-comment 1))   ;; Then, advance by one character, and one whole comment
    ";")

   ((eq (char-after) ?\=)                       ;; Spit out '=' to kick off proper indentation for hanging assignment
    (forward-char 1)
    "=")

   ((progn (forward-comment (point-max))        ;; Read past ALL comments
           (looking-at "[;,:.])"))              ;; Are we looking at ; , : or .

    (forward-char 1)                            ;; If so, advance one character
    (match-string 0))                           ;; And then return whatever looking-at found (?)

   (t (smie-default-forward-token))))           ;; If none of the above matched, defer to SMIE default search

(defun raku-smie--backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))               ;; Retreate past ALL comments up to point
    (cond
     ;; FIXME: If the next char is not whitespace, what should we do?
     ;; Cond #1 - Same end-of-block hack, I think
     ((and (eq (char-before) ?\})               ;; Point is preceded immediately by `}`
           (raku-smie--not-interpolation-p)     ;; And, not in an interpolation
           (> pos (point)))                     ;; And, point has moved backward

      ";")                                      ;; If so, yield ';'

     ((eq (char-before) ?\=)
      (forward-char -1)
      "=")

     ;; Cond #2 - Get whatever precedes [,:;.]
     ((memq (char-before) '(?\; ?\, ?\: ?\.))   ;; Point is preceded immediately by `;`, `,`, `:`, or `.`
      (forward-char -1)                         ;; Retreat one char
      (string (char-after)))                    ;; Return char after point (the char we just retreated past)

     (t (smie-default-backward-token)))))       ;; If none of the above matched, defer to SMIE default search

(defun raku-smie-rules (kind token)
  (pcase (cons kind token)
    ;; Basic indent offset
    (`(:elem . basic) raku-indent-offset)

    ;; Indent offset for function args
    (`(:elem . arg) 0)

    (`(:list-intro . ,(or `";" `"")) t) ;"" stands for BOB (bug#15467).

    ;; Make sure that hanging assignment gets indented
    (`(:before . "=")
     (if (smie-rule-hanging-p)
         (smie-rule-parent raku-indent-offset)))

    (`(:before . "{")
     (when (smie-rule-hanging-p) ; is `{` the last thing on this line?
       (smie-backward-sexp ";")  ; y tho
       (smie-indent-virtual)))

    (`(:before . ,(or "{" "("))
     (if (smie-rule-hanging-p)
         (smie-rule-parent 0)))

    ;; Method chaining indentation
    (`(:before . ".")
     (when (raku-smie--method-chain-p)
       (+ (raku-smie--find-chain-root-indentation) raku-indent-offset)))))

(provide 'raku-indent)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; raku-indent.el ends here
