;; Provide a menu bar item to ease insertion of Unicode characters.


;; Make a menu keymap (with a prompt string)
;; and make it the menu bar item’s definition.
;; put it at the end
(define-key global-map [menu-bar unicode]
  (cons "Unicode" (make-sparse-keymap "Unicode")))
;; Define specific subcommands in this menu.
(define-key global-map
  [menu-bar unicode forward]
  '("Forward word" . forward-word))
(define-key global-map
  [menu-bar unicode backward]
  '("Backward word" . backward-word))
(defvar menu-bar-final-items '(help-menu unicode-menu)) ;; doesn't work??





;;===========================
(provide 'raku-unicode-menu)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; raku-imenu.el ends here
