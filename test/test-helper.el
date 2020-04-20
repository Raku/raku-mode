;;; test-helper.el --- Raku Mode: Non-interactive unit-test setup  -*- lexical-binding: t; -*-

;;; Commentary:

;; Non-interactive test suite setup for ERT Runner.

;;; Code:

(message "Running tests on Emacs %s" emacs-version)

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Do not load outdated byte code for tests
       (load-prefer-newer t))
  ;; Load the file under test
  (add-to-list 'load-path source-directory)
  (load (expand-file-name "raku-mode")))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; test-helper.el ends here
