(require 'fill-column-indicator)
(setq fci-rule-width 2)
(define-globalized-minor-mode
 global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode t)
(setq fci-rule-color "cadet blue")

(provide 'setup-fci)
