(require 'ace-jump-mode)

;  ace-jump-mode : [C-c SPC]
;; ace-jump-char : [C-u SPC]
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(provide 'setup-ace-jump-mode)
