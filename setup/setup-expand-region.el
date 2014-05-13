(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'smart-forward)
(global-set-key (kbd "M--") 'smart-up)
(global-set-key (kbd "M-=") 'smart-down)

(provide 'setup-expand-region)
