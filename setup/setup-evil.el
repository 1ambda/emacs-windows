(require 'evil)
(evil-mode 1)

;; use capslock as ESC
(setq w32-enable-caps-lock nil)
(global-set-key [capslock] 'evil-force-normal-state)

(defun set-mode-to-default-emacs (mode)
  (evil-set-initial-state mode 'emacs))

(mapcar 'set-mode-to-default-emacs
	'(eassist-mode
	  eshell))


;; evil-plugin: evil-nerd-commenter
(evilnc-default-hotkeys)

;; evil-plugin: evil-exchange
(require 'evil-exchange)
(evil-exchange-install)

;; evil-plugin: evil-leader
(global-evil-leader-mode)
;; evil-leader/set-key "k" 'kill-other-buffers)

;; evil-plugin : surround
(require 'surround)
(global-surround-mode 1)

;; evil-plugin : evil-nubmers
(require 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; evil-plugin : evil-matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; unmap key "M-." for ggtags. Use "." instead
(define-key evil-normal-state-map (kbd "M-.") nil)

;; unmap key "C-c p" for sr-speedbar toggle
(define-key evil-normal-state-map (kbd "C-c p") nil)
(define-key evil-insert-state-map (kbd "C-c p") nil)

;; unmap C-p, C-n for move cursor
(define-key evil-insert-state-map (kbd "C-p") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)

;; unmap C-w, C-y, C-d
(define-key evil-insert-state-map (kbd "C-w") 'kill-region)
(define-key evil-insert-state-map (kbd "C-y") 'yank)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)


(provide 'setup-evil)
