(setq cache-directory (concat user-emacs-directory "cache/"))

;; Use debug
(setq debug-on-error t)

;; Setup Keyboard, Language
(set-language-environment "Korean")

;; Set locale as UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Disable toolbar, scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable backup, auto-save
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Set eshell, auto-save directory
(setq eshell-directory-name (concat cache-directory "eshell"))
(set 'auto-save-list-file-prefix (concat cache-directory "auto-save-list/"))

;; Enable line-number
(global-linum-mode t)

;; No start-up message
(setq inhibit-startup-message t)

;; Disable transient-mark-mode
(setq transient-mark-mode t)
(transient-mark-mode 1)

;; Enable delete-selection-mode
(delete-selection-mode 1)

;; Set cookie directory
(setq url-configuration-directory 
      (concat user-emacs-directory "cache/"))

;; Use operating system's trash
(setq delete-by-moving-to-trash t)

;; Set return key as newline and indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; Use key "C-c C-q" to open init.el
(global-set-key (kbd "C-c C-q") (lambda ()
				    (interactive)
				    (find-file "~/.emacs.d/init.el")))


(provide 'setup-default)
