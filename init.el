;; Set Directory Path
(setq dir/site-lisp "~/.emacs.d/site-lisp/"
      dir/setup "~/.emacs.d/setup/"
      dir/cache "~/.emacs.d/cache/")

(add-to-list 'load-path dir/site-lisp)
(add-to-list 'load-path dir/setup)

;; Use common-lisp
(require 'cl)

;; Setup site-lisp
(mapc 'require '(
		 setup-default
		 setup-package))

;; Install packages from MELPA
(defun init-packages ()
  (packages-install
   '(evil
     surround
     evil-numbers
     evil-matchit
     evil-leader
     evil-exchange
     evil-nerd-commenter
     ido-vertical-mode
     ido-at-point
     ido-ubiquitous
     flx-ido
     smex
     f
     s
     frame-restore
     iedit
     fill-column-indicator
     ace-jump-mode
     expand-region
     smart-forward
     )))

(condition-case nil
    (init-packages)
  (error
   (package-refresh-contents)
   (init-packages)))

;; Setup packages, builtin libraries
(mapc 'require '(
		 ;; site-lisp

		 ;; setup display
		 setup-font
		 setup-theme

		 ;; setup backup, restoring frame
		 setup-desktop
		 setup-revive-plus
		 setup-frame

		 ;; essential packages
		 setup-evil
		 setup-ido
		 setup-recentf
		 setup-smex
		 setup-uniquify
		 setup-windmove
		 setup-buffcycle

		 ;; editing packages
		 setup-iedit
		 setup-fci
	         setup-ace-jump-mode
		 setup-expand-region

		 ;; language

		 ))

;; Setup keybinding and alias
(require 'setup-alias)
(require 'setup-custom)

;; turn off debug option after loading emacs
(setq debug-on-error nil)
