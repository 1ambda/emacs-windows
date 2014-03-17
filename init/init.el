;; location variable
(defvar *os-platform* 'nil)
(cond 
    ((eq system-type 'darwin) (setf *os-platform* "osx"))
    ((eq system-type 'windows-nt) (setf *os-platform* "windows"))
    ((eq system-type 'gnu/linux) (setf *os-platform* "linux")))

;; language
(set-language-environment "utf-8")

;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

;; initialize
(iswitchb-mode)
(setq current-language-environment "UTF-8") ;; set UTF-8
(global-linum-mode t) ;; line-number
(setq make-backup-files nil) ;; no backup file
(setq auto-save-default nil) ;; no auto save
(global-auto-revert-mode 1) ;; auto load
(electric-pair-mode) ;; auto pair

;; custom key setting
(defun reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs"))
(global-set-key (kbd "<f6>") 'reload-emacs-config)
(global-set-key (kbd "<f8>") "hello insert function")
(global-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
(global-set-key [(control shift return)] 'semantic-ia-complete-tip)
(global-set-key [(control c)(control return)] 'semantic-complete-analyze-inline)
(global-set-key [(shift return)] 'semantic-analyze-possible-completions)
;; f7 git shell on windows
(when (eq system-type 'windows-nt)
  (global-set-key [(f7)]
                  '(lambda ()
                     (interactive)
                     (w32-shell-execute "open"
					"C:\\Program Files (x86)\\Git\\bin\\sh.exe"
					"--login -i"))))
;; C-/ for eclipse-like comment
(defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
	(end (line-end-position)))
    (when (region-active-p)
      (setq start (save-excursion
		    (goto-char (region-beginning))
		    (beginning-of-line)
		    (point))
	    end (save-excursion
		  (goto-char (region-end))
		  (end-of-line)
		  (point))))
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "C-/") 'comment-eclipse)


;; custom function
(defun kill-other-buffers ()
  "Kill all other buffers"
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer)
	      (remove-if-not 'buffer-file-name (buffer-list)))))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; cedet
(load "~/.emacs.d/cedet-1.1/cedet-devel-load.el")
;; (load "~/.emacs.d/cedet-1.1/contrib/cedet-contrib-load.el")
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)

(semantic-mode 1)
(require 'semantic/ia)

;; cedet semantic
(semantic-load-enable-excessive-code-helpers)
(global-semantic-show-parser-state-mode 1)
;; (setq semantic-python-dependency-system-include-path
;;      '("/usr/lib/python2.7/")) 
(global-semanticdb-minor-mode 1)
(setq semanticdb-default-system-save-directory
      (setq semanticdb-default-save-directory "~/.emacs.d/semanticdb"))

;; ecb
(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(require 'ecb)
;; customize the keys for ECB
(define-key ecb-mode-map (kbd "M-1") 'ecb-goto-window-directories)
(define-key ecb-mode-map (kbd "M-2") 'ecb-goto-window-sources)
(define-key ecb-mode-map (kbd "M-3") 'ecb-goto-window-methods)
(define-key ecb-mode-map (kbd "M-4") 'ecb-goto-window-history)
(define-key ecb-mode-map (kbd "M-5") 'ecb-goto-window-compilation)
(define-key ecb-mode-map (kbd "M-0") 'ecb-goto-window-edit1)

;; windows
(desktop-save-mode 1)
(desktop-read)
(add-to-list 'load-path "~/.emacs.d/windows")
(require 'windows)
(win:startup-with-window)
;; (define-key ctl-x-map "C" 'see-you-again)

;; evil
(add-to-list 'load-path "~/.emacs.d/evil") ;;no need with 24
(require 'evil)
(evil-mode 1)

;; ido
(require 'ido)
(ido-mode t)

;; windmove
(windmove-default-keybindings 'meta)

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(require 'tomorrow-night-theme)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; auto-complete
;; http://seorenn.blogspot.kr/2011/03/emacs-auto-complete-mode.html
;; install : http://probongster.blogspot.kr/2014/02/emacs_10.html
;; (add-to-list 'load-path "~/.emacs.d/cl-lib")
;; (require 'cl-lib)
(add-to-list 'load-path "~/.emacs.d/popup-el")
(add-to-list 'load-path "~/.emacs.d/auto-complete")  
(require 'auto-complete-config)
(ac-config-default)  
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")  

;; sbcl, slime
(setq inferior-lisp-program "D:/lisp/sbcl/sbcl.exe")
(add-to-list 'load-path "D:/lisp/slime")
(require 'slime)
(slime-setup '(slime-repl))

;; lisp auto indent
(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)
(define-key lisp-mode-map (kbd "RET") 'newline-and-indent)

;; lisp compile key
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html
(defun save-lisp-file ()
  (interactive)
  (save-buffer)
  (defvar lisp-file-name nil)
  (setq lisp-file-name buffer-file-name))

(define-key lisp-mode-map (kbd "<f5>") 'save-lisp-file)
(define-key emacs-lisp-mode-map (kbd "<f5>") 'save-lisp-file)
(define-key slime-repl-mode-map  (kbd "<f5>") (lambda ()
						(interactive)
						(insert
						 (format "(load \"%s\")" lisp-file-name))))

;; ac-slime
(add-to-list 'load-path "~/.emacs.d/ac-slime")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; hyperspec
;; (add-to-list 'load-path "~/.emacs.d/hyperspec")

;; markdown
;; http://jblevins.org/projects/markdown-mode/
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; slime custom key binding
(add-hook 'markdown-mode-hook '(lambda ()
			     (interactive)
			     (message "slime")
			     (define-key markdown-mode-map (kbd "<f8>") 'reload-emacs-config)))

;; speedbar toggle
;; http://seorenn.blogspot.kr/2012/09/emacs-speedbar-sr-speedbarel.html
(add-to-list 'load-path "~/.emacs.d/sr-speedbar")
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-auto-refresh t)
(defun my-speedbar ()
  "Toggle sr-speedbar and select"
  (interactive)
  (progn
    (if (sr-speedbar-exist-p)
	(sr-speedbar-refresh))
    (sr-speedbar-toggle)
    (if (sr-speedbar-exist-p)
	(sr-speedbar-select-window))))

(global-set-key (kbd "C-x p") 'my-speedbar)

;; js3-mode
(add-to-list 'ac-modes 'js3-mode)
(setq js3-lazy-commas t)
(setq js3-lazy-operators t)
(setq js3-lazy-dots t)
(setq js3-expr-indent-offset 4)
(setq js3-paren-indent-offset 4)
(setq js3-square-indent-offset 4)
(setq js3-curly-indent-offset 4)
(setq js3-auto-indent-p t)
(setq js3-enter-indents-newline t)
(setq js3-indent-on-enter-key t)

;; web-mode.el
;; http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
