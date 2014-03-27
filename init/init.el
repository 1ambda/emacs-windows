;; location variable
(cond
 ((setq os-windows-p (eq system-type 'windows-nt)))
 ((setq os-linux-p (eq system-type 'gnu/linux)))
 ((setq os-mac-p (eq-system-type 'darwin))))

;; language
;; http://terzeron.net/wiki/doku.php?id=emacs_%EC%84%A4%EC%A0%95
(when enable-multibyte-characters
  (set-language-environment "Korean")
  (setq locale-value 
        (if (string= (getenv "LANG") "ko_KR.utf8") 'utf-8 'euc-kr))
  (prefer-coding-system locale-value)
  (set-default-coding-systems locale-value)
  (setq-default file-name-coding-system locale-value)
  (setq-default locale-coding-system locale-value)
  (set-terminal-coding-system locale-value)
  (set-keyboard-coding-system locale-value)
  (set-selection-coding-system locale-value)
  (setq-default buffer-file-coding-system locale-value)
  (setq-default buffer-coding-system locale-value)
  (setq file-coding-system locale-value)
  (setq terminal-coding-system locale-value)
  (setq shell-coding-system locale-value)
)
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
(global-linum-mode t) ;; line-number
(setq make-backup-files nil) ;; no backup file
(setq auto-save-default nil) ;; no auto save
(global-auto-revert-mode 1) ;; auto load
(electric-indent-mode 1) ;; auto indent
(defalias 'yes-or-no-p 'y-or-n-p) ;; convert yes-or-no-p into y-or-n-p
;; (global-hl-line-mode 1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))



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
(when os-windows-p
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

;; custom key setting using "C-c"
;; use C-c C-c instead of M-x. It would be easier.
(global-set-key (kbd "C-c C-c") 'execute-extended-command)
(global-set-key (kbd "C-c S") 'create-custom-snippet)        ;; create snippet of current mode
(global-set-key (kbd "C-c R") 'reload-current-mode-snippets) ;; loading all snippets
(global-set-key (kbd "C-c e")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/init/init.el")))

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
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)



;; cedet semantic
(semantic-mode 1)
(require 'semantic/ia)
(semantic-load-enable-excessive-code-helpers)
(global-semantic-show-parser-state-mode 1)
;; (setq semantic-python-dependency-system-include-path
;;      '("/usr/lib/python2.7/")) 
(global-semanticdb-minor-mode 1)
;; (global-semantic-tag-folding-mode 1)
(setq semanticdb-default-system-save-directory
      (setq semanticdb-default-save-directory "~/.emacs.d/semanticdb"))

;; ecb
(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(require 'ecb)
;; customize the keys for ECB
(setq ecb-tip-of-the-day nil)
(define-key ecb-mode-map (kbd "M-1") 'ecb-goto-window-directories)
(define-key ecb-mode-map (kbd "M-2") 'ecb-goto-window-sources)
(define-key ecb-mode-map (kbd "M-3") 'ecb-goto-window-methods)
(define-key ecb-mode-map (kbd "M-4") 'ecb-goto-window-history)
(define-key ecb-mode-map (kbd "M-5") 'ecb-goto-window-compilation)
(define-key ecb-mode-map (kbd "M-0") 'ecb-goto-window-edit1)
(define-key ecb-mode-map (kbd "C-c w") 'ecb-toggle-ecb-windows)


(require 'cc-mode)
(define-key c++-mode-map (kbd "C-c C-c") 'execute-extended-command)
(define-key c-mode-map (kbd "C-c C-c") 'execute-extended-command)
(define-key c++-mode-map (kbd "C-c c") 'compile)
(define-key c-mode-map (kbd "C-c c") 'compile)
(setq-default c-basic-offset 4)
(setq c-default-style 
      '((java-mode . "java") (c++-mode . "stroustrup") (other . "k&r")))
(require 'xcscope)
(cscope-setup)
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))


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
(require 'tomorrow-night-bright-theme)



;; yasnippet
(add-to-list 'load-path
             "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/elpa/yasnippet-20140314.255/snippets"
                         "~/.emacs.d/snippets"))
;; (add-hook 'html-mode-hook #'(lambda () (set (make-local-variable 'yas-extra-modes) 'web-mode)))
(yas-global-mode 1)
(yas-reload-all 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "S-<tab>") 'yas-expand)
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))
(define-key yas-minor-mode-map (kbd "<C-tab>")     'yas-ido-expand)
;; create my custom snippet in "~/.emacs.d/snippets" based on major mode
;; key binding : C-c S
(setq *new-snippet-content*
"# name: 
# key: 
# --
")

(defun create-custom-snippet (snippet-name)
  (interactive "sSnippet name : ")
  (if (not (file-directory-p "~/.emacs.d/snippets"))
      (make-directory "~/.emacs.d/snippets"))
  (if (not (file-directory-p (format "~/.emacs.d/snippets/%s" major-mode)))
      (make-directory (format "~/.emacs.d/snippets/%s" major-mode)))
  (let ((file-name (format "~/.emacs.d/snippets/%s/%s" major-mode snippet-name)))
    (if (file-exists-p file-name)
        (progn (message "The snippet already exists")
               (switch-to-buffer (find-file-noselect file-name)))
      (progn (write-region *new-snippet-content* nil file-name)
             (switch-to-buffer (find-file-noselect file-name))))))

;; load all snippets 
;; key binding : C-c s
(defun reload-current-mode-snippets ()
  (interactive)
  (yas-reload-all))

;; auto-complete
;; http://seorenn.blogspot.kr/2011/03/emacs-auto-complete-mode.html
;; install : http://probongster.blogspot.kr/2014/02/emacs_10.html
(add-to-list 'load-path "~/.emacs.d/popup-el")
(add-to-list 'load-path "~/.emacs.d/auto-complete")  
(require 'auto-complete-config)
(ac-config-default)  
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")

;; sbcl, slime
(cond (os-windows-p (progn (setq inferior-lisp-program "C:/lisp/sbcl/sbcl.exe")
			   (add-to-list 'load-path "C:/lisp/slime")))
      (os-linux-p (progn (setq inferior-lisp-program "sbcl")
			 (load (expand-file-name "~/quicklisp/slime-helper.el")))))

(require 'slime)
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-repl))

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
(setq markdown-command "perl ~/.emacs.d/markdown/Markdown.pl"
      markdown-content-type "text/html"
      markdown-css-path "style.css"
      markdown-coding-system 'utf-8)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "M-<up>") nil)
            (define-key markdown-mode-map (kbd "M-<down>") nil)
            (define-key markdown-mode-map (kbd "M-<left>") nil)
            (define-key markdown-mode-map (kbd "M-<right>") nil)))
;; save exported HTML into temp directory
(defadvice markdown-export (around set-temp-path-for-exported-file activate)
  (ad-set-arg 0 (format "%s/%s" "~/.emacs.d/temp-dir" (file-name-nondirectory buffer-file-name)))
  ad-do-it)

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
(custom-set-variables
 '(js3-lazy-operators t)
 '(js3-lazy-commas t)
 '(js3-lazy-dots t)
 '(js3-curly-indent-offset 2)
 '(js3-expr-indent-offset 2)
 '(js3-paren-indent-offset 2)
 '(js3-square-indent-offset 2)
 '(js3-auto-indent-p t)
 '(js3-enter-indents-newline t)
 '(js3-indent-on-enter-key t)
 '(js3-consistent-level-indent-inner-bracket t))

;; web-mode.el
;; http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\html\\'" . web-mode))
(add-hook 'web-mode-hook (lambda ()
                           (interactive)
                           (progn
                             (setq web-mode-markup-indent-offset 4)
                             (setq web-mode-css-indent-offset 4)
                             (setq web-mode-code-indent-offset 4))))

;; smartparens
(add-to-list 'load-path "~/.emacs.d/smartparens")
(require 'smartparens-config)
(smartparens-global-mode t)
(add-hook 'slime-repl-mode-hook
          #'(lambda ()
              (smartparens-mode -1)))

;; IRC
(defun irc-start ()
   "Connect to IRC."
   (interactive)
   (erc :server "irc.freenode.net" :port 6667 :nick "anster" :full-name "razenrote")
   (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#lisp")))
   (setq erc-interpret-mirc-color t)
   (setq erc-hide-list '("JOIN" "PART" "QUIT")))

;; emmet : enhanced zencoding
;; https://github.com/smihica/emmet-mode
(add-to-list 'load-path "~/.emacs.d/emmet-mode")
(require 'emmet-mode)
(setq emmet-preview-default nil)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;; font face
(set-face-foreground 'highlight "eaeaea")
(set-face-background 'highlight "424242")

;; tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; google drive sync
(defvar google-drive-installed-p nil)
(if (file-directory-p "~/Google")
    (setq google-drive-installed-p t))

;; load TODO in google drive after initializing Emacs
(add-hook 'after-init-hook
          (lambda ()
            (if google-drive-installed-p
                (switch-to-buffer (find-file "~/Google/TODO")))))



;; use cygwin bash instead os MS-DOS on windows
(if os-windows-p
    (progn
      (add-to-list 'load-path "~/.emacs.d/cygwin-mount")
      (setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
      (setq exec-path (cons "c:/cygwin/bin/" exec-path))
      (require 'cygwin-mount)
      (cygwin-mount-activate)
      
      (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
      (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)
      (setq explicit-shell-file-name "bash.exe")
      (setq shell-file-name explicit-shell-file-name)
      (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
      (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)))
  
;; C, C++ Development Env 
;; in case of C, C++ mode We have to override the key binding
(add-hook 'eshell-mode-hook 
          (lambda ()
            (define-key eshell-mode-map (kbd "C-c C-c") 'execute-extended-command)))
(define-key compilation-mode-map (kbd "C-c C-c") 'execute-extended-command)



;; xcscope
;; https://github.com/dkogan/xcscope.el
;; TODO : http://www.emacswiki.org/emacs/CScopeAndEmacs
;; (add-to-list 'load-path "~/.emacs.d/xcscope.el")
;; (add-hook 'c-mode-hook 'cscope-minor-mode)
;; (add-hook 'c++-mode-hook 'cscope-minor-mode)

;; helm
;; TODO : https://github.com/jixiuf/helm-etags-plus
;; (add-to-list 'load-path "~/.emacs.d/helm")
;; (require 'helm-config)

;; ctags config
;; TODO : http://www.emacswiki.org/emacs/BuildTags
;; TODO : [etags shortcut]
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Tag.html#Find-Tag

;; test area


;; windows
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(add-to-list 'load-path "~/.emacs.d/windows")
(require 'windows)
(win:startup-with-window)
