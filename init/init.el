;; location variable
(cond
 ((setq os-windows-p (eq system-type 'windows-nt)))
 ((setq os-linux-p (eq system-type 'gnu/linux)))
 ((setq os-mac-p (eq-system-type 'darwin))))

;; language
(set-language-environment "utf-8")
(setq-default coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq-default buffer-coding-system 'utf-8)
(setq-default file-name-coding-system 'utf-8)
(setq-default senmail-coding-system 'utf-8)
(setq file-coding-system 'utf-8)
(setq sendmail-coding-system 'utf-8)
(setq terminal-coding-system 'utf-8)
(setq shell-coding-system 'utf-8)

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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

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
(global-set-key (kbd "C-c S") 'create-custom-snippet)        ;; create snippet of current mode
(global-set-key (kbd "C-c s") 'reload-current-mode-snippets) ;; loading all snippets
(global-set-key (kbd "C-c E")
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
(add-to-list 'load-path
             "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/elpa/yasnippet-20140314.255/snippets"
                         "~/.emacs.d/snippets"))
(add-hook 'html-mode-hook #'(lambda () (set (make-local-variable 'yas-extra-modes) 'web-mode)))
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
      (make-directory "~/.emacs.d/snippetes"))
  (if (not (file-directory-p (format "~/.emacs.d/snippets/%s" major-mode)))
      (make-directory (format "~/.emacs.d/snippets/%s" major-mode)))
  (let ((file-name (format "~/.emacs.d/snippets/%s/%s" major-mode snippet-name)))
    (if (file-exists-p file-name)
        (message "The snippet already exists")
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

;; lisp auto indent
;; (define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent) 
;; (define-key lisp-mode-map (kbd "RET") 'newline-and-indent)

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

;; autopair except where you are in interpreter such as slime
;; https://github.com/capitaomorte/autopair
(add-to-list 'load-path "~/.emacs.d/autopair")
(require 'autopair)
(autopair-global-mode)
(add-hook 'slime-repl-mode-hook
          #'(lambda ()
              (setq autopair-dont-activate t)
              (autopair-mode -1)))

;; IRC
(defun irc-start ()
   "Connect to IRC."
   (interactive)
   (erc :server "irc.freenode.net" :port 6667 :nick "anster" :full-name "razenrote")
   (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#clnoobs" "#lisp")))
   (setq erc-interpret-mirc-color t)
   (setq erc-hide-list '("JOIN" "PART" "QUIT")))

;; emmet : enhanced zencoding
;; https://github.com/smihica/emmet-mode
(add-to-list 'load-path "~/.emacs.d/emmet-mode")
(require 'emmet-mode)
(setq emmet-preview-default nil)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;; test area
(defun my-test-function (&optional file-name)
  (interactive)
  (message "%s" file-name))
  ;; (message "%s" (file-name-nondirectory buffer-file-name)))
(global-set-key (kbd "C-c p") 'my-test-function)

(defun test-minibuffer-function (text)
  (interactive "sEnter friend's name :")
  (message "Name: %s" test))

(defun get-current-major-mode ()
  (interactive)
  (message (concat "Hello" (format "%s" major-mode))))

