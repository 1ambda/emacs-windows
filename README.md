Emacs 24.3 on Windows
===

Last Update - 2014.03.24

By. Anster
<br>

### 1. How to use

git clone https://github.com/ansterd/emacs  
git submodule init  
git submodule update
```
(load "~/.emacs.d/init/init.el") ;; add to this line in yourr ~/.emacs
```

### 2. Contents

Support C/C++, Lisp, Javascript/Node.js, HTML/CSS

TODO : Java, Python

- cedet 1.1
- ecb 2.40
- evil
- auto-complete
- ac-slime
- zenburn theme
- emmet-mode
- web-mode
- ido
	* ido-at-point
	* ido-vertical-mode
	* flx-ido
	* ido-goto-symbol
- expand-region
- ace-jump-mode
- windmove
- windows
- revive
- yasnippet
- markdown-mode
- js3-mode
- smartparens
- helm (previously known for anything.el)
- xcscope
- auto-complete-c-headers
- auto-complete-clang
- iedit
- flymake
- powerline
- header2
- member-functions
- windcycle
- buffcycle


### 3. OS dependent config

In case of Windows, You need

- Git bas
- slime, sbcl
- cygwin

```
(cond
 ((setq os-windows-p (eq system-type 'windows-nt)))
 ((setq os-linux-p (eq system-type 'gnu/linux)))
 ((setq os-mac-p (eq-system-type 'darwin))))
 
;; f7 git shell on windows
(when os-windows-p
  (global-set-key [(f7)]
                  '(lambda ()
                     (interactive)
                     (w32-shell-execute "open"
					"C:\\Program Files (x86)\\Git\\bin\\sh.exe"
					"--login -i"))))
					
;; sbcl, slime
(cond (os-windows-p (progn (setq inferior-lisp-program "C:/lisp/sbcl/sbcl.exe")
			                     (add-to-list 'load-path "C:/lisp/slime")))
      (os-linux-p (progn (setq inferior-lisp-program "sbcl")
			                   (load (expand-file-name "~/quicklisp/slime-helper.el")))))	
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
```
