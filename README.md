Emacs 24.3 on Windows 7, Ubuntu 13.10
===

2014.03.24

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
- windmove
- windows
- revive
- yasnippet
- markdown-mode
- js3-mode
- smartparens
- helm (previously known for anything.el)
- xcscope




### 3. OS dependent config
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
```

By. Anster
