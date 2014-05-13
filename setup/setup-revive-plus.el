(add-to-list 'load-path "~/.emacs.d/site-lisp/windows")
(add-to-list 'load-path "~/.emacs.d/site-lisp/revive-plus")

(require 'revive)
(require 'revive+)
(setq revive-plus:last-wconf-file
      "~/.emacs.d/cache/revive/last-wconf")
(setq revive-plus:all-frames t)
(revive-plus:demo)

(provide 'setup-revive-plus)
