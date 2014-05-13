(setq buffcycle-directory 
      "~/.emacs.d/site-lisp/buffcycle") 
(add-to-list 'load-path buffcycle-directory)

(require 'buffcycle)
;; (global-unset-key (kbd "C-x x"))
;; (defun close-and-kill-this-pane ()
;;       "If there are multiple windows, then close this pane and kill the buffer in it also."
;;       (interactive)
;;       (kill-this-buffer)
;;       (if (not (one-window-p))
;;           (delete-window)))
;; (global-set-key (kbd "C-x x") 'close-and-kill-this-pane)

(provide 'setup-buffcycle)
