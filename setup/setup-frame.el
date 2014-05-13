;; Use frame-restore
(require 'frame-restore)
(frame-restore-mode)
(setq frame-restore-parameters-file
      (concat "~/.emacs.d/cache/frame/frame-restore-parameters"))

(provide 'setup-frame)
