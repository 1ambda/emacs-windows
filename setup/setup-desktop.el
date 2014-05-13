;; Use desktop
(require 'desktop)
(desktop-save-mode 1)

;; Set desktop path, file name
(setq desktop-save-path "~/.emacs.d/cache/desktop/")
(setq desktop-path `(,desktop-save-path))
(setq desktop-dirname desktop-save-path)
(setq desktop-base-file-name ".emacs.desktop")

;; Save desktop automatically
(defun desktop/auto-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'desktop/auto-save)

(provide 'setup-desktop)


