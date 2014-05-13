;; recentf with ido
(require 'ido)
(require 'recentf)

;; recentf option
(setq recentf-max-menu-items 25)

;; exclude list
;; (setq recentf-exclude '(
;; 			"bookmarks"
;; 			))

(add-to-list 'recentf-exclude "bookmarks")
(add-to-list 'recentf-exclude ".revive.el")
(add-to-list 'recentf-exclude ".recentf")
(add-to-list 'recentf-exclude ".windows")
(add-to-list 'recentf-exclude ".emacs-bmk-bmenu-state.el")
(add-to-list 'recentf-exclude "org-loaddefs.el")
(add-to-list 'recentf-exclude "org-autoloads.el")
(add-to-list 'recentf-exclude "org$")
(add-to-list 'recentf-exclude "TAGS")

;; find recent file using ido
(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

(recentf-mode 1)

;;;
;;; custom function
;;;

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
	  (mapcar (lambda (x)
		    (cons (file-name-nondirectory x)
			  x))
		  recentf-list))
	 (filename-list
	  (remove-duplicates (mapcar #'car file-assoc-list)
			     :test #'string=))
	 (filename (ido-completing-read "Choose recent file: "
					filename-list
					nil
					t)))
    (when filename
      (find-file (cdr (assoc filename
			     file-assoc-list))))))


(provide 'setup-recentf)
