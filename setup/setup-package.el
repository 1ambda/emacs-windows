(setq package-user-dir (concat user-emacs-directory "packages/"))
(setq gnu-dir (concat package-user-dir "archives/gnu"))
(setq marmalade-dir (concat package-user-dir "archives/marmalade"))
(setq melpa-dir (concat package-user-dir "archives/melpa"))

(require 'package)

;; Add package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; package-refresh-contents if archive directories do not exist
(unless (and
	 (file-exists-p gnu-dir)
	 (file-exists-p marmalade-dir)
	 (file-exists-p melpa-dir))
  (package-refresh-contents))

(defun packages-install (packages)
  (loop for package in packages do
	(when (not (package-installed-p package))
	  (package-install package)))
  (delete-other-windows))

(provide 'setup-package)
