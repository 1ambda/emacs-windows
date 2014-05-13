(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-flex-matching t
      ido-enable-prefix nil
      ido-case-fold t
      ido-max-prospects 10
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-enable-tramp-completion nil
      ffap-require-prefix t
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

(global-set-key (kbd "C-c i") 'ido-goto-symbol)
(define-key ido-buffer-completion-map (kbd "<down>") 'ido-next-match)
(define-key ido-buffer-completion-map (kbd "<up>") 'ido-prev-match)
(define-key ido-file-completion-map (kbd "<down>") 'ido-next-match)
(define-key ido-file-completion-map (kbd "<up>") 'ido-prev-match)

(defun ido-imenu ()
  "Query with `ido-completing-read' a symbol in the buffer's
imenu index, then jump to that symbol's location."
  (interactive)
  (goto-char
   (let ((lst (nreverse
               (flatten-assoc-tree
                (imenu--make-index-alist)
                'imenu--subalist-p))))
     (access (ido-completing-read "Symbol: " (mapcar 'car lst)) lst))))

;; vertical mode
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;; ido at point mode [C-M-i]
(require 'ido-at-point)

;; flx-ido
(require 'flx-ido)
(flx-ido-mode 1)

;; ignore list
(add-to-list 'ido-ignore-buffers "*IBuffer*")
(add-to-list 'ido-ignore-buffers "^\*CEDET")
(add-to-list 'ido-ignore-buffers "^\*SPEEDBAR")
(add-to-list 'ido-ignore-buffers "\.org$")
(add-to-list 'ido-ignore-buffers "^\*Flycheck")
(add-to-list 'ido-ignore-buffers "^\*ggtags")
(add-to-list 'ido-ignore-buffers "^\*helm")
(add-to-list 'ido-ignore-buffers "^\*Compile-Log")

;; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

;;;
;;; custom-function
;;;


(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
	  (ido-enable-flex-matching
	   (if (boundp 'ido-enable-flex-matching)
	       ido-enable-flex-matching t))
	  name-and-pos symbol-names position)
      (unless ido-mode
	(ido-mode 1)
	(setq ido-enable-flex-matching t))
      (while (progn
	       (imenu--cleanup)
	       (setq imenu--index-alist nil)
	       (ido-goto-symbol (imenu--make-index-alist))
	       (setq selected-symbol
		     (ido-completing-read "Symbol? " symbol-names))
	       (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
	(push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
	(goto-char (overlay-start position)))
       (t
	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
	(cond
	 ((and (listp symbol) (imenu--subalist-p symbol))
	  (ido-goto-symbol symbol))
	 ((listp symbol)
	  (setq name (car symbol))
	  (setq position (cdr symbol)))
	 ((stringp symbol)
	  (setq name symbol)
	  (setq position
		(get-text-property 1 'org-imenu-marker symbol))))
	(unless (or (null position) (null name)
		    (string= (car imenu--rescan-item) name))
	  (add-to-list 'symbol-names name)
	  (add-to-list 'name-and-pos (cons name position))))))))

(defun ido-sort-mtime ()
  (setq ido-temp-list
	(sort ido-temp-list 
	      (lambda (a b)
		(time-less-p
		 (sixth (file-attributes (concat ido-current-directory b)))
		 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
	      (lambda (x) (and (char-equal (string-to-char x) ?.) x))
	      ido-temp-list))))

(provide 'setup-ido)
