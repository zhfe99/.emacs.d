;;; package --- Summary
;;; Commentary:
;;; Code:

;; ace-mode
(global-ace-isearch-mode +1)
(setq ace-isearch-use-jump nil)

;; turn-off beacon
(beacon-mode -1)

;; turn-off which-function-mode. Otherwise open cython (.pyx, .pyd) become very slow
(which-function-mode -1)

;; search with selected region
(defun jrh-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'jrh-isearch-with-region)

;; revert buffer without confirmation
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; insert double space
(defun my-insert-double-space ()
  "Insert space so that a|bc -> a bc |."
  (interactive)
  (insert " ")
  (forward-char 2)
  (insert " "))

;; insert single space
(defun my-insert-single-space ()
  "Insert space so that a|bc -> a b |c."
  (interactive)
  (insert " ")
  (forward-char 1)
  (insert " "))

;; dash for doc
(prelude-require-package 'dash-at-point)
(add-to-list 'dash-at-point-mode-alist
             '(python-mode . "python2,numpy,django,sqlalchemy,numpy,scipy"))
(add-to-list 'dash-at-point-mode-alist '(sql-mode . "PASCAL"))

(defun my-string-starts-with (s begins)
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

;; get current file on server or local
(defun my-get-current-file-on-server-or-local()
  "Get current file on server or local."
  (interactive)
  (if (my-string-starts-with (buffer-file-name) "/scp:")
      (progn
        (setq my-buffer-file-name (substring (buffer-file-name) (length "/scp:feng@skyserver3k:/home/ma/feng/") nil))
        (setq my-buffer-file-name1 (concat "/Users/feng/" my-buffer-file-name))
        (find-file my-buffer-file-name1))
    (progn
      (setq my-buffer-file-name (substring (buffer-file-name) (length "/Users/feng/") nil))
      (setq my-buffer-file-name1 (concat "/scp:feng@skyserver3k:/home/ma/feng/" my-buffer-file-name))
      (find-file my-buffer-file-name1)
      )))

;; get current fold on server or local
(defun my-get-current-dired-on-server-or-local()
  "Get current fold on server or local."
  (interactive)
  (if (my-string-starts-with dired-directory "/scp:")
      (progn
        (setq my-buffer-file-name (substring dired-directory (length "/scp:feng@skyserver3k:/home/ma/feng/") nil))
        (setq my-buffer-file-name1 (concat "/Users/feng/" my-buffer-file-name))
        (message dired-directory)
        (message my-buffer-file-name1)
        (find-file my-buffer-file-name1))
    (progn
      (setq my-buffer-file-name (substring dired-directory (length "~/") nil))
      (setq my-buffer-file-name1 (concat "/scp:feng@skyserver3k:/home/ma/feng/" my-buffer-file-name))
      (message dired-directory)
      (message my-buffer-file-name1)
      (find-file my-buffer-file-name1)
      )))

;; get current fold on server or local
(defun my-get-current-on-server-or-local()
  "Get current fold on server or local."
  (interactive)
  (if (string= major-mode "dired-mode")
      (my-get-current-dired-on-server-or-local)
    (my-get-current-file-on-server-or-local)))

(defun my-align-comment()
  (interactive)
  (align-regexp
   (region-beginning)
   (region-end)
   (concat "\\(\\s-*\\)" " -")))

(require 'helm-flycheck)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r" "C-c p" "C-c C-x"))
;; (guide-key-mode 1)

(require 'which-key)
(which-key-mode)

(provide 'my-editor)
;;; my-editor.el ends here
