;;; package --- Summary
;;; Commentary:

;; My main editor setting.

;;; Code:

;; smooth scroll
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;; turn-off guru warning
(setq prelude-guru nil)

;; ace-mode
(global-ace-isearch-mode -1)
(setq ace-isearch-use-ace-jump nil)

;; turn-off beacon
(beacon-mode -1)

;; turn-on which-function-mode
;; but turn-off it for cython (.pyx, .pyd) otherwise it will be extremely slow
(which-function-mode 1)
(eval-after-load "which-func"
  '(setq which-func-modes '(java-mode c++-mode c-mode org-mode python-mode emacs-lisp-mode)))

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

;; switch to current file on server or local
(defun my-switch-to-current-file-on-server-or-local()
  "Switch current file on server or local."
  (interactive)
  (let (my-buffer-file-name my-buffer-file-name1)
    (if (my-string-starts-with (buffer-file-name) "/scp:")
        (progn
          (setq my-buffer-file-name (substring (buffer-file-name) (length "/scp:feng@skyserver3k:/home/ma/feng/") nil))
          (setq my-buffer-file-name1 (concat "/Users/feng/" my-buffer-file-name))
          (find-file my-buffer-file-name1))
      (progn
        (setq my-buffer-file-name (substring (buffer-file-name) (length "/Users/feng/") nil))
        (setq my-buffer-file-name1 (concat "/scp:feng@skyserver3k:/home/ma/feng/" my-buffer-file-name))
        (find-file my-buffer-file-name1)
        ))))

;; get current fold on server or local
(defun my-switch-to-current-dired-on-server-or-local()
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
(defun my-switch-to-current-on-server-or-local()
  "Get current fold on server or local."
  (interactive)
  (if (string= major-mode "dired-mode")
      (my-switch-to-current-dired-on-server-or-local)
    (my-switch-to-current-file-on-server-or-local)))

(defun my-align-comment()
  (interactive)
  (align-regexp
   (region-beginning)
   (region-end)
   (concat "\\(\\s-*\\)" " -")))

(global-flycheck-mode -1)
;; (require 'helm-flycheck)
;; (setq flycheck-display-errors-delay 0.9)
;; (setq flycheck-display-errors-function #'flycheck-display-error-messages)
;; (add-to-list 'display-buffer-alist (cons "\\*Flycheck error messages\\*" (cons #'display-buffer-no-window nil)))

(keyfreq-mode 1)
(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        previous-line
        next-line))
(keyfreq-autosave-mode 1)

(require 'which-key)
(which-key-mode)

;; narrow / widen the current region
;; or narrow / widen the current subtree if in org-mode
(defun my-narrow-or-widen-dwim ()
  "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        ((equal major-mode 'org-mode) (org-narrow-to-subtree))
        (t (error "Please select a region to narrow to"))))

;; re-open file as sudo
(defun my-find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

;; load markdown-mode
(require 'markdown-mode)

;; markdown-mode-hook
(add-hook 'markdown-mode-hook
          (lambda()
            (visual-line-mode 1)))

;; load markdown-preview-mode (C-c C-c p is to preview the markdown file in browser)
(require 'markdown-preview-mode)

;; use multiple-cursor
(require 'multiple-cursors)

;; multiple-cursor: map RET to get consistent behavior at the terminal
;; https://github.com/magnars/multiple-cursors.el/pull/168
(define-key mc/keymap (kbd "<RET>") 'multiple-cursors-mode)

;; use auto-save
;; follow http://www.jianshu.com/p/998ceaf522d1
(require 'auto-save)
(auto-save-enable)
(setq auto-save-slient t)

;; enable to use global company
(global-company-mode)

(cond
 ((string-equal system-type "darwin")
  (require 'reveal-in-osx-finder)))

(provide 'my-editor)
;;; my-editor.el ends here
