;;; package --- Summary
;;; Commentary:

;; My buffer setting.

;;; Code:

;; organize ibuffer
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Remote" (filename . "^/scp"))
               ("Image" (mode . image-mode))
               ("Python" (mode . python-mode))
               ("Lua" (mode . lua-mode))
               ("Dired" (mode . dired-mode))
               ("Matlab" (mode . matlab-mode))
               ("Org" (or
                       (mode . org-mode)
                       (mode . markdown-mode)))
               ("C++" (or
                       (mode . makefile-mode)
                       (mode . c-mode)
                       (mode . c++-mode)
                       (mode . cuda-mode)))
               ("Tex" (or
                       (mode . latex-mode)
                       (mode . plain-tex-mode)
                       (mode . bibtex-mode)))
               ("Web" (or
                       (mode . html-mode)
                       (mode . nxml-mode)
                       (mode . web-mode)
                       (mode . js2-mode)
                       (mode . conf-mode)
                       (mode . css-mode)))
               ("Shell" (or
                         (mode . emacs-lisp-mode)
                         (mode . sh-mode)))
               ("Configuration" (mode . protobuf-mode))
               ("Console" (name . "^\\*.*\\*$"))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;;====================================================
;; Open buffer in other window
(require 'ace-window)

;; open other buffer in current window
(defun my-duplicate-ace-buffer-in-current-window ()
  "Open other buffer in current window."
  (interactive)
  (let ((start-win (selected-window))
        (buf (current-buffer))
        (win (aw-select " Ace Buffer: ")))
    (progn
      (aw-switch-to-window win)
      (setq buf (buffer-name))
      (message buf)
      (aw-switch-to-window start-win)
      (switch-to-buffer buf))))

;; open current buffer in other window
(defun my-duplicate-current-buffer-in-ace-window ()
  "Open current buffer in other window."
  (interactive)
  (let (buf)
    (setq buf (current-buffer))
    (aw-switch-to-window (aw-select " Ace Buffer: "))
    (switch-to-buffer buf)))

;; move current buffer in other window
(defun my-move-current-buffer-in-ace-window ()
  "Move current buffer in other window"
  (interactive)
  (let ((curr-buf (current-buffer))
        (prev-buf (previous-buffer))
        (start-win (selected-window))
        (target-win (aw-select " Ace Buffer: ")))
    (aw-switch-to-window target-win)
    (switch-to-buffer curr-buf)
    (aw-switch-to-window start-win)
    (switch-to-buffer prev-buf)))

;;==============
;; use auto-save
;; follow http://www.jianshu.com/p/998ceaf522d1

;; Emacs' default auto-save is stupid to generate #foo# files!
(setq auto-save-default nil)

(setq auto-save-idle 1)
(setq auto-save-slient t)

(defun auto-save-buffers ()
  (interactive)
  (let ((autosave-buffer-list))
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (buffer-file-name) (buffer-modified-p))
            (progn
              (push (buffer-name) autosave-buffer-list)
              (if auto-save-slient
                  (with-temp-message ""
                    (basic-save-buffer))
                (basic-save-buffer))
              )))
      ;; Tell user when auto save files.
      (unless auto-save-slient
        (cond
         ;; It's stupid tell user if nothing to save.
         ((= (length autosave-buffer-list) 1)
          (message "# Saved %s" (car autosave-buffer-list)))
         ((> (length autosave-buffer-list) 1)
          (message "# Saved %d files: %s"
                   (length autosave-buffer-list)
                   (mapconcat 'identity autosave-buffer-list ", ")))))
      )))

(defun auto-save-enable ()
  (interactive)
  (run-with-idle-timer auto-save-idle t #'auto-save-buffers))

(auto-save-enable)

;;=======
;; save buffer with whitespace cleanup
(defun my-save-buffer ()
  "Save buffer with whitespace cleanup."
  (interactive)
  (whitespace-cleanup)
  (save-buffer))

;; revert buffer without confirmation
(defun my-revert-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(provide 'my-buffer)
;;; my-ibuffer.el ends here
