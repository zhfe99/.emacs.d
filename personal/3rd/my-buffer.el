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
  (let (buf (current-buffer))
    (aw-switch-to-window (aw-select " Ace Buffer: "))
    (switch-to-buffer buf)))

;; use auto-save
;; follow http://www.jianshu.com/p/998ceaf522d1
(require 'auto-save)
(auto-save-enable)
(setq auto-save-slient t)

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

;;===============================================
;; Change the binding action of ivy-switch-buffer
(ivy-set-actions
 'ivy-switch-buffer
 '(("a" (lambda (buffer)
          (aw-switch-to-window (aw-select " Ace Buffer: "))
          (switch-to-buffer buffer))
    "ace")
   ("1" (lambda (buffer)
          (delete-other-windows)
          (switch-to-buffer buffer))
    "other")
   ("2" (lambda (buffer)
          (split-window-below-and-move-there-dammit)
          (switch-to-buffer buffer))
    "below")
   ("3" (lambda (buffer)
          (split-window-right-and-move-there-dammit)
          (switch-to-buffer buffer))
    "right")
   ("k"
    (lambda (x)
      (kill-buffer x)
      (ivy--reset-state ivy-last))
    "kill")))

;;===================================================
;; Change the binding action of counsel-switch-buffer
(ivy-set-actions
 'counsel-find-file
 '(("a" (lambda (file)
          (aw-switch-to-window (aw-select " Ace Buffer: "))
          (find-file file))
    "ace")
   ("1" (lambda (file)
          (delete-other-windows)
          (find-file file))
    "other")
   ("2" (lambda (file)
          (split-window-below-and-move-there-dammit)
          (find-file file))
    "below")
   ("3" (lambda (file)
          (split-window-right-and-move-there-dammit)
          (find-file file))
    "right")
   ("z" (lambda (d)
          (reveal-in-osx-finder-as d nil))
    "reveal")))

;;===========================================================
;; Change the binding action of counsel-goto-recent-directory
(ivy-set-actions
 'counsel-goto-recent-directory
 '(("a" (lambda (d)
          (aw-switch-to-window (aw-select " Ace Buffer: "))
          (dired d))
    "ace")
   ("1" (lambda (d)
          (delete-other-windows)
          (dired d))
    "other")
   ("2" (lambda (d)
          (split-window-below-and-move-there-dammit)
          (dired d))
    "below")
   ("3" (lambda (d)
          (split-window-right-and-move-there-dammit)
          (dired d))
    "right")
   ("z" (lambda (d)
          (reveal-in-osx-finder-as d nil))
    "reveal")))

(provide 'my-buffer)
;;; my-ibuffer.el ends here
