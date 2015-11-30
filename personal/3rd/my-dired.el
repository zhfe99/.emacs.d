;;; package --- Summary
;;; Commentary:
;;; Code:

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(eval-after-load 'dired
  '(progn
     (require 'dired+)
     (require 'dired-sort)
     (global-dired-hide-details-mode)))

(setq dired-listing-switches "-alh")

;; dired omit files
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\.DS_Store$"))

;; ace-jump only search filename in dired
(add-hook 'dired-mode-hook
          (lambda ()
            (subword-mode 1)
            (dired-omit-mode t)
            (setq-local ace-jump-search-filter
                        (lambda ()
                          (get-text-property (point) 'dired-filename)))))

;; ibuffer
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
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

(defun ace-command-other-window (cmd &optional one-win-cmd)
  "Execute CMD in another window.
If provided, call ONE-WIN-CMD instead when there is only one window."
  (interactive "CM-x (other window) ")
  (catch 'done
    (when (and one-win-cmd
               (not (window-parent)))
      (call-interactively one-win-cmd)
      (throw 'done t))
    (let ((start-window (selected-window)))
      (unwind-protect
          (progn
            (aw-switch-to-window
             (aw-select  " Ace - Command "))
            (call-interactively cmd))
        (aw-switch-to-window start-window)))))

(defun ace-find-file ()
  "Find a file and display it in another window."
  (interactive)
  (if (not (window-parent))
      (ido-find-file-other-window)
    (let ((start-win (selected-window))
          (buf (find-file-noselect (ido-read-file-name "File: ")))
          (win (aw-select " Ace File: ")))
      (unwind-protect
          (progn
            (aw-switch-to-window win)
            (switch-to-buffer buf))
        (aw-switch-to-window start-win)))))

(defun ace-switch-buffer ()
  "Switch to another buffer in another window."
  (interactive)
  (if (not (window-parent))
      (ido-switch-buffer-other-window)
    (let ((start-win (selected-window))
          (buf (ido-read-buffer "Buffer: "))
          (win (aw-select " Ace Buffer: ")))
      (unwind-protect
          (progn
            (aw-switch-to-window win)
            (switch-to-buffer buf))
        (aw-switch-to-window start-win)))))

(defun ace-dired-find-file ()
  "Find a file and display it in another window."
  (interactive)
  (if (not (window-parent))
      (ido-find-file-other-window)
    (let ((start-win (selected-window))
          (buf (dired-get-file-for-visit))
          (win (aw-select " Ace File: ")))
      (progn
        (aw-switch-to-window win)
        (find-file buf)))))

(require 'ace-window)
(defun ace-duplicate-buffer ()
  "Switch to another buffer in another window."
  (interactive)
  (let ((start-win (selected-window))
        (buf (current-buffer))
        (win (aw-select " Ace Buffer: ")))
    (progn
      ;; (message win)
      (aw-switch-to-window win)
      (setq buf (buffer-name))
      (message buf)
      (aw-switch-to-window start-win)
      (switch-to-buffer buf)
      )))

(define-key dired-mode-map "o" 'ace-dired-find-file)
(define-key dired-mode-map (kbd "M-b") 'subword-backward)
(define-key dired-mode-map (kbd "M-u") 'helm-projectile-switch-project)
(define-key dired-mode-map (kbd "M-p") 'my-get-current-on-server-or-local)

(provide 'my-dired)
;;; my-dired.el ends here