;;; package --- Summary
;;; Commentary:

;; My Python setting.

;;; Code:

;; Use only own snippets, do not use bundled ones
(require 'yasnippet)
(require 'yasnippet-snippets)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(yas-reload-all)

(use-package elpy
  :ensure t
  :defer t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3"))
  ;; (advice-add 'python-mode :before 'elpy-enable))

;; I don't like highlight-indentation mode
;; Instead I prefer to use indent-guide mode
(setq elpy-modules '(elpy-module-sane-defaults
                     ;; elpy-module-company
                     ;; elpy-module-flymake
                     ;; elpy-module-pyvenv
                     elpy-module-highlight-indentation))

;; use fly-check instead of flymake
;; (with-eval-after-load 'elpy
;;   (remove-hook 'elpy-modules 'elpy-module-flymake))

(setq python-shell-prompt-detect-failure-warning nil)

;; show elpy buffer in the current window
(defun my-elpy-shell-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (pop-to-buffer-same-window (process-buffer (elpy-shell-get-or-create-process))))

;; using ipython as the default python console
;; (setq python-shell-interpreter "python")
;; (setq python-shell-interpreter-args "--pylab")

;; remap up key in python shell
(define-key comint-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)

;; update creating date in the comment area (for python)
(defun my-python-create-date ()
  "Update creating date in the comment area (for python)."
  (interactive)
  (save-excursion
    (let ((time-format "%Y-%m") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "  create" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "create xxx not found")))))

;; python mode hook
(add-hook 'python-mode-hook
          '(lambda ()
             (git-gutter+-mode)
             ;; (linum-mode)
             (setq python-indent-offset 4)))

;; Select all lines belong to the sub-block
(defun my-nav-expand-to-sub-block ()
  "Select all lines belong to the sub-block."
  (interactive)
  (save-excursion
    (setq elpy-nav-expand--initial-position (point))
    (python-nav-forward-statement)
    (let ((indentation (current-indentation)))
      (if (= indentation 0)
          (mark-whole-buffer)
        (while (<= indentation (current-indentation))
          (python-nav-backward-statement))
        (next-line)
        (push-mark (point) nil t)
        (while (<= indentation (current-indentation))
          (python-nav-forward-statement))
        (python-nav-backward-statement)))
    (elpy-nav-indent-shift-left)))

(defun my-blank-line-p ()
  (= (current-indentation)
     (- (line-end-position) (line-beginning-position))))

(defun my-python-shift-block-right-two-space ()
  "Select all lines belong to the sub-block."
  (interactive)
  (save-excursion
    (let ((init-indentation (current-indentation))
          (curr-indentation)
          (curr-line-empty))
      (next-line)
      (setq curr-indentation (current-indentation))
      (setq curr-line-empty (my-blank-line-p))
      (while (or (my-blank-line-p)
                 (< init-indentation curr-indentation))
        (move-beginning-of-line nil)
        (insert "  ")
        (next-line)
        (setq curr-indentation (current-indentation))))))

(provide 'my-python)
;;; my-python.el ends here
