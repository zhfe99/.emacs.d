;;; package --- Summary
;;; Commentary:
;;; Code:

;; projectile key
(global-set-key (kbd "H-u") 'projectile-command-map)
(define-key projectile-command-map (kbd "g") 'helm-projectile-grep)
(define-key projectile-command-map (kbd "t") 'projectile-regenerate-tags)

;; my key for coding (all left-hand characters)
(global-set-key (kbd "H-q") 'last-kbd-macro)

;; w is available
(global-set-key (kbd "H-e") 'ediff-files)
(global-set-key (kbd "H-r") 'revert-buffer-no-confirm)
(global-set-key (kbd "H-t") 'git-timemachine)
(global-set-key (kbd "H-a") '(lambda ()
                               (interactive)
                               (find-file "~/log/org/my/todo.org")))
(global-set-key (kbd "H-A") '(lambda ()
                               (interactive)
                               (find-file "~/log/org/my/info.org")))
(global-set-key (kbd "H-s") 'sr-speedbar-toggle)
(global-set-key (kbd "H-d") 'dash-at-point)
(global-set-key (kbd "H-f") 'find-name-dired)
(global-set-key (kbd "H-g") 'rgrep)
;; z are available
(global-set-key (kbd "H-x") 'package-list-packages)
(add-hook 'matlab-mode-hook
          (lambda ()
            (local-set-key (kbd "H-c") 'my-matlab-create-date)
            (local-set-key (kbd "M-;") 'comment-dwim)))
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "H-c") 'my-python-create-date)))
(add-hook 'sh-mode-hook
          (lambda ()
            (local-set-key (kbd "H-c") 'my-sh-create-date)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'org-open-at-point)))

;; v b are available
(global-set-key (kbd "H--") '(lambda ()
                                    (interactive)
                                    (align-regexp
                                     (region-beginning)
                                     (region-end)
                                     (concat "\\(\\s-*\\)" " -"))))

;; my key for shell & window management (all right-hand characters)
;; y u i are available
(global-set-key (kbd "H-o") 'reveal-in-finder)
(global-set-key (kbd "H-p") 'my-elpy-shell-switch-to-shell)
;; [ ] are available
(global-set-key (kbd "H-|") 'my-toggle-window-split)
;; h j k ' are available
(global-set-key (kbd "H-j") 'org-clock-goto)
(global-set-key (kbd "H-l") 'matlab-shell)
(global-set-key (kbd "H-n") 'multi-term-next)
(global-set-key (kbd "H-m") 'multi-term)
(global-set-key (kbd "H-,") 'my-insert-double-space)
(global-set-key (kbd "H-.") 'my-insert-single-space)

;; global key
(global-set-key (kbd "<f5>") 'kmacro-set-counter)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "<H-M-up>") 'buf-move-up)
(global-set-key (kbd "<H-M-down>") 'buf-move-down)
(global-set-key (kbd "<H-M-left>") 'buf-move-left)
(global-set-key (kbd "<H-M-right>") 'buf-move-right)
(global-set-key (kbd "C-M-k") 'sp-kill-sexp)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x O") 'ace-swap-window)
(global-set-key (kbd "C-x 0") 'ace-delete-window)
(global-set-key (kbd "C-x )") 'delete-window)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-r") 'helm-swoop)

(provide 'my-keymap)
;;; my-basic.el ends here
