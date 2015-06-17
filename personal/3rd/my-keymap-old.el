;;; package --- Summary
;;; Commentary:
;;; Code:

;; projectile key
(global-set-key (kbd "H-p") 'projectile-command-map)
(define-key projectile-command-map (kbd "g") 'helm-projectile-grep)
(define-key projectile-command-map (kbd "t") 'projectile-regenerate-tags)

;; my key prefix
(define-prefix-command 'my-key-map)
(global-set-key (kbd "M-m") 'my-key-map)

;; my key for coding (all left-hand characters)
(define-key my-key-map (kbd "q") 'last-kbd-macro)
(global-set-key (kbd "H-q") 'last-kbd-macro)
;; w is available
(define-key my-key-map (kbd "e") 'ediff-files)
(define-key my-key-map (kbd "r") 'revert-buffer-no-confirm)
(define-key my-key-map (kbd "t") 'git-timemachine)
(define-key my-key-map (kbd "a") '(lambda ()
                                    (interactive)
                                    (find-file "~/log/org/my/todo.org")))
(define-key my-key-map (kbd "A") '(lambda ()
                                    (interactive)
                                    (find-file "~/log/org/my/info.org")))
(define-key my-key-map (kbd "s") 'sr-speedbar-toggle)
(define-key my-key-map (kbd "d") 'dash-at-point)
(define-key my-key-map (kbd "f") 'find-name-dired)
(define-key my-key-map (kbd "g") 'rgrep)
;; z are available
(define-key my-key-map (kbd "x") 'package-list-packages)
(add-hook 'matlab-mode-hook
          (lambda ()
            (local-set-key (kbd "M-m c") 'my-matlab-create-date)
            (local-set-key (kbd "M-;") 'comment-dwim)))
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "M-m c") 'my-python-create-date)))
(add-hook 'sh-mode-hook
          (lambda ()
            (local-set-key (kbd "M-m c") 'my-sh-create-date)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'org-open-at-point)))
;; v b are available
(define-key my-key-map (kbd "-") '(lambda ()
                                    (interactive)
                                    (align-regexp
                                     (region-beginning)
                                     (region-end)
                                     (concat "\\(\\s-*\\)" " -"))))

;; my key for shell & window management (all right-hand characters)
;; y u i are available
(define-key my-key-map (kbd "o") 'reveal-in-finder)
(define-key my-key-map (kbd "p") 'my-elpy-shell-switch-to-shell)
;; [ ] are available
(define-key my-key-map (kbd "|") 'my-toggle-window-split)
;; h j k ' are available
(define-key my-key-map (kbd "j") 'org-clock-goto)
(define-key my-key-map (kbd "l") 'matlab-shell)
(define-key my-key-map (kbd "n") 'multi-term-next)
(define-key my-key-map (kbd "m") 'multi-term)
(define-key my-key-map (kbd ",") 'my-insert-double-space)
(define-key my-key-map (kbd ".") 'my-insert-single-space)

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

(provide 'my-keymap-old)
;;; my-basic.el ends here
