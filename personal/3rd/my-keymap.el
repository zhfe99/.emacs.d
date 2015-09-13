;;; package --- Summary
;;; Commentary:
;;; Code:

;; projectile
(global-set-key (kbd "H-u") 'projectile-command-map)
(define-key projectile-command-map (kbd "g") 'helm-projectile-grep)
(define-key projectile-command-map (kbd "t") 'projectile-regenerate-tags)
(define-key projectile-command-map (kbd "o") 'projectile-find-other-file)

;; isearch
(define-key isearch-mode-map (kbd "C-'") 'helm-swoop-from-isearch)

;; H-key for coding (all left-hand characters)
;; H-z v b y u i [ ] h ' are available
(global-set-key (kbd "H-q") 'last-kbd-macro)
(global-set-key (kbd "H-w") 'sp-rewrap-sexp)
(global-set-key (kbd "H-e") 'ediff-files)
(global-set-key (kbd "H-r") 'revert-buffer-no-confirm)
(global-set-key (kbd "H-t") 'git-timemachine)
(global-set-key (kbd "H-a") 'my-open-todo-org)
(global-set-key (kbd "H-A") 'my-open-info-org)
(global-set-key (kbd "H-i") 'change-inner)
(global-set-key (kbd "H-I") 'copy-inner)
(global-set-key (kbd "H-S") 'helm-imenu)
(global-set-key (kbd "H-s") 'sp-splice-sexp)
(global-set-key (kbd "H-d") 'dash-at-point)
(global-set-key (kbd "H-f") 'my-get-current-on-server-or-local)
(global-set-key (kbd "H-F") 'find-name-dired)
(global-set-key (kbd "H-g") 'rgrep)
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
(add-hook 'lua-mode-hook
          (lambda ()
            (local-set-key (kbd "H-c") 'my-lua-create-date)))
(global-set-key (kbd "H-x") 'package-list-packages)
(global-set-key (kbd "H--") 'my-align-comment)
(global-set-key (kbd "H-o") 'reveal-in-finder)
(global-set-key (kbd "H-O") 'projectile-find-other-file)
(global-set-key (kbd "H-p") 'my-elpy-shell-switch-to-shell)
(global-set-key (kbd "H-|") 'my-toggle-window-split)
(global-set-key (kbd "H-j") 'org-clock-goto)
(global-set-key (kbd "H-k") 'kill-whole-line)
(global-set-key (kbd "H-l") 'lua-show-process-buffer)
(global-set-key (kbd "H-n") 'multi-term-next)
(global-set-key (kbd "H-m") 'matlab-shell)
(global-set-key (kbd "H-t") 'multi-term)
(global-set-key (kbd "H-.") 'my-insert-single-space)
(global-set-key (kbd "H->") 'my-insert-double-space)
(global-set-key (kbd "<H-down>") 'my-push-window-down)
(global-set-key (kbd "<H-up>") 'my-push-window-up)
(global-set-key (kbd "<H-left>") 'my-push-window-left)
(global-set-key (kbd "<H-right>") 'my-push-window-right)

;; C-key
(global-set-key (kbd "<f5>") 'kmacro-set-counter)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)
(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)

;; H-M-key
(global-set-key (kbd "<H-M-up>") 'buf-move-up)
(global-set-key (kbd "<H-M-down>") 'buf-move-down)
(global-set-key (kbd "<H-M-left>") 'buf-move-left)
(global-set-key (kbd "<H-M-right>") 'buf-move-right)
(global-set-key (kbd "C-M-k") 'sp-kill-sexp)

;; M-key
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(prelude-require-package 'jump-char)
(global-set-key (kbd "M-m") 'iy-go-up-to-char)
(global-set-key (kbd "M-M") 'iy-go-to-char-backward)
(global-set-key (kbd "<M-up>") 'move-text-up)
(eval-after-load "smartparens-mode"
  '(define-key minor-mode-map (kbd "<M-up>") 'move-text-up))
(global-set-key (kbd "<M-down>") 'move-text-down)

;; C-x
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x O") 'ace-swap-window)
(global-set-key (kbd "C-x 0") 'ace-delete-window)
(global-set-key (kbd "C-x )") 'delete-window)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x 2") 'split-window-below-and-move-there-dammit)
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

(defun get-key-combo (key)
  "Just return the KEY combo entered by the user."
  (interactive "kKey combo: ")
  key)

(defun keymap-unset-key (key keymap)
  "Remove binding of KEY in a KEYMAP."
  (interactive
   (list (call-interactively #'get-key-combo)
         (completing-read "Which map: " minor-mode-map-alist nil t)))
  (let ((map (rest (assoc (intern keymap) minor-mode-map-alist))))
    (when map
      (define-key map key nil)
      (message  "%s unbound for %s" key keymap))))

;; adjust key for smartparens
(keymap-unset-key '[M-down] "smartparens-mode")
(keymap-unset-key '[M-up] "smartparens-mode")
(keymap-unset-key '[M-s] "smartparens-mode")
(keymap-unset-key (kbd "M-s") "smartparens-mode")
(global-set-key (kbd "M-k") 'sp-kill-hybrid-sexp)
(global-set-key (kbd "M-s") 'save-buffer)

(global-set-key (kbd "C-1") '(lambda ()
                               (interactive)
                               (window-configuration-to-register 1)))
(global-set-key (kbd "C-2") '(lambda ()
                               (interactive)
                               (window-configuration-to-register 2)))
(global-set-key (kbd "C-3") '(lambda ()
                               (interactive)
                               (window-configuration-to-register 3)))
(global-set-key (kbd "M-1") '(lambda ()
                               (interactive)
                               (jump-to-register 1)))
(global-set-key (kbd "M-2") '(lambda ()
                               (interactive)
                               (jump-to-register 2)))
(global-set-key (kbd "M-3") '(lambda ()
                               (interactive)
                               (jump-to-register 3)))
;; (keymap-unset-key (kbd "C-c o") "prelude-mode")

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)

;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

(provide 'my-keymap)
;;; my-keymap.el ends here
