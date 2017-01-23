;;; package --- Summary
;;; Commentary:

;; My global key-map setting.

;;; Code:

;; C-
;; The following keys are still available:
;; -
;; q ]
;; ;
;;
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below-and-balance)
(global-set-key (kbd "C-3") 'split-window-right-and-balance)
(global-set-key (kbd "C-0") 'ace-delete-window-and-balance)
(global-set-key (kbd "C-z") 'mc-friendly/zap-up-to-char)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-h b") 'counsel-descbinds)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-j") 'indent-new-comment-line)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-t") 'hydra-transpose/body)
(global-set-key (kbd "C-o") 'hydra-window/body)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-'") 'dired-jump)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)

;; Some of the key might not be available in terminal (eg., iterm2)
;; need rebind
(global-set-key (kbd "\e[45;1~") 'delete-other-windows) ; C-1
(global-set-key (kbd "\e[45;2~") 'split-window-below-and-balance) ; C-2
(global-set-key (kbd "\e[45;3~") 'split-window-right-and-balance) ; C-3
(global-set-key (kbd "\e[45;0~") 'ace-delete-window-and-balance) ; C-0
(global-set-key (kbd "\e[27;5;39~") 'dired-jump) ; C-'
(global-set-key (kbd "\e[46;4~") 'mc/mark-previous-like-this) ; C-,
(global-set-key (kbd "\e[46;5~") 'mc/mark-next-like-this) ; C-.
(global-set-key (kbd "\e[46;3~") 'er/expand-region) ; C-=
(global-set-key (kbd "\e[47;A~") 'move-text-up)     ; M-up
(global-set-key (kbd "\e[47;B~") 'move-text-down)   ; M-down

;; M-
;; The following keys are still available:
;; =
;; p ]
;; k
;; n
(global-set-key (kbd "M-Z") 'iy-go-up-to-char)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-'") 'repeat)
(global-set-key (kbd "M-e") 'hydra-edit/body)
(global-set-key (kbd "M-r") 'hydra-region/body)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-u") 'counsel-projectile)
(global-set-key (kbd "M-t") 'hydra-term/body)
(global-set-key (kbd "M-h") 'ivy-switch-buffer)
(global-set-key (kbd "M-o") 'counsel-find-file)
(global-set-key (kbd "M-l") 'hydra-open/body)
(global-set-key (kbd "M-g") 'hydra-git/body)
(global-set-key (kbd "M-c") 'hydra-case/body)
(global-set-key (kbd "M-a") 'hydra-sp/body)
(global-set-key (kbd "M-s") 'my-save-buffer)
(global-set-key (kbd "M-i") 'hydra-jump/body)
(global-set-key (kbd "M-j") 'avy-goto-word-1)
(global-set-key (kbd "M-J") 'avy-goto-line)
(global-set-key (kbd "M-;") 'my-comment-dwim)
(global-set-key (kbd "M-m") 'hydra-mc/body)
(global-set-key (kbd "M-.") 'hydra-tag/body)
(global-set-key (kbd "M-q") 'hydra-toggle/body)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; H-
(global-set-key (kbd "H-g") 'hydra-gdb/body)

;; Some of the key might not be available in terminal (eg., iterm2)
;; need rebind
(global-set-key (kbd "\e[60;1~") 'hydra-gdb/body) ; H-g

;; f-

;; dired-mode
(define-key dired-mode-map "c" 'dired-toggle-read-only)
(define-key dired-mode-map "d" 'dired-do-delete)
(define-key dired-mode-map "f" 'my-dired-find-file-ace-window)
(define-key dired-mode-map ")" 'dired-omit-mode)
(define-key dired-mode-map "/" 'dired-narrow)
(define-key dired-mode-map "'" 'dired-up-directory)
(define-key dired-mode-map "s" 'hydra-dired-sort/body)
(define-key dired-mode-map "i" 'hydra-dired-info/body)
(define-key dired-mode-map "r" 'hydra-dired-ranger/body)
(define-key dired-mode-map (kbd "M-b") nil)
(define-key dired-mode-map (kbd "M-u") nil)
(define-key dired-mode-map (kbd "M-i") nil)
(define-key dired-mode-map (kbd "C-o") nil)
(define-key dired-mode-map (kbd "M-g") nil)
(define-key dired-mode-map (kbd "M-l") nil)
(define-key dired-mode-map (kbd "M-p") nil)

;; org-mode
(require 'org)
(define-key org-mode-map (kbd "M-,") 'hydra-org/body)
(define-key org-mode-map (kbd "M-a") nil)
(define-key org-mode-map (kbd "M-e") nil)
(define-key org-mode-map (kbd "M-h") nil)
(define-key org-mode-map (kbd "C-'") nil)
(define-key org-mode-map (kbd "<M-up>") nil)
(define-key org-mode-map (kbd "<M-down>") nil)
(define-key org-mode-map (kbd "<S-up>") nil)
(define-key org-mode-map (kbd "<S-down>") nil)
(define-key org-mode-map (kbd "<S-left>") nil)
(define-key org-mode-map (kbd "<S-right>") nil)
(define-key org-mode-map (kbd "\e[49;C~") 'org-shiftmetaleft) ; M-S-left in iterm2
(define-key org-mode-map (kbd "\e[49;D~") 'org-shiftmetaright) ; M-S-right in iterm2

;; org-agenda-mode
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "<S-right>") nil)
            (local-set-key (kbd "<S-left>") nil)
            (local-set-key (kbd "o") 'org-open-at-point)
            (local-set-key (kbd "c") 'my-org-capture)
            (local-set-key (kbd "w") 'org-agenda-refile)
            (local-set-key (kbd "M-,") 'hydra-org/body)))

;; markdown-mode
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") nil)
            (local-set-key (kbd "M-n") nil)))

;; lisp-mode
(define-key lisp-mode-map (kbd "M-,") 'hydra-lisp/body)
(define-key emacs-lisp-mode-map (kbd "M-,") 'hydra-lisp/body)

;; sh-mode
(with-eval-after-load "sh-mode"
  (define-key sh-mode-map (kbd "M-,") 'hydra-sh/body))

;; python-mode
(define-key python-mode-map (kbd "M-,") 'hydra-python/body)

;; elpy
(with-eval-after-load "elpy"
  (define-key elpy-mode-map (kbd "<M-S-left>") 'my-nav-expand-to-sub-block)
  (define-key elpy-mode-map (kbd "<M-up>") nil)
  (define-key elpy-mode-map (kbd "<M-down>") nil)
  (define-key elpy-mode-map (kbd "<M-S-right>") 'my-python-shift-block-right-two-space)
  (define-key elpy-mode-map (kbd "M-.") nil)
  (define-key elpy-mode-map (kbd "\e[49;C~") 'my-nav-expand-to-sub-block)
  (define-key elpy-mode-map (kbd "\e[47;C~") 'elpy-nav-indent-shift-left)
  (define-key elpy-mode-map (kbd "\e[47;D~") 'elpy-nav-indent-shift-right)
  (define-key elpy-mode-map (kbd "\e[49;D~") 'my-python-shift-block-right-two-space))

;; c-mode
(define-key c++-mode-map (kbd "M-j") nil)
(define-key c-mode-map (kbd "M-j") nil)
(define-key c++-mode-map (kbd "M-e") nil)
(define-key c-mode-map (kbd "M-e") nil)
(define-key c++-mode-map (kbd "M-a") nil)
(define-key c-mode-map (kbd "M-a") nil)
(define-key c++-mode-map (kbd "M-q") nil)
(define-key c-mode-map (kbd "M-q") nil)
(define-key c++-mode-map (kbd "M-,") 'hydra-c/body)
(define-key c-mode-map (kbd "M-,") 'hydra-c/body)

;; protobuf-mode
(require 'protobuf-mode)
(define-key protobuf-mode-map (kbd "M-j") 'avy-goto-word-1)

;; matlab
(add-hook 'matlab-mode-hook
          (lambda()
            (local-set-key (kbd "M-q") nil)
            (local-set-key (kbd "M-j") nil)
            (local-set-key (kbd "M-a") nil)
            (local-set-key (kbd "M-s") nil)
            (local-set-key (kbd "M-;") nil)))

;; diff-mode
(define-key diff-mode-map (kbd "M-o") 'hydra-open/body)

;; flyspell-mode
(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)

;; smartparens-mode
(define-key smartparens-mode-map (kbd "<M-up>") nil)
(define-key smartparens-mode-map (kbd "<M-down>") nil)
(define-key smartparens-mode-map (kbd "M-s") nil)
(define-key smartparens-mode-map (kbd "M-r") nil)

;; prelude-mode
(define-key prelude-mode-map (kbd "M-o") nil)

;; js2-mode
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "M-.") nil))
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "M-j") nil))

(provide 'my-keymap)
;;; my-keymap.el ends here
