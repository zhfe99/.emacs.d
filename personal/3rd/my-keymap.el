;;; package --- Summary
;;; Commentary:

;; My global key-map setting.

;;; Code:

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

;; H-
(global-set-key (kbd "H-g") 'hydra-gdb/body)
(global-set-key (kbd "\e[60;1~") 'hydra-gdb/body) ; H-g
(global-set-key (kbd "H-i") 'hydra-info/body)
(global-set-key (kbd "H-.") 'hydra-tag/body)

;; C-
;; The following keys are still available:
;; q
;; ; '
;; , 
;; Some of the key might not be available in terminal
(global-set-key (kbd "C-z") 'mc-friendly/zap-up-to-char)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-j") 'indent-new-comment-line)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-t") 'hydra-transpose/body)
(global-set-key (kbd "C-o") 'hydra-window/body)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-.") 'hydra-mc/body)
(global-set-key (kbd "\e[46;5~") 'hydra-mc/body) ; C-. in iterm2
(global-set-key (kbd "C-=") 'hydra-region/body)
(global-set-key (kbd "\e[46;3~") 'hydra-region/body) ; C-= in iterm2

;; M-
;; The following keys are still available:
;; =
;; r [ ]
;; l
;;
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-'") 'repeat)
(global-set-key (kbd "M-e") 'hydra-edit/body)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-t") 'hydra-term/body)
(global-set-key (kbd "M-h") 'ivy-switch-buffer)
(global-set-key (kbd "M-H") 'counsel-goto-recent-directory)
(global-set-key (kbd "M-o") 'hydra-open/body)
(global-set-key (kbd "M-g") 'hydra-git/body)
(global-set-key (kbd "M-c") 'hydra-case/body)
(global-set-key (kbd "M-s") 'hydra-sp/body)
(global-set-key (kbd "M-i") 'hydra-jump/body)
(global-set-key (kbd "M-j") 'avy-goto-word-1)
(global-set-key (kbd "M-.") 'helm-etags+-select)
(global-set-key (kbd "M-,") 'helm-etags+-history-go-back)
(global-set-key (kbd "M-k") 'crux-kill-whole-line)
(global-set-key (kbd "M-q") 'hydra-toggle/body)
(global-set-key (kbd "M-m") 'iy-go-up-to-char)
(global-set-key (kbd "M-M") 'iy-go-to-char-backward)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; function key
(global-set-key (kbd "<f1>") 'org-agenda-list)
(global-set-key (kbd "<f2>") 'org-todo-list)

;; dired-mode
(define-key dired-mode-map "E" 'ace-dired-find-file)
(define-key dired-mode-map "o" 'crux-open-with)
(define-key dired-mode-map "c" 'dired-toggle-read-only)
(define-key dired-mode-map "d" 'dired-do-delete)
(define-key dired-mode-map "D" 'bjm/move-file-here)
(define-key dired-mode-map "Y" 'ora-dired-rsync)
(define-key dired-mode-map ")" 'dired-omit-mode)
(define-key dired-mode-map "J" 'dired-up-directory)
(define-key dired-mode-map "L" 'my-org-store-link)
(define-key dired-mode-map (kbd "<f1>") 'org-agenda-list)
(define-key dired-mode-map (kbd "<f2>") 'org-todo-list)
(define-key dired-mode-map (kbd "M-b") 'subword-backward)
(define-key dired-mode-map (kbd "M-i") 'hydra-jump/body)
(define-key dired-mode-map (kbd "C-o") 'hydra-window/body)
(define-key dired-mode-map (kbd "M-g") 'magit-status-fullscreen)

;; org-mode
(define-key org-mode-map (kbd "M-a") 'hydra-org/body)
(define-key org-mode-map (kbd "M-h") 'ivy-switch-buffer)
(define-key org-mode-map (kbd "<S-up>") 'windmove-up)
(define-key org-mode-map (kbd "<S-down>") 'windmove-down)
(define-key org-mode-map (kbd "<S-left>") 'windmove-left)
(define-key org-mode-map (kbd "<S-right>") 'windmove-right)
(define-key org-mode-map (kbd "\e[49;C~") 'org-shiftmetaleft) ; M-S-left in iterm2
(define-key org-mode-map (kbd "\e[49;D~") 'org-shiftmetaright) ; M-S-right in iterm2
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "M-a") 'hydra-org/body)))

;; lisp-mode
(define-key lisp-mode-map (kbd "M-a") 'hydra-lisp/body)
(define-key emacs-lisp-mode-map (kbd "M-a") 'hydra-lisp/body)

;; sh-mode
(with-eval-after-load "sh-mode"
  (define-key sh-mode-map (kbd "M-a") 'hydra-sh/body))

;; python-mode
(define-key python-mode-map (kbd "M-a") 'hydra-python/body)

;; elpy
(with-eval-after-load "elpy"  
  (define-key elpy-mode-map (kbd "<M-S-left>") 'my-nav-expand-to-sub-block)
  (define-key elpy-mode-map (kbd "<M-S-right>") 'my-python-shift-block-right-two-space)
  (define-key elpy-mode-map (kbd "\e[49;C~") 'my-nav-expand-to-sub-block)
  (define-key elpy-mode-map (kbd "\e[47;C~") 'elpy-nav-indent-shift-left)
  (define-key elpy-mode-map (kbd "\e[47;D~") 'elpy-nav-indent-shift-right)
  (define-key elpy-mode-map (kbd "\e[49;D~") 'my-python-shift-block-right-two-space))

;; c-mode
(define-key c++-mode-map (kbd "M-j") 'nil)
(define-key c-mode-map (kbd "M-j") 'nil)
(define-key c++-mode-map (kbd "M-e") nil)
(define-key c-mode-map (kbd "M-e") nil)
(define-key c++-mode-map (kbd "M-a") 'hydra-c/body)
(define-key c-mode-map (kbd "M-a") 'hydra-c/body)
(define-key c++-mode-map (kbd "M-q") nil)
(define-key c-mode-map (kbd "M-q") nil)
(define-key protobuf-mode-map (kbd "M-j") 'avy-goto-word-1)

;; diff-mode
(define-key diff-mode-map (kbd "M-o") 'hydra-open/body)

;; flyspell-mode
(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)

;; smartparens-mode
(with-eval-after-load "smartparens-mode"
  (define-key minor-mode-map (kbd "<M-up>") 'move-text-up))
(keymap-unset-key '[M-down] "smartparens-mode")
(keymap-unset-key '[M-up] "smartparens-mode")
(keymap-unset-key '[M-s] "smartparens-mode")
(keymap-unset-key (kbd "M-s") "smartparens-mode")
(keymap-unset-key (kbd "M-r") "smartparens-mode")

(provide 'my-keymap)
;;; my-keymap.el ends here
