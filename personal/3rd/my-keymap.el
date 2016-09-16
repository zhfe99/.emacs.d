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

;; C-
;; The following keys are still available:
;; -
;; q
;; ; '
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
(global-set-key (kbd "C-.") 'dired-jump)
(global-set-key (kbd "C-,") 'company-complete)

;; Some of the key might not be available in terminal (eg., iterm2)
;; need rebind
(global-set-key (kbd "\e[45;1~") 'delete-other-windows) ; C-1
(global-set-key (kbd "\e[45;2~") 'split-window-below-and-balance) ; C-2
(global-set-key (kbd "\e[45;3~") 'split-window-right-and-balance) ; C-3
(global-set-key (kbd "\e[45;0~") 'ace-delete-window-and-balance) ; C-0
(global-set-key (kbd "\e[46;4~") 'company-complete) ; C-,
(global-set-key (kbd "\e[46;5~") 'dired-jump) ; C-.
(global-set-key (kbd "\e[46;3~") 'er/expand-region) ; C-=
(global-set-key (kbd "\e[47;A~") 'move-text-up)     ; M-up
(global-set-key (kbd "\e[47;B~") 'move-text-down)   ; M-down

;; M-
;; The following keys are still available:
;; =
;; [ ]
;; k
;;
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
(global-set-key (kbd "M-p") 'hydra-special/body)
(global-set-key (kbd "M-g") 'hydra-git/body)
(global-set-key (kbd "M-c") 'hydra-case/body)
(global-set-key (kbd "M-a") 'hydra-sp/body)
(global-set-key (kbd "M-s") 'my-save-buffer)
(global-set-key (kbd "M-i") 'hydra-jump/body)
(global-set-key (kbd "M-j") 'avy-goto-word-1)
(global-set-key (kbd "M-J") 'avy-goto-line)
(global-set-key (kbd "M-.") 'hydra-tag/body)
(global-set-key (kbd "M-q") 'hydra-toggle/body)
(global-set-key (kbd "M-m") 'iy-go-up-to-char)
(global-set-key (kbd "M-M") 'iy-go-to-char-backward)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; function key

;; dired-mode
(define-key dired-mode-map "E" 'ace-dired-find-file)
(define-key dired-mode-map "o" 'crux-open-with)
(define-key dired-mode-map "c" 'dired-toggle-read-only)
(define-key dired-mode-map "d" 'dired-do-delete)
(define-key dired-mode-map "t" 'my-term-open-at-current-buffer)
(define-key dired-mode-map "D" 'bjm/move-file-here)
(define-key dired-mode-map "Y" 'ora-dired-rsync)
(define-key dired-mode-map ")" 'dired-omit-mode)
(define-key dired-mode-map "." 'dired-up-directory)
(define-key dired-mode-map "L" 'my-org-store-link)
(define-key dired-mode-map "s" 'hydra-dired-sort/body)
(define-key dired-mode-map "z" 'reveal-in-osx-finder)
(define-key dired-mode-map (kbd "M-b") 'subword-backward)
(define-key dired-mode-map (kbd "M-u") 'counsel-projectile)
(define-key dired-mode-map (kbd "M-i") 'hydra-jump/body)
(define-key dired-mode-map (kbd "C-o") 'hydra-window/body)
(define-key dired-mode-map (kbd "M-g") 'hydra-git/body)
(define-key dired-mode-map (kbd "M-l") 'hydra-open/body)
(define-key dired-mode-map (kbd "M-p") 'hydra-special/body)

;; org-mode
(define-key org-mode-map (kbd "M-,") 'hydra-org/body)
(define-key org-mode-map (kbd "M-a") 'hydra-sp/body)
(define-key org-mode-map (kbd "M-e") 'hydra-edit/body)
(define-key org-mode-map (kbd "M-h") 'ivy-switch-buffer)
(define-key org-mode-map (kbd "<M-up>") 'move-text-up)
(define-key org-mode-map (kbd "<M-down>") 'move-text-down)
(define-key org-mode-map (kbd "<S-up>") 'windmove-up)
(define-key org-mode-map (kbd "<S-down>") 'windmove-down)
(define-key org-mode-map (kbd "<S-left>") 'windmove-left)
(define-key org-mode-map (kbd "<S-right>") 'windmove-right)
(define-key org-mode-map (kbd "\e[49;C~") 'org-shiftmetaleft) ; M-S-left in iterm2
(define-key org-mode-map (kbd "\e[49;D~") 'org-shiftmetaright) ; M-S-right in iterm2
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "M-,") 'hydra-org/body)))

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
(define-key c++-mode-map (kbd "M-a") 'hydra-sp/body)
(define-key c-mode-map (kbd "M-a") 'hydra-sp/body)
(define-key c++-mode-map (kbd "M-,") 'hydra-c/body)
(define-key c-mode-map (kbd "M-,") 'hydra-c/body)
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

;; prelude-mode
(defun my-prelude-mode-keys ()
  "My keybindings for prelude-mode."
  (define-key prelude-mode-map (kbd "M-o") nil))
(add-hook 'prelude-mode-hook 'my-prelude-mode-keys)

(provide 'my-keymap)
;;; my-keymap.el ends here
