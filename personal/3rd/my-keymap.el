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
(global-set-key (kbd "\e[60;1~") 'hydra-gdb/body)

;; C-
;; Some of the key might not be available in terminal
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-M-k") 'crux-kill-whole-line)
(global-set-key (kbd "C-j") 'indent-new-comment-line)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-t") 'hydra-transpose/body)
(global-set-key (kbd "C-o") 'hydra-window/body)
(global-set-key (kbd "C-.") 'hydra-mc/body)
(global-set-key (kbd "\e[46;5~") 'hydra-mc/body) ; C-. in iterm2
(global-set-key (kbd "C-=") 'hydra-region/body)
(global-set-key (kbd "\e[46;3~") 'hydra-region/body) ; C-= in iterm2

;; M-
;; the following keys are available
;; e, r, i
;; a, h, k
;; , /
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-t") 'hydra-term/body)
(global-set-key (kbd "M-o") 'hydra-open/body)
(global-set-key (kbd "M-g") 'hydra-git/body)
(global-set-key (kbd "M-c") 'hydra-case/body)
(global-set-key (kbd "M-s") 'hydra-sp/body)
(global-set-key (kbd "M-j") 'hydra-jump/body)
(global-set-key (kbd "M-l") 'hydra-vi/body)
(global-set-key (kbd "M-m") 'hydra-misc/body)
(global-set-key (kbd "M-.") 'hydra-tag/body)

;; dired-mode
(define-key dired-mode-map "E" 'ace-dired-find-file)
(define-key dired-mode-map "o" 'crux-open-with)
(define-key dired-mode-map "c" 'dired-toggle-read-only)
(define-key dired-mode-map "d" 'dired-do-delete)
(define-key dired-mode-map "D" 'bjm/move-file-here)
(define-key dired-mode-map "Y" 'ora-dired-rsync)
(define-key dired-mode-map ")" 'dired-omit-mode)
(define-key dired-mode-map "J" 'dired-up-directory)
(define-key dired-mode-map (kbd "M-b") 'subword-backward)
(define-key dired-mode-map (kbd "M-l") 'hydra-line/body)
(define-key dired-mode-map (kbd "C-o") 'hydra-window/body)
(define-key dired-mode-map (kbd "M-g") 'hydra-git/body)
(define-key org-mode-map (kbd "M-m") 'hydra-org/body)

;; unset keys
(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)
(eval-after-load "smartparens-mode"
  '(define-key minor-mode-map (kbd "<M-up>") 'move-text-up))
(keymap-unset-key '[M-down] "smartparens-mode")
(keymap-unset-key '[M-up] "smartparens-mode")
(keymap-unset-key '[M-s] "smartparens-mode")
(keymap-unset-key (kbd "M-s") "smartparens-mode")
(keymap-unset-key (kbd "M-r") "smartparens-mode")

;; key-chord
(key-chord-define-global "yy" nil)
(key-chord-define-global "xx" nil)
(key-chord-define-global "jj" 'linum-mode)

(provide 'my-keymap)
;;; my-keymap.el ends here
