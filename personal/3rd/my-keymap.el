;;; package --- Summary
;;; Commentary:
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
;; available:
;; q w e r y u [ ]
;; s h l '
;; z b v , /
;; (global-set-key (kbd "H-q") 'god-local-mode)
(global-set-key (kbd "H-i") 'helm-imenu)
(global-set-key (kbd "H-t") 'my-regenerate-etags)
(global-set-key (kbd "H-o") 'reveal-in-finder)
(global-set-key (kbd "H-p") 'git-push-remote-origin)
;; (global-set-key (kbd "H-p") 'my-elpy-shell-switch-to-shell)
(global-set-key (kbd "H-a") 'my-open-todo-org)
(global-set-key (kbd "H-A") 'my-open-info-org)
(global-set-key (kbd "H-d") 'ace-duplicate-buffer)
(global-set-key (kbd "H-f") 'find-name-dired)
(global-set-key (kbd "H-g") 'rgrep)
(global-set-key (kbd "H-k") 'sp-kill-hybrid-sexp)
;; (global-set-key (kbd "H-l") 'lua-show-process-buffer)
(global-set-key (kbd "H-x") 'package-list-packages)
(global-set-key (kbd "H-c") 'org-capture)
(global-set-key (kbd "H--") 'my-align-comment)
(global-set-key (kbd "H-j") 'org-clock-goto)
(global-set-key (kbd "H-m") 'matlab-shell)
;; (global-set-key (kbd "H-.") 'my-reset-tags-table-list)
(global-set-key (kbd "H-.") 'my-insert-single-space)
(global-set-key (kbd "H->") 'my-insert-double-space)
(global-set-key (kbd "<H-down>") 'my-push-window-down)
(global-set-key (kbd "<H-up>") 'my-push-window-up)
(global-set-key (kbd "<H-left>") 'my-push-window-left)
(global-set-key (kbd "<H-right>") 'my-push-window-right)
(global-set-key (kbd "H-4") 'my-goto-window-conf-4)

;; C-
(global-set-key (kbd "<f5>") 'kmacro-set-counter)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-'") 'helm-swoop)
(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)
(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below-and-move-there-dammit)
(global-set-key (kbd "C-3") 'split-window-right-and-move-there-dammit)
(global-set-key (kbd "C-4") 'my-save-window-conf-4)
(global-set-key (kbd "C-0") 'ace-delete-window)
(global-set-key (kbd "C-)") 'delete-window)
(define-key isearch-mode-map (kbd "C-'") 'helm-swoop-from-isearch)
(global-unset-key (kbd "C-t"))
(global-set-key (kbd "C-t l") 'transpose-lines)
(global-set-key (kbd "C-t w") 'transpose-words)
(global-set-key (kbd "C-t c") 'transpose-chars)
(global-set-key (kbd "C-t s") 'transpose-sexps)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "C-j") 'indent-new-comment-line)
(require 'goto-chg)
(global-set-key (kbd "C-c ,") 'goto-last-change)
;; (global-set-key (kbd "C-c b .") 'goto-last-change-reverse)

;; C-x
(global-set-key (kbd "C-x o") 'ace-select-window)
(global-set-key (kbd "C-x O") 'ace-swap-window)

;; H-M-
(global-set-key (kbd "<H-M-up>") 'buf-move-up)
(global-set-key (kbd "<H-M-down>") 'buf-move-down)
(global-set-key (kbd "<H-M-left>") 'buf-move-left)
(global-set-key (kbd "<H-M-right>") 'buf-move-right)

;; M-
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-I") 'change-outer)
(global-set-key (kbd "M-g") 'avy-goto-line)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-m") 'iy-go-up-to-char)
(global-set-key (kbd "M-M") 'iy-go-to-char-backward)
(global-set-key (kbd "<M-up>") 'move-text-up)
(eval-after-load "smartparens-mode"
  '(define-key minor-mode-map (kbd "<M-up>") 'move-text-up))
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "M-k") 'kill-whole-line)
(keymap-unset-key '[M-down] "smartparens-mode")
(keymap-unset-key '[M-up] "smartparens-mode")
(keymap-unset-key '[M-s] "smartparens-mode")
(keymap-unset-key (kbd "M-s") "smartparens-mode")
(keymap-unset-key (kbd "M-r") "smartparens-mode")
(global-set-key (kbd "M-r") 'revert-buffer-no-confirm)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-h") 'helm-mini)
(global-set-key (kbd "M-H") 'helm-projectile-find-file)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-o") 'helm-find-files)
;; (setq scroll-preserve-screen-position 1)
(global-set-key (kbd "M-t") 'multi-term-next)
(global-set-key (kbd "M-T") 'multi-term)
(global-set-key (kbd "M-u") 'helm-projectile-switch-project)
(global-set-key (kbd "M-p") 'my-get-current-on-server-or-local)
(global-set-key (kbd "M-'") 'helm-flycheck)
(global-set-key (kbd "M-.") 'helm-etags+-select)
(global-set-key (kbd "M-*") 'helm-etags+-history)
(global-set-key (kbd "M-,") 'helm-etags+-history-go-back)
;; (global-set-key (kbd "M-.") 'etags-select-find-tag)
(global-set-key (kbd "M-j") 'avy-goto-word-1)
(global-set-key (kbd "M-a") 'sp-splice-sexp)
(global-set-key (kbd "M-A") 'sp-rewrap-sexp)

;; key-chord
(key-chord-define-global "yy" nil)
(key-chord-define-global "xx" nil)

(provide 'my-keymap)
;;; my-keymap.el ends here
