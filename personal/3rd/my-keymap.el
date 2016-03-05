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
;; available:
;; q w e r t y u i [ ]
;; h l '
;; z b v /
;; notice these keys might not be accessible in terminal
(global-set-key (kbd "H-e") 'ediff-files)
(global-set-key (kbd "H-o") 'reveal-in-finder)
(global-set-key (kbd "H-a") 'my-open-todo-org)
(global-set-key (kbd "H-A") 'my-open-info-org)
(global-set-key (kbd "H-s") 'my-find-file-as-sudo)
(global-set-key (kbd "H-u") 'git-push-remote-origin)
(global-set-key (kbd "H-p") 'my-switch-to-current-on-server-or-local)
(global-set-key (kbd "H-d") 'ace-duplicate-buffer)
(global-set-key (kbd "H-f") 'find-name-dired)
(global-set-key (kbd "H-g") 'rgrep)
(global-set-key (kbd "H-k") 'sp-kill-hybrid-sexp)
(global-set-key (kbd "H-x") 'package-list-packages)
(global-set-key (kbd "H-c") 'org-capture)
(global-set-key (kbd "H--") 'my-align-comment)
(global-set-key (kbd "H-j") 'org-clock-goto)
(global-set-key (kbd "H-J") 'org-show-todo-tree)
(global-set-key (kbd "H-m") 'matlab-shell)
(global-set-key (kbd "H-n") 'my-narrow-or-widen-dwim)
(global-set-key (kbd "H-,") 'diff-hl-previous-hunk)
(global-set-key (kbd "H-.") 'diff-hl-next-hunk)
(global-set-key (kbd "<H-down>") 'my-push-window-down)
(global-set-key (kbd "<H-up>") 'my-push-window-up)
(global-set-key (kbd "<H-left>") 'my-push-window-left)
(global-set-key (kbd "<H-right>") 'my-push-window-right)
(global-set-key (kbd "H-4") 'my-goto-window-conf-4)

;; use key in iterm2 on OSX
(global-set-key (kbd "\e[45;1~") 'delete-other-windows)
(global-set-key (kbd "\e[45;2~") 'split-window-below-and-move-there-dammit)
(global-set-key (kbd "\e[45;3~") 'split-window-right-and-move-there-dammit)
(global-set-key (kbd "\e[45;0~") 'ace-delete-window)
(global-set-key (kbd "\e[46;4~") 'mc/mark-previous-like-this)
(global-set-key (kbd "\e[46;5~") 'mc/mark-next-like-this)
(global-set-key (kbd "\e[46;3~") 'er/expand-region)
(global-set-key (kbd "\e[46;A~") 'my-push-window-up)
(global-set-key (kbd "\e[46;B~") 'my-push-window-down)
(global-set-key (kbd "\e[46;C~") 'my-push-window-left)
(global-set-key (kbd "\e[46;D~") 'my-push-window-right)
(global-set-key (kbd "\e[27;5;39~") 'helm-swoop)
(global-set-key (kbd "\e[46;3;A~") 'buf-move-up)
(global-set-key (kbd "\e[46;3;B~") 'buf-move-down)
(global-set-key (kbd "\e[46;3;C~") 'buf-move-left)
(global-set-key (kbd "\e[46;3;D~") 'buf-move-right)

;; C-
;; Some of the key might not be available in terminal
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-'") 'helm-swoop)
(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key isearch-mode-map (kbd "C-'") 'helm-swoop-from-isearch)
(global-unset-key (kbd "C-t"))
(global-set-key (kbd "C-t l") 'transpose-lines)
(global-set-key (kbd "C-t w") 'transpose-words)
(global-set-key (kbd "C-t c") 'transpose-chars)
(global-set-key (kbd "C-t s") 'transpose-sexps)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "C-j") 'indent-new-comment-line)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below-and-move-there-dammit)
(global-set-key (kbd "C-3") 'split-window-right-and-move-there-dammit)
(global-set-key (kbd "C-4") 'my-save-window-conf-4)
(global-set-key (kbd "C-0") 'ace-delete-window)

;; C-x
(global-set-key (kbd "C-x o") 'ace-select-window)
(global-set-key (kbd "C-x O") 'ace-swap-window)

;; C-c
(define-key prelude-mode-map (kbd "C-c i") nil)
(global-set-key (kbd "C-c i") 'helm-imenu-anywhere)
(global-set-key (kbd "C-c M-.") 'find-tag)
(global-set-key (kbd "C-c M-*") 'pop-tag-mark)
(global-set-key (kbd "C-c M-j") 'avy-pop-mark)

;; H-M-
(global-set-key (kbd "<H-M-up>") 'buf-move-up)
(global-set-key (kbd "<H-M-down>") 'buf-move-down)
(global-set-key (kbd "<H-M-left>") 'buf-move-left)
(global-set-key (kbd "<H-M-right>") 'buf-move-right)

;; M-
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-l") 'dired-jump)
;; (global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-g") 'avy-goto-line)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-m") 'iy-go-up-to-char)
(global-set-key (kbd "M-M") 'iy-go-to-char-backward)
(global-set-key (kbd "<M-up>") 'move-text-up)
(define-key input-decode-map "\e\e[A" [(meta up)])
(define-key input-decode-map "\e\e[B" [(meta down)])
(eval-after-load "smartparens-mode"
  '(define-key minor-mode-map (kbd "<M-up>") 'move-text-up))
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "M-k") 'kill-whole-line)
(keymap-unset-key '[M-down] "smartparens-mode")
(keymap-unset-key '[M-up] "smartparens-mode")
(keymap-unset-key '[M-s] "smartparens-mode")
(keymap-unset-key (kbd "M-s") "smartparens-mode")
(keymap-unset-key (kbd "M-r") "smartparens-mode")
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-r") 'revert-buffer-no-confirm)
(global-set-key (kbd "M-h") 'helm-mini)
(global-set-key (kbd "M-o") 'helm-find-files)
(global-set-key (kbd "M-t") 'multi-term-next)
(global-set-key (kbd "M-T") 'multi-term)
(global-set-key (kbd "M-u") 'git-push-remote-origin)
(global-set-key (kbd "M-'") 'helm-flycheck)
(global-set-key (kbd "M-.") 'helm-etags+-select)
(global-set-key (kbd "M-*") 'helm-etags+-history)
(global-set-key (kbd "M-,") 'helm-etags+-history-go-back)
(global-set-key (kbd "M-j") 'avy-goto-word-1)
(global-set-key (kbd "M-J") 'avy-goto-char)
(global-set-key (kbd "M-a") 'sp-splice-sexp)
(global-set-key (kbd "M-A") 'sp-rewrap-sexp)
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c c") 'subword-capitalize)
(global-set-key (kbd "M-c u") 'subword-upcase)
(global-set-key (kbd "M-c l") 'subword-downcase)

;; function key
(global-set-key (kbd "<f1>") 'org-agenda-list)
(global-set-key (kbd "<f5>") 'kmacro-set-counter)

;; key-chord
(key-chord-define-global "yy" nil)
(key-chord-define-global "xx" nil)
(key-chord-define-global "jj" 'linum-mode)

(provide 'my-keymap)
;;; my-keymap.el ends here
