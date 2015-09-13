;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'prelude-evil)

;; evil key
(key-chord-define-global "jj" nil)
(key-chord-define-global "yy" nil)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

;; disable evil in some mode
(evil-set-initial-state 'magit-log-edit-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'magit-popup-mode 'emacs)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; evil-escape
(prelude-require-package 'evil-escape)
(setq-default evil-escape-delay 0.2)
(setq evil-escape-excluded-major-modes '(dired-mode))
(setq-default evil-escape-key-sequence "kj")
(evil-escape-mode 1)

;; Move back the cursor one position when exiting insert mode
(setq evil-move-cursor-back t)

;; My frequently used commands are listed here
(setq evil-leader/leader "SPC")
(prelude-require-package 'evil-leader)
;(evil-leader/set-key
  ;; SPACE will evil-ace-jump-word-mode by default
 ;;  "al" 'evil-ace-jump-line-mode ; ,al for Ace Jump (line)
;;   "ac" 'evil-ace-jump-char-mode ; ,ac for Ace Jump (char)
;;   "j" 'avy-goto-word-1 ; ,ac for Ace Jump (char)
;;   "." 'projectile-find-tag ; ,ac for Ace Jump (char)
;;   "-" '(lambda ()
;;         (interactive)
;;         (align-regexp
;;         (region-beginning)
;;         (region-end)
;;         (concat "\\(\\s-*\\)" " -"))) ; ,ac for Ace Jump (char)
;; )
;; (global-evil-leader-mode)

(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

(provide 'my-evil)
;;; my-evil.el ends here
