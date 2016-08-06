;;; package --- Summary
;;; Commentary:

;; My hydra key binding.

;;; Code:
(use-package hydra)

;; window
(defhydra hydra-window (:hint nil)
  "window"
  ("h" windmove-left "move left")
  ("j" windmove-down "move down")
  ("k" windmove-up "move up")
  ("l" windmove-right "move right")
  ("M-h" buf-move-left "move left")
  ("M-j" buf-move-down "move down")
  ("M-k" buf-move-up "move up")
  ("M-l" buf-move-right "move right")
  ("H" my-push-window-left "push left")
  ("J" my-push-window-down "push down")
  ("K" my-push-window-up "push up")
  ("L" my-push-window-right "push right")
  ("o" ace-select-window "select")
  ("u" (progn
         (winner-undo)
         (setq this-command 'winner-undo))
   "undo")
  ("r" winner-redo "redo")
  ("2" split-window-below-and-move-there-dammit "split below")
  ("3" split-window-right-and-move-there-dammit "split down")
  ("b" balance-windows "balance")
  ("+" text-scale-increase "increase")
  ("-" text-scale-decrease "decrease")
  ("0" (text-scale-adjust 0) "reset")
  ("s" (lambda () (interactive) (ace-window 4)) "swap")
  ("d" (lambda () (interactive) (ace-window 16)) "delete")
  ("q" nil))

;; change case
(defhydra hydra-case ()
  "case"
  ("c" subword-capitalize "captial")
  ("u" subword-upcase "upcase")
  ("l" subword-downcase "downcase")
  ("b" backward-word "backword")
  ("f" forward-word "forward")
  ("q" nil))

;; smartparens
(defhydra hydra-sp ()
  "smartparens"
  ("f" sp-forward-sexp "forward")
  ("d" sp-down-sexp "down")
  ("u" sp-backward-up-sexp "backward up")
  ("b" sp-backward-sexp "backward")
  ("p" sp-backward-down-sexp "backward-down")
  ("n" sp-up-sexp "up")
  ("<down>" sp-next-sexp "next")
  ("<up>" sp-previous-sexp "previous")
  ("s" sp-splice-sexp "splice" :exit t)
  ("r" sp-rewrap-sexp "rewarp" :exit t)
  ("k" sp-kill-hybrid-sexp "kill-sexp" :exit t))

;; multiple-cursors
(defhydra hydra-mc ()
  "multiple-cursors"
  ("." mc/mark-next-like-this "next-like")
  ("," mc/mark-previous-like-this "previous-like")
  ("a" mc/mark-all-like-this "all-like")
  )

;; term
(defhydra hydra-term (:color red :hint nil)
  "term"
  ("t" multi-term "new term" :exit t)
  ("n" multi-term-next "next term")
  ("p" multi-term-prev "previous term")
  ("m" matlab-shell "matlab" :exit t)
  ("e" eshell "eshell" :exit t)
  ("a" ansi-term "ansi-term" :exit t)
  ("h" counsel-yank-zsh-history "zsh-history" :exit t))

;; jump
(defhydra hydra-jump ()
  "jump"
  ("c" avy-goto-char "char" :exit t)
  ("w" avy-goto-word-1 "word" :exit t)
  ("l" avy-goto-line "line" :exit t)
  ("k" org-clock-goto "clock" :exit t)
  ("'" avy-pop-mark "pop-mark")
  ("i" counsel-imenu "counsel-imenu" :exit t)
  ("I" ivy-imenu-goto "ivy-imenu" :exit t)
  ("m" iy-go-up-to-char "iy-go-up-to-char" :exit t)
  ("M" iy-go-up-to-char-backward "iy-go-up-to-char-backward" :exit t))

;; open
(defhydra hydra-open ()
  "open"
  ("b" ivy-switch-buffer "buffer" :exit t)
  ("d" counsel-goto-recent-directory "directory" :exit t)
  ("f" counsel-find-file "file" :exit t)
  ("a" org-agenda-list "agenda" :exit t)
  ("t" org-todo-list "todo" :exit t)
  ("c" org-capture "capture" :exit t)
  ("u" ace-duplicate-current-buffer-in-other-window "duplicate" :exit t)
  ("p" my-switch-to-current-on-server-or-local "same-file" :exit t)
  ("z" reveal-in-osx-finder "reveal-in-osx-finder"))

;; tag
(defhydra hydra-tag ()
  "tag"
  ("." helm-etags+-select "helm-etags" :exit t)
  ("/" helm-etags+-history "etags history")
  ("," helm-etags+-history-go-back "etags history go back")
  (">" find-tag "tag"))

;; git
(defhydra hydra-git ()
  "git"
  ("g" magit-status "magit-status" :exit t)
  ("i" my-goto-git-gutter "git-gutter" :exit t)
  ("p" git-gutter:previous-hunk "previous hunk")
  ("=" vc-diff "diff")
  ("n" git-gutter:next-hunk "next hunk"))

;; transpose
(defhydra hydra-transpose ()
  "transpose"
  ("l" transpose-lines "line")
  ("w" transpose-words "word")
  ("c" transpose-chars "char")
  ("s" transpose-sexps "sexp"))

;; line
(defhydra hydra-line ()
  "line"
  ("<up>" move-text-up "switch up")
  ("<down>" move-text-down "switch down")
  ("p" previous-line "move up")
  ("n" next-line "move next")
  ("<" beginning-of-buffer "begin")
  (">" end-of-buffer "end")
  ("v" scroll-up-command "scroll up")
  ("V" scroll-down-command "scroll down")
  ("l" recenter-top-bottom "recenter")
  ("k" kill-whole-line "kill"))

;; region
(defhydra hydra-region ()
  "region"
  ("a" my-align-comment "align-comment" :exit t)
  ("=" er/expand-region "expand")
  ("-" er/contract-region "contract")
  ("g" prelude-google "google" :exit t)
  ("b" prelude-bing "bing" :exit t)
  ("n" my-narrow-or-widen-dwim "narrow")
  ("l" mc/edit-beginnings-of-lines "mc line" :exit t))

;; misc
(defhydra hydra-misc ()
  "misc"
  ("c" my-cleanup "cleanup" :exit t)
  ("e" ediff "ediff" :exit t)
  )

(provide 'my-hydra)
;;; my-hydra.el ends here
