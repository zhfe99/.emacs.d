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
  ("<left>" buf-move-left "move left")
  ("<down>" buf-move-down "move down")
  ("<up>" buf-move-up "move up")
  ("<right>" buf-move-right "move right")
  ("C-h" my-move-splitter-left "splitter left")
  ("C-j" my-move-splitter-down "splitter down")
  ("C-k" my-move-splitter-up "splitter up")
  ("C-l" my-move-splitter-right "splitter right")
  ("H" my-push-window-left "push left")
  ("J" my-push-window-down "push down")
  ("K" my-push-window-up "push up")
  ("L" my-push-window-right "push right")
  ("o" ace-select-window "select")
  ("u" winner-undo "undo")
  ("r" winner-redo "redo" :exit t)
  ("1" delete-other-windows "delete others")
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
  ("e" eval-last-sexp "eval" :exit t)
  ("s" sp-splice-sexp "splice" :exit t)
  ("r" sp-rewrap-sexp "rewarp" :exit t)
  ("i" change-inner "change inner" :exit t)
  ("o" change-outer "change outer" :exit t)
  ("k" sp-kill-hybrid-sexp "kill-sexp" :exit t))

;; multiple-cursors
(defhydra hydra-mc ()
  "multiple-cursors"
  ("." mc/mark-next-like-this "next-like")
  ("," mc/mark-previous-like-this "previous-like")
  ("a" mc/edit-beginnings-of-lines "begin" :exit t)
  ("e" mc/edit-ends-of-lines "end" :exit t)
  ("l" mc/edit-lines "line" :exit t)
  ("r" mc/mark-all-in-region "all-in-region" :exit t)
  ("m" mc/mark-all-like-this "all-like" :exit t))

;; term
(defhydra hydra-term (:color blue)
  "term"
  ("n" multi-term "new term")
  ("t" multi-term-next "next term")
  ("p" multi-term-prev "previous term")
  ("m" matlab-shell "matlab")
  ("e" eshell "eshell")
  ("a" ansi-term "ansi-term")
  ("h" counsel-yank-zsh-history "zsh-history"))

;; jump
(defhydra hydra-jump (:color blue)
  "jump"
  ("c" avy-goto-char "char")
  ("j" avy-goto-word-1 "word")
  ("l" avy-goto-line "line")
  ("k" org-clock-goto "clock")
  ("'" avy-pop-mark "pop-mark")
  ("i" counsel-imenu "counsel-imenu")
  ("I" ivy-imenu-goto "ivy-imenu")
  ("f" iy-go-up-to-char "iy-go-up-to-char")
  ("F" iy-go-up-to-char-backward "iy-go-up-to-char-backward"))

;; open
(defhydra hydra-open (:color blue)
  "open"
  ("o" ivy-switch-buffer "buffer" :exit t)
  ("d" counsel-goto-recent-directory "directory" :exit t)
  ("f" find-file-in-project "file" :exit t)
  ("M-o" counsel-find-file "file" :exit t)
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
  ("g" magit-status-fullscreen "magit" :exit t)
  ("G" magit-status "magit-status" :exit t)
  ("i" my-goto-git-gutter+ "git-gutter" :exit t)
  ("p" git-gutter+-previous-hunk "previous")
  ("=" git-gutter+-show-hunk "show")
  ("n" git-gutter+-next-hunk "next")
  ("s" git-gutter+-stage-hunks "stage")
  ("c" git-gutter+-stage-and-commit "commit")
  ("t" git-timemachine "time-machine" :exit t)
  ("a" git-gutter+-stage-and-commit-whole-buffer "whole"))

;; transpose
(defhydra hydra-transpose ()
  "transpose"
  ("l" transpose-lines "line")
  ("w" transpose-words "word")
  ("c" transpose-chars "char")
  ("s" transpose-sexps "sexp"))

;; line
(defhydra hydra-line (:color pink)
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
  ("o" crux-smart-open-line "open line")
  ("{" backward-paragraph "backward")
  ("}" forward-paragraph "forward")
  ("k" kill-whole-line "kill")
  ("w" avy-goto-word-1 "word")
  ("f" forward-char "forward char")
  ("w" forward-word "forward word")
  ("b" backward-char "backward char")
  ("d" crux-duplicate-and-comment-current-line-or-region "duplicate")
  ("W" backward-word "backward word")
  ("a" crux-move-beginning-of-line "head")
  ("e" move-end-of-line "end")
  ("/" undo-tree-undo "undo")
  ("'" pop-to-mark-command "pop")
  ("m" set-mark-command "mark")
  ("q" nil))

;; region
(defhydra hydra-region ()
  "region"
  ("a" my-align-comment "align-comment" :exit t)
  ("=" er/expand-region "expand")
  ("-" er/contract-region "contract")
  ("g" prelude-google "google" :exit t)
  ("b" prelude-bing "bing" :exit t)
  ("s" helm-swoop "swoop" :exit t)
  ("n" my-narrow-or-widen-dwim "narrow"))

;; misc
(defhydra hydra-misc (:color blue)
  "misc"
  ("c" my-cleanup "cleanup")
  ("e" ediff "ediff")
  ("l" linum-mode "linum")
  ("h" helm-man-woman "man")
  ("b" edebug-defun "debug")
  ("o" crux-sudo-edit "sudo")
  ("i" irony-install-server "irony")
  ("s" swiper-all "swiper all"))

;; org
(defhydra hydra-org (:color red :hint nil)
  "
Navigation^
---------------------------------------------------------
_j_ next heading
_k_ prev heading
_h_ next heading (same level)
_l_ prev heading (same level)
_u_p higher heading
_g_o to
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-forward-heading-same-level)
  ("l" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("g" org-goto :exit t))

(provide 'my-hydra)
;;; my-hydra.el ends here
