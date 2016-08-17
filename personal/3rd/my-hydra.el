;;; package --- Summary
;;; Commentary:

;; My hydra key binding.

;;; Code:
(use-package hydra)

;; window
(defhydra hydra-window (:hint nil)
  "
switch: _h_ _j_ _k_ _l_ _o_
push:   _H_ _J_ _K_ _L_
size:   _C-h_ _C-j_ _C-k_ _C-l_ _b_
move:   _↑_ _→_ _↓_ _←_ _s_
split:  _2_ _3_ _u_ _r_
text:   _0_ _=_ _-_
delete: _1_ _d_"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("<left>" buf-move-left)
  ("<down>" buf-move-down)
  ("<up>" buf-move-up)
  ("<right>" buf-move-right)
  ("C-h" my-move-splitter-left)
  ("C-j" my-move-splitter-down)
  ("C-k" my-move-splitter-up)
  ("C-l" my-move-splitter-right)
  ("H" my-push-window-left)
  ("J" my-push-window-down)
  ("K" my-push-window-up)
  ("L" my-push-window-right)
  ("o" ace-select-window)
  ("u" winner-undo)
  ("r" winner-redo :exit t)
  ("1" delete-other-windows)
  ("2" split-window-below-and-move-there-dammit)
  ("3" split-window-right-and-move-there-dammit)
  ("b" balance-windows)
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-adjust 0))
  ("s" (lambda () (interactive) (ace-window 4)))
  ("d" (lambda () (interactive) (ace-window 16)))
  ("q" nil))

;; change case
(defhydra hydra-case (:color pink)
  "case"
  ("c" subword-capitalize "captial")
  ("u" subword-upcase "upcase")
  ("l" subword-downcase "downcase")
  ("b" backward-word "backword")
  ("f" forward-word "forward")
  ("q" nil "quit"))

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
  ("'" avy-pop-mark "pop-mark" :exit nil)
  ("i" counsel-imenu "counsel-imenu")
  ("I" ivy-imenu-goto "ivy-imenu")
  ("f" iy-go-up-to-char "iy-go-up-to-char")
  ("F" iy-go-up-to-char-backward "iy-go-up-to-char-backward")
  ("z" zop-to-char "zop-to-char")
  ("Z" zap-to-char "zap-to-char"))

;; open
(defhydra hydra-open (:color blue :hint nil)
  "
^buffer^     ^file^        ^org^
_o_: open    _M-o_: open   _t_: todo
_s_: save    _f_: project  _a_: agenda
_k_: kill    _z_: reveal   _c_: capture
_b_: bury    _d_: dired
_r_: revert
_d_: dupe"
  ("o" ivy-switch-buffer)
  ("d" counsel-goto-recent-directory)
  ("f" find-file-in-project)
  ("M-o" counsel-find-file)
  ("a" org-agenda-list)
  ("t" org-todo-list)
  ("s" save-buffer)
  ("b" bury-buffer)
  ("r" revert-buffer-no-confirm)
  ("k" kill-this-buffer)
  ("c" org-capture)
  ("u" ace-duplicate-current-buffer-in-other-window)
  ("p" my-switch-to-current-on-server-or-local)
  ("z" reveal-in-osx-finder))

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

;; gdb
(defhydra hydra-gdb (:color pink)
  "gdb"
  ("r" gdb "start" :exit t)
  ("B" gdb-display-breakpoints-buffer "display-breakpoints-buffer")
  ("b" gud-break "break" :exit t)
  ("g" gdb-display-gdb-buffer "display-gdb-buffer")
  ("l" gdb-display-locals-buffer "display-locals-buffer")
  ("m" gdb-many-windows "many-windoes")
  ("o" gdb-display-io-buffer "display-io-buffer")
  ("S" gdb-display-stack-buffer "display-stack-buffer")
  ("c" gud-cont "cont")
  ("n" gud-next "next")
  ("l" gud-refresh "refresh")
  ("f" gud-finish "finish")
  ("u" gud-up "up")
  ("d" gud-down "down")
  ("s" gud-step "step")
  ("q" nil "quit"))

;; misc
(defhydra hydra-misc (:color blue)
  "misc"
  ("c" my-cleanup "cleanup")
  ("e" ediff "ediff")
  ("l" linum-mode "linum")
  ("h" helm-man-woman "man")
  ("b" edebug-defun "edebug")
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
