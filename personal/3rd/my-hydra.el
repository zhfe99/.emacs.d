;;; package --- Summary
;;; Commentary:

;; My hydra key binding.

;;; Code:
(use-package hydra)

;; window
(defhydra hydra-window (:hint nil)
  "
^Switch^   ^Delete^   ^Boundary^     ^Swap^     ^Split^    ^Text^
^======^===^====^=====^========^=====^====^=====^=====^====^====^======
_h_ left   _H_ left   _C-h_ left     _←_ left   _2_ below  _=_ increase
_j_ down   _J_ down   _C-j_ down     _↓_ down   _3_ right  _-_ decrease
_k_ up     _K_ up     _C-k_ up       _↑_ up     _u_ undo   _0_ default
_l_ right  _L_ right  _C-l_ right    _→_ right  _r_ redo
_o_ ace    _1_ other  _b_   balance  _s_ ace
^^         _d_ ace
"
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
(defhydra hydra-jump (:color blue :hint nil)
  "
^Jump^    ^Move^    ^Del^    ^Imenu^  ^Org^
^====^====^=========^===^====^=====^==^===^====
_j_ word  _f_ ford  _z_ zop  _i_ ivy  _k_ clock
_c_ char  _F_ back  _Z_ zap  _I_ my
_l_ line
_'_ pop
"
  ("c" avy-goto-char)
  ("j" avy-goto-word-1)
  ("l" avy-goto-line)
  ("k" org-clock-goto)
  ("'" avy-pop-mark :exit nil)
  ("i" counsel-imenu)
  ("I" my-ivy-imenu-goto)
  ("f" iy-go-up-to-char)
  ("F" iy-go-up-to-char-backward)
  ("z" zop-to-char)
  ("Z" zap-to-char))

;; open
(defhydra hydra-open (:color blue :hint nil)
  "
^Buffer^    ^File^       ^Org^
^======^====^====^=======^===^======
_o_ open    _M-o_ open   _t_ todo
_s_ save    _f_ project  _a_ agenda
_k_ kill    _z_ reveal   _c_ capture
_b_ bury    _d_ dired
_r_ revert  _p_ machine
_u_ dupe"
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
(defhydra hydra-git (:hint nil)
  "
^Over^    ^Jump^    ^Operation^
^====^====^====^====^=========^
_g_ full  _i_ goto  _s_ stage
_G_ orig  _p_ prev  _c_ commit
^^        _n_ next  _a_ all
^^        _v_ show  _t_ time"
  ("g" magit-status-fullscreen :exit t)
  ("G" magit-status :exit t)
  ("i" my-goto-git-gutter+ :exit t)
  ("p" git-gutter+-previous-hunk)
  ("v" git-gutter+-show-hunk)
  ("n" git-gutter+-next-hunk)
  ("s" git-gutter+-stage-hunks)
  ("c" git-gutter+-stage-and-commit)
  ("t" git-timemachine :exit t)
  ("a" git-gutter+-stage-and-commit-whole-buffer))

;; transpose
(defhydra hydra-transpose ()
  "transpose"
  ("l" transpose-lines "line")
  ("w" transpose-words "word")
  ("c" transpose-chars "char")
  ("s" transpose-sexps "sexp"))

(defhydra hydra-vi (:pre (set-cursor-color "#e52b50")
                         :post (set-cursor-color "#ffffff")
                         :color pink :hint nil)
  "
^Arrow^    ^Scroll^    ^Delete^  ^Move^
^=====^====^======^====^======^==^====^
_h_ left   _C-v_ up    _d_ del   _
_j_ down   _M-v_ down  _x_ char
_k_ up     ^^          _u_ undo
_l_ right
"
  ;; movement
  ("w" forward-word)
  ("b" backward-word)
  ;; scrolling
  ("C-v" scroll-up-command nil)
  ("M-v" scroll-down-command nil)
  ("v" recenter-top-bottom)
  ;; arrows
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("l" forward-char)
  ;; delete
  ("x" delete-char)
  ("d" hydra-vi-del/body "del" :exit t)
  ("u" undo-tree-undo)
  ;; should be generic "open"
  ("r" push-button "open")
  ("." hydra-repeat)
  ;; bad
  ("m" set-mark-command "mark")
  ("a" move-beginning-of-line "beg")
  ("e" move-end-of-line "end")
  ("y" kill-ring-save "yank" :exit t)
  ;; exit points
  ("q" nil "ins")
  ("C-n" (forward-line 1) nil :exit t)
  ("C-p" (forward-line -1) nil :exit t))

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
_u_ higher heading
_g_ to
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-forward-heading-same-level)
  ("l" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("g" org-goto :exit t))

(provide 'my-hydra)
;;; my-hydra.el ends here
