;;; package --- Summary
;;; Commentary:

;; My hydra key binding.

;;; Code:
(use-package hydra)

;; window
(defhydra hydra-window (:hint nil :idle 1.5)
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
(defhydra hydra-case (:color pink :idle 1.5)
  "case"
  ("c" subword-capitalize "captial")
  ("u" subword-upcase "upcase")
  ("l" subword-downcase "downcase")
  ("b" backward-word "backword")
  ("f" forward-word "forward")
  ("q" nil "quit"))

;; smartparens
(defhydra hydra-sp (:color pink :hint nil :idle 1.5)
  "
^Move^    ^Kill^     ^Wrap^      ^Lisp^
^====^====^====^=====^====^======^====^
_f_ ford  _i_ in     _s_ splice  _e_ eval
_b_ back  _o_ out    _r_ rewarp
_p_ in    _k_ kill   _l_ slurp
_n_ out   _d_ ford   _a_ barf
^^        _D_ back"
  ("f" sp-forward-sexp)
  ("P" sp-down-sexp)
  ("N" sp-backward-up-sexp)
  ("b" sp-backward-sexp)
  ("p" sp-backward-down-sexp)
  ("n" sp-up-sexp)
  ("<down>" sp-next-sexp)
  ("<up>" sp-previous-sexp)
  ("e" eval-last-sexp :exit t)
  ("s" sp-splice-sexp :exit t)
  ("r" sp-rewrap-sexp :exit t)
  ("l" sp-slurp-hybrid-sexp)
  ("a" sp-forward-barf-sexp)
  ("i" change-inner :exit t)
  ("o" change-outer :exit t)
  ("k" sp-kill-hybrid-sexp :exit t)
  ("d" sp-kill-sexp)
  ("D" sp-backward-kill-sexp)
  ("q" nil))

;; multiple-cursors
(defhydra hydra-mc (:hint nil :idle 1.5)
  "
^Select^       ^Line^    ^Multi^
^======^=======^====^====^=====^=
_._ next       _l_ line  _r_ all
_>_ skip next  _a_ head  _m_ mark
_,_ prev       _e_ tail
_<_ skip prev"
  ("." mc/mark-next-like-this)
  (">" mc/skip-to-next-like-this)
  ("," mc/mark-previous-like-this)
  ("<" mc/skip-to-previous-like-this)
  ("a" mc/edit-beginnings-of-lines :exit t)
  ("e" mc/edit-ends-of-lines :exit t)
  ("l" mc/edit-lines :exit t)
  ("r" mc/mark-all-in-region :exit t)
  ("m" mc/mark-all-like-this :exit t))

;; term
(defhydra hydra-term (:color blue :hint nil :idle 1.5)
  "
^Select^  ^Mode^      ^History^
^======^==^====^======^====^=====
_t_ next  _m_ matlab  _h_ zsh
_n_ new   _e_ eshell  _H_ bash
_p_ prev  _a_ ansi"
  ("n" multi-term)
  ("t" multi-term-next)
  ("p" multi-term-prev)
  ("m" matlab-shell)
  ("e" eshell)
  ("a" ansi-term)
  ("h" counsel-yank-zsh-history)
  ("H" counsel-yank-bash-history))

;; jump
(defhydra hydra-jump (:color blue :hint nil :idle 1.5)
  "
^Jump^    ^Move^    ^Del^    ^Imenu^  ^Org^
^====^====^=========^===^====^=====^==^===^====
_j_ word  _f_ ford  _z_ zop  _i_ ivy  _k_ clock
_c_ char  _F_ back  _Z_ zap  _I_ my
_l_ line
_'_ pop"
  ("c" avy-goto-char)
  ("j" avy-goto-word-1)
  ("l" avy-goto-line)
  ("k" org-clock-goto)
  ("'" avy-pop-mark :exit nil)
  ("i" counsel-imenu)
  ("I" my-ivy-imenu-goto)
  ("f" iy-go-up-to-char)
  ("F" iy-go-to-char-backward)
  ("z" zop-to-char)
  ("Z" zap-to-char))

;; open
(defhydra hydra-open (:color blue :hint nil :idle 1.5)
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
(defhydra hydra-transpose (:idle 1.5)
  "transpose"
  ("l" transpose-lines "line")
  ("w" transpose-words "word")
  ("c" transpose-chars "char")
  ("s" transpose-sexps "sexp"))

(defhydra hydra-vi (:pre (set-cursor-color "#e52b50")
                         :post (set-cursor-color "#ffffff")
                         :color pink :hint nil :idle 1.5)
  "
^Char^     ^Word^    ^Line^     ^Buffer^  ^Other^
^====^=====^====^====^====^=====^====^====^=====^===
_h_ left   _w_ ford  _a_ head   _g_ head  _m_ mark
_j_ down   _b_ back  _e_ tail   _G_ tail  _u_ undo
_k_ up     _dw_ del  _↑_ up     _V_ up    _._ repeat
_l_ right  ^^        _↓_ down   _v_ down
_x_ del    ^^        _yy_ yank"
  ;; movement
  ("w" forward-word)
  ("b" backward-word)
  ;; scrolling
  ("v" scroll-up-command nil)
  ("V" scroll-down-command nil)
  ;; arrows
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("l" forward-char)
  ;; delete
  ("x" delete-char)
  ("dw" kill-word)
  ("dd" crux-kill-whole-line)
  ("u" undo-tree-undo)
  ;; should be generic "open"
  ("r" push-button)
  ("." hydra-repeat)
  ;; buffer
  ("g" beginning-of-buffer)
  ("G" end-of-buffer)
  ;; bad
  ("m" set-mark-command)
  ("yy" crux-duplicate-current-line-or-region)
  ;; line
  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("<up>" move-text-up)
  ("<down>" move-text-down)
  ;; exit points
  ("q" nil))

;; region
(defhydra hydra-region (:idle 1.5)
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

;; edit
(defhydra hydra-edit (:color blue :hint nil)
  "
_q_ fill"
  ("q" fill-paragraph)
  ("c" my-cleanup "cleanup")
  ("e" ediff "ediff")
  ("l" linum-mode "linum")
  ("h" helm-man-woman "man")
  ("b" edebug-defun "edebug")
  ("o" crux-sudo-edit "sudo")
  ("i" irony-install-server "irony")
  ("s" swiper-all "swiper all"))

;; org
(defhydra hydra-org (:color red :hint nil :idle 1.5)
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
