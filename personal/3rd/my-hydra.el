;;; package --- Summary
;;; Commentary:

;; My hydra key binding.

;;; Code:
(use-package hydra)

;; window
(defhydra hydra-window (:hint nil :idle 1.5)
  "
^Focus^    ^Delete^   ^Boundary^     ^Swap^     ^Split^    ^Text^
^=====^====^====^=====^========^=====^====^=====^=====^====^====^======
_h_ left   _H_ left   _C-h_ left     _←_ left   _2_ below  _=_ increase
_j_ down   _J_ down   _C-j_ down     _↓_ down   _3_ right  _-_ decrease
_k_ up     _K_ up     _C-k_ up       _↑_ up     _u_ undo   _0_ default
_l_ right  _L_ right  _C-l_ right    _→_ right  _r_ redo
_o_ ace    _1_ other  _b_   balance  _s_ ace
^^         _d_ ace"
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
  ("H" my-push-window-left :exit t)
  ("J" my-push-window-down :exit t)
  ("K" my-push-window-up :exit t)
  ("L" my-push-window-right :exit t)
  ("o" ace-select-window)
  ("u" winner-undo)
  ("r" winner-redo :exit t)
  ("1" delete-other-windows :exit t)
  ("2" split-window-below-and-move-there-dammit)
  ("3" split-window-right-and-move-there-dammit)
  ("4" my-save-window-conf-1 :exit t)
  ("5" my-goto-window-conf-1 :exit t)
  ("b" balance-windows :exit t)
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-adjust 0))
  ("s" (lambda () (interactive) (ace-window 4)) :exit t)
  ("d" (lambda () (interactive) (ace-window 16)) :exit t)
  ("q" nil))

;; open
(defhydra hydra-open (:color blue :hint nil :idle 1.5)
  "
^Buffer^    ^File^       ^Special^
^======^====^====^=======^===^======
_k_ kill    _f_ project  _ss_ scratch
_b_ bury    _z_ reveal   _sc_ capture
_r_ revert  _d_ dired    _sa_ agenda
_u_ dupe    _m_ machine  _st_ todo
_U_ dupe2   ^^           _si_ info"
  ("k" kill-this-buffer)
  ("b" bury-buffer)
  ("r" my-revert-buffer)
  ("f" find-file-in-project)
  ("z" reveal-in-osx-finder)
  ("d" counsel-goto-recent-directory)
  ("u" my-duplicate-current-buffer-in-ace-window)
  ("U" my-duplicate-ace-buffer-in-current-window)
  ("m" my-switch-to-current-on-server-or-local)
  ("sc" org-capture)
  ("sa" org-agenda-list)
  ("st" org-todo-list)
  ("si" (lambda () (interactive) (find-file "~/code/mine/org/info.org")))
  ("ss" (lambda () (interactive) (switch-to-buffer "*scratch*"))))

;; jump
(defhydra hydra-jump (:color blue :hint nil :idle 1.5)
  "
^Avy^     ^Scroll^  ^Imenu^  ^Bookmark^  ^Misc^
^===^=====^======^==^=====^==^========^==^====^===
_c_ char  _v_ down  _i_ list  _b_ jump    _k_ clock
_l_ line  _V_ up    _I_ my    _B_ set     _'_ pop"
  ("c" avy-goto-char)
  ("l" avy-goto-line)
  ("k" org-clock-goto)
  ("'" avy-pop-mark)
  ("i" counsel-imenu)
  ("I" my-ivy-imenu-goto)
  ("b" bookmark-jump)
  ("B" bookmark-set)
  ("v" my-scroll-down-half :exit nil)
  ("V" my-scroll-up-half :exit nil)
  ("q" nil))

;; toggle
(defhydra hydra-toggle (:color blue :hint nil :idle 1.5)
  "
^Mode^      ^Clean^    ^Theme^   ^Edit^
^=====^=====^=====^====^=====^===^====^==
_l_ linum   _p_ proj   _t_ load  _q_ fill
_v_ visual  _W_ space  ^^        _s_ sudo
_w_ space   ^^         ^^        _e_ ediff"
  ("q" fill-paragraph)
  ("e" ediff)
  ("l" linum-mode)
  ("t" counsel-load-theme)
  ("s" crux-sudo-edit)
  ("p" my-cleanup)
  ("W" whitespace-cleanup)
  ("w" whitespace-mode)
  ("v" visual-line-mode))

;; edit
(defhydra hydra-edit (:color blue :hint nil :idle 1.5)
  "
^Line^    ^Other^
^====^=====^====^=
_t_ date
_l_ line"
  ("t" my-insert-current-date)
  ("l" my-avy-copy-line)
  ("o" crux-smart-open-line)
  ("O" crux-smart-open-line-above))

;; smartparens
(defhydra hydra-sp (:color pink :hint nil :idle 1.5)
  "
^Move^    ^Out^      ^Wrap^      ^Kill^
^====^====^===^======^====^======^====^===
_f_ ford  _,_ left   _s_ splice  _k_ kill
_b_ back  _._ right  _r_ rewarp  _d_ ford
^^        _>_ end    _l_ slurp   _D_ back
^^        _<_ left   _a_ barf    _i_ inner
^^        ^^         ^^          _o_ outer"
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("," sp-backward-up-sexp)
  ("." sp-up-sexp)
  ("<" my-sp-backward-up-sexp-loop)
  (">" my-sp-up-sexp-loop)
  ("s" sp-splice-sexp :exit t)
  ("r" sp-rewrap-sexp :exit t)
  ("l" sp-slurp-hybrid-sexp)
  ("a" sp-forward-barf-sexp)
  ("i" change-inner :exit t)
  ("o" change-outer :exit t)
  ("k" sp-kill-hybrid-sexp :exit t)
  ("d" sp-kill-sexp :exit t)
  ("D" sp-backward-kill-sexp :exit t)
  ("q" nil))

;; git
(defhydra hydra-git (:hint nil)
  "
^Over^     ^Jump^    ^Operation^
^====^=====^====^====^=========^
_g_ magit  _i_ menu  _s_ stage
^^         _p_ prev  _c_ commit
^^         _n_ next  _a_ all
^^         _v_ show  _t_ time"
  ("g" magit-status-fullscreen :exit t)
  ("G" counsel-git-grep :exit t)
  ("i" my-goto-git-gutter+ :exit t)
  ("p" git-gutter+-previous-hunk)
  ("v" git-gutter+-show-hunk)
  ("n" git-gutter+-next-hunk)
  ("s" git-gutter+-stage-hunks)
  ("c" git-gutter+-stage-and-commit)
  ("t" git-timemachine :exit t)
  ("a" git-gutter+-stage-and-commit-whole-buffer))

;; term
(defhydra hydra-term (:color blue :hint nil :idle 1.5)
  "
^Select^  ^Mode^      ^History^  ^Rename^      ^Mode^
^======^==^====^======^=======^==^======^======^====^==
_t_ menu  _m_ matlab  _h_ zsh    _r_ w/o place _l_ line
_n_ next  _e_ eshell  _H_ bash   _R_ w/ place  _c_ char
_p_ prev  _M-t_ new"
  ("l" term-line-mode)
  ("c" term-char-mode)
  ("t" my-ivy-term-goto)
  ("M-t" multi-term)
  ("n" multi-term-next)
  ("p" multi-term-prev)
  ("m" matlab-shell)
  ("e" eshell)
  ("M-t" multi-term)
  ("r" my-term-rename-as-prompt-level-1)
  ("R" my-term-rename-as-prompt-level-0)
  ("h" counsel-yank-zsh-history)
  ("H" counsel-yank-bash-history))

;; info
(defhydra hydra-info (:color blue :hint nil :idle 1.5)
  "
^Select^  ^Mode^      ^History^  ^Misc^
^======^==^====^======^====^=====^=====^
_m_ man"
  ("m" helm-man-woman))

;; tag
(defhydra hydra-tag (:color blue :hint nil :idle 1.5)
  "
^Helm^    ^Dump^    ^Emacs^
^====^====^====^====^=====^=
_._ ford  _f_ ford  _>_ ford
_,_ back  _b_ back
_/_ hist"
  ("." helm-etags+-select)
  ("," helm-etags+-history-go-back)
  ("/" helm-etags+-history)
  ("f" dumb-jump-go)
  ("b" dumb-jump-back)
  (">" find-tag))

;; transpose
(defhydra hydra-transpose (:idle 1.5)
  "transpose"
  ("l" transpose-lines "line")
  ("w" transpose-words "word")
  ("c" transpose-chars "char")
  ("s" transpose-sexps "sexp"))

;; change case
(defhydra hydra-case (:color pink :idle 1.5)
  "case"
  ("c" subword-capitalize "captial")
  ("u" subword-upcase "upcase")
  ("l" subword-downcase "downcase")
  ("q" nil))

;; region
(defhydra hydra-region (:hint nil :idle 1.5)
  "
^Mark^      ^MC^       ^Search^    ^Other^
^====^======^==^=======^======^====^=====^===
_=_ expand  _._ next   _s_ swoop   _n_ narrow
_-_ shrink  _,_ prev   _g_ google  _a_ align
_p_ para    _>_ snext  _b_ bing
_f_ defun   _<_ sprev
_\"_ quote   _r_ all
_(_ pair    _l_ line
^^          _m_ mark"
  ("a" my-align-comment :exit t)
  ("p" er/mark-paragraph :exit t)
  ("f" er/mark-defun :exit t)
  ("\"" er/mark-inside-quotes :exit t)
  ("(" er/mark-inside-pairs :exit t)
  ("=" er/expand-region)
  ("-" er/contract-region)
  ("g" prelude-google :exit t)
  ("b" prelude-bing :exit t)
  ("s" helm-swoop :exit t)
  ("." mc/mark-next-like-this)
  (">" mc/skip-to-next-like-this)
  ("," mc/mark-previous-like-this)
  ("<" mc/skip-to-previous-like-this)
  ("n" my-narrow-or-widen-dwim)
  ("l" mc/edit-lines :exit t)
  ("r" mc/mark-all-in-region :exit t)
  ("m" mc/mark-all-like-this :exit t))

;; gdb
(defhydra hydra-gdb (:color pink :hint nil)
  "
^Move^    ^Break^
^====^====^=====^
_c_ cont  _b_ set
_n_ next  _B_ show
_s_ step"
  ("r" gdb :exit t)
  ("B" gdb-display-breakpoints-buffer)
  ("b" gud-break :exit t)
  ("m" gdb-many-windows)
  ("c" gud-cont)
  ("n" gud-next)
  ("l" gud-refresh)
  ("f" gud-finish)
  ("u" gud-up)
  ("d" gud-down)
  ("s" gud-step)
  ("q" nil))

;; org
(defhydra hydra-org (:color pink :hint nil :idle 1.5)
  "
^Move^    ^Cross^   ^Decorator^   ^Change^    ^Open^     ^Link^
^====^====^=====^===^====^========^======^====^====^=====^====^====
_n_ next  _j_ next  _t_ todo      _←_ left    _o_ open   _y_ stored
_p_ prev  _k_ prev  _w_ refile    _↓_ down    _O_ dired  _Y_ Chrome
^^        _u_ up    _#_ tag       _↑_ up
^^        ^^        _,_ priority  _→_ right"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("u" outline-up-heading)
  ("n" org-forward-heading-same-level)
  ("p" org-backward-heading-same-level)
  ("T" org-show-todo-tree)
  ("t" org-todo)
  ("y" org-insert-link :exit t)
  ("Y" my-insert-current-chrome-link :exit t)
  ("w" org-refile)
  ("#" org-set-tags)
  ("," org-priority)
  ("<left>" org-shiftmetaleft)
  ("<right>" org-shiftmetaright)
  ("<up>" org-metaup)
  ("<down>" org-metadown)
  ("o" org-open-at-point :exit t)
  ("O" my-org-open-at-point :exit t)
  ("q" nil))

;; lisp
(defhydra hydra-lisp (:color blue :hint nil :idle 1.5)
  "
^Eval^
^====^====
_g_ edebug
_e_ eval"
  ("g" edebug-defun)
  ("e" eval-defun))

;; python
(defhydra hydra-python (:color red :hint nil :idle 1.5)
  "
^Date^
---------
_c_ create"
  ("c" my-python-create-date))

;; shell
(defhydra hydra-sh (:color red :hint nil :idle 1.5)
  "
^Date^
---------
_c_ create"
  ("c" my-sh-create-date))

;; c-mode
(defhydra hydra-c (:color red :hint nil)
  "
^Tag^
^===^
_i_ irony"
  ("i" irony-install-server))

(provide 'my-hydra)
;;; my-hydra.el ends here
