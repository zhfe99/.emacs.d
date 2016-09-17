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
_k_ up     _K_ up     _C-k_ up       _↑_ up     ^^         _0_ default
_l_ right  _L_ right  _C-l_ right    _→_ right
^^         _1_ other  _b_   balance  _s_ ace"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("<left>" buf-move-left)
  ("<down>" buf-move-down)
  ("<up>" buf-move-up)
  ("<right>" buf-move-right)
  ("s" ace-swap-window :exit t)
  ("C-h" my-move-splitter-left)
  ("C-j" my-move-splitter-down)
  ("C-k" my-move-splitter-up)
  ("C-l" my-move-splitter-right)
  ("H" my-push-window-left :exit t)
  ("J" my-push-window-down :exit t)
  ("K" my-push-window-up :exit t)
  ("L" my-push-window-right :exit t)
  ("1" delete-other-windows :exit t)
  ("2" split-window-below)
  ("3" split-window-right)
  ("4" my-save-window-conf-1 :exit t)
  ("5" my-goto-window-conf-1 :exit t)
  ("b" balance-windows :exit t)
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-adjust 0))
  ("q" nil))

;; open
(defhydra hydra-open (:color blue :hint nil :idle 1.5)
  "
^Buffer^    ^File^
^======^====^====^=====
_k_ kill    _f_ project
_b_ bury    _z_ reveal
_r_ revert  _d_ dired
_u_ dupe    _m_ machine
_U_ dupe2"
  ("k" kill-this-buffer)
  ("b" bury-buffer)
  ("r" my-revert-buffer)
  ("f" find-file-in-project)
  ("z" reveal-in-osx-finder)
  ("d" counsel-goto-recent-directory)
  ("u" my-duplicate-current-buffer-in-ace-window)
  ("U" my-duplicate-ace-buffer-in-current-window)
  ("m" my-switch-to-current-on-server-or-local))

;; special buffer
(defhydra hydra-special (:color blue :hint nil :idle 1.5)
  "
^Org^     ^Todo^       ^Emacs^      ^Bash^
^===^=====^====^=======^=====^======^====^=====
_i_ info  _a_ agenda   _s_ scratch  _d_ dynamic
_r_ read  _t_ todo     _m_ message
_p_ proj  _c_ capture  _l_ async"
  ("i" (lambda () (interactive) (find-file "~/code/mine/org/info.org")))
  ("r" (lambda () (interactive) (find-file "~/code/mine/org/read.org")))
  ("p" (lambda () (interactive) (find-file "~/code/mine/org/proj.org")))
  ("d" (lambda () (interactive) (find-file "~/Downloads")))
  ("e" (lambda () (interactive) (find-file "~/papers/experts")))
  ("D" (lambda () (interactive) (find-file "~/.dotfiles/shell/bash_user_dynamic.sh")))
  ("a" org-agenda-list)
  ("t" org-todo-list)
  ("c" org-capture)
  ("l" (lambda () (interactive) (switch-to-buffer "*Async Shell Command*")))
  ("s" (lambda () (interactive) (switch-to-buffer "*scratch*")))
  ("m" (lambda () (interactive) (switch-to-buffer "*Messages*"))))

;; jump
(defhydra hydra-jump (:color blue :hint nil :idle 1.5)
  "
^Avy^     ^Scroll^  ^Imenu^   ^Bookmark^  ^Misc^
^===^=====^======^==^=====^===^========^==^====^===
_c_ char  _v_ down  _i_ list  _b_ jump   _k_ clock
_l_ line  _V_ up    _I_ my    _B_ set    _'_ pop"
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
  ("n" (lambda () (interactive) (scroll-up 1)) :exit nil)
  ("p" (lambda () (interactive) (scroll-down 1)) :exit nil)
  ("q" nil))

;; toggle
(defhydra hydra-toggle (:color blue :hint nil :idle 1.5)
  "
^Mode^      ^Theme^   ^Other^
^====^======^=====^===^====^===
_l_ linum   _t_ load  _s_ sudo
_v_ visual  ^^        _c_ cleanup
_w_ space   ^^
_i_ indent-guide
_I_ indent-highlight"
  ("i" indent-guide-mode)
  ("I" highlight-indentation-mode)
  ("l" linum-mode)
  ("t" counsel-load-theme)
  ("s" crux-sudo-edit)
  ("c" my-cleanup)
  ("w" whitespace-mode)
  ("v" visual-line-mode))

;; edit
(defhydra hydra-edit (:color blue :hint nil :idle 1.5)
  "
^Region^    ^Insert^   ^Para^   ^File^     ^Spell^
^======^====^======^===^====^===^====^=====^=====^==
_a_ align   _t_ date  _q_ fill  _e_ ediff  _i_ check
_n_ narrow  ^^        ^^        _f_ find"
  ("t" my-insert-current-date)
  ("a" my-align-comment)
  ("n" my-narrow-or-widen-dwim)
  ("i" ispell-word)
  ("e" ediff)
  ("f" find-name-dired)
  ("q" fill-paragraph))

;; smartparens
(defhydra hydra-sp (:color pink :hint nil :idle 1.5)
  "
^Move^    ^Out^      ^Wrap^      ^Kill^
^====^====^===^======^====^======^====^===
_f_ ford  _,_ left   _s_ splice  _k_ kill
_b_ back  _._ right  _r_ rewarp  _d_ ford
^^        _>_ end    _l_ slurp   _i_ inner
^^        _<_ head   _a_ barf    _o_ outer"
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
  ("q" nil))

;; git
(defhydra hydra-git (:hint nil)
  "
^Over^     ^Jump^    ^Operation^
^====^=====^====^====^=========^
_g_ magit  _i_ menu  _s_ stage
_G_ grep   _p_ prev  _c_ commit
_h_ ag     _n_ next  _a_ all
^^         _v_ show  _t_ time"
  ("g" magit-status-fullscreen :exit t)
  ("G" counsel-git-grep :exit t)
  ("h" counsel-ag :exit t)
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
^Select^  ^Mode^      ^History^  ^Rename^       ^Mode^
^======^==^====^======^=======^==^======^=======^====^==
_t_ menu  _m_ matlab  _h_ zsh    _r_ w/o place  _l_ line
_n_ next  _e_ eshell  _H_ bash   _R_ w/ place   _c_ char
_p_ prev  _M-t_ new"
  ("l" term-line-mode)
  ("c" term-char-mode)
  ("t" my-ivy-term-goto)
  ("n" multi-term-next)
  ("p" multi-term-prev)
  ("m" matlab-shell)
  ("e" eshell)
  ("M-t" my-term-open-at-current-buffer)
  ("r" my-term-rename-as-prompt-level-1)
  ("R" my-term-rename-as-prompt-level-0)
  ("h" counsel-yank-zsh-history)
  ("H" counsel-yank-bash-history))

;; tag
(defhydra hydra-tag (:color blue :hint nil :idle 1.5)
  "
^Etags^    ^Dump^    ^Emacs^  ^Gtags^
^=====^====^====^====^=====^==^=====^
_._ ford  _f_ ford  _>_ ford  _g_ find
_,_ back  _b_ back
_/_ hist"
  ("g" counsel-gtags-dwim)
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
^Mark^      ^MC^       ^Search^
^====^======^==^=======^======^==
_=_ expand  _._ next   _g_ google
_-_ shrink  _,_ prev   _b_ baidu
_p_ para    _>_ snext  _B_ bing
_f_ defun   _<_ sprev
_\"_ quote   _r_ all
_(_ pair    _l_ line
^^          _m_ mark"
  ("p" er/mark-paragraph :exit t)
  ("f" er/mark-defun :exit t)
  ("\"" er/mark-inside-quotes :exit t)
  ("(" er/mark-inside-pairs :exit t)
  ("=" er/expand-region)
  ("-" er/contract-region)
  ("g" prelude-google :exit t)
  ("b" prelude-baidu :exit t)
  ("B" prelude-bing :exit t)
  ("." mc/mark-next-like-this)
  (">" mc/skip-to-next-like-this)
  ("," mc/mark-previous-like-this)
  ("<" mc/skip-to-previous-like-this)
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
^Move^    ^Cross^   ^Decorator^   ^Change^    ^Tree^      ^Link^
^====^====^=====^===^====^========^======^====^====^======^====^====
_N_ next  _n_ next  _t_ todo      _←_ left    _k_ kill    _y_ stored
_P_ prev  _p_ prev  _w_ refile    _↓_ down    _s_ sort    _Y_ Chrome
^^        _u_ up    _#_ tag       _↑_ up      _>_ expand  _o_ open
^^        ^^        _,_ priority  _→_ right   ^^          _O_ dired"
  ("s" org-sort)
  ("n" outline-next-visible-heading)
  ("p" outline-previous-visible-heading)
  ("u" outline-up-heading)
  (">" org-content)
  ("N" org-forward-heading-same-level)
  ("P" org-backward-heading-same-level)
  ("T" org-show-todo-tree)
  ("t" org-todo)
  ("y" org-insert-link :exit t)
  ("Y" my-insert-current-chrome-link :exit t)
  ("k" crux-kill-whole-line)
  ("w" org-refile)
  ("#" org-set-tags)
  ("," org-priority)
  ("<left>" org-shiftmetaleft)
  ("<right>" org-shiftmetaright)
  ("<up>" org-metaup)
  ("<down>" org-metadown)
  ("o" org-open-at-point)
  ("O" my-org-open-at-point)
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

;; dired sort
(defhydra hydra-dired-sort (:color blue :hint nil)
  "
_n_ name
_d_ date
_s_ size"
  ("d" dired-sort-ctime)
  ("n" dired-sort-name)
  ("s" dired-sort-size))

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
