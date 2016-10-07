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
_k_ kill    _z_ reveal
_b_ bury    _d_ dired
_r_ revert  _m_ machine
_u_ dupe
_U_ dupe2"
  ("k" kill-this-buffer)
  ("b" bury-buffer)
  ("r" my-revert-buffer)
  ("z" reveal-in-osx-finder)
  ("d" counsel-goto-recent-directory)
  ("u" my-duplicate-current-buffer-in-ace-window)
  ("U" my-duplicate-ace-buffer-in-current-window)
  ("m" my-switch-to-current-on-server-or-local))

;; special buffer
(defhydra hydra-special (:color blue :hint nil :idle 1.5)
  "
^Org^     ^Todo^       ^Emacs^      ^Fold^         ^Conf^
^===^=====^====^=======^=====^======^====^=========^====^=====
_i_ info  _a_ agenda   _s_ scratch  _d_ Downloads  _D_ dynamic
_r_ read  _t_ todo     _m_ message  _e_ experts
_p_ proj  _c_ capture  _l_ async
_k_ clock"
  ("i" (lambda () (interactive) (find-file "~/code/mine/org/info.org")))
  ("r" (lambda () (interactive) (find-file "~/code/mine/org/read.org")))
  ("p" (lambda () (interactive) (find-file "~/code/mine/org/proj.org")))
  ("d" (lambda () (interactive) (find-file "~/Downloads")))
  ("e" (lambda () (interactive) (find-file "~/papers/experts")))
  ("D" (lambda () (interactive) (find-file "~/.dotfiles/shell/bash_user_dynamic.sh")))
  ("a" org-agenda-list)
  ("t" org-todo-list)
  ("c" org-capture)
  ("k" org-clock-goto)
  ("l" (lambda () (interactive) (switch-to-buffer "*Async Shell Command*")))
  ("s" (lambda () (interactive) (switch-to-buffer "*scratch*")))
  ("m" (lambda () (interactive) (switch-to-buffer "*Messages*"))))

;; jump
(defhydra hydra-jump (:color blue :hint nil :idle 1.5)
  "
^Move^    ^Scroll^  ^Avy^     ^List^
^====^====^======^==^===^=====^====^======
_h_ left  _d_ down  _c_ char  _i_ imenu
_j_ down  _u_ up    _'_ pop   _b_ bookmark
_k_ up    ^^        ^^        _B_ set
_l_ right
_m_ mark"
  ("c" avy-goto-char)
  ("'" avy-pop-mark)
  ("i" counsel-imenu)
  ("b" counsel-bookmark)
  ("B" bookmark-set)
  ("d" my-scroll-down-half :exit nil)
  ("u" my-scroll-up-half :exit nil)
  ("l" forward-char :exit nil)
  ("h" backward-char :exit nil)
  ("m" set-mark-command :exit nil)
  ("j" (lambda () (interactive) (scroll-up 1)) :exit nil)
  ("k" (lambda () (interactive) (scroll-down 1)) :exit nil)
  ("q" nil))

;; toggle
(defhydra hydra-toggle (:color blue :hint nil :idle 1.5)
  "
^Mode^      ^Theme^   ^Other^
^====^======^=====^===^====^===
_l_ linum   _t_ load  _s_ sudo
_v_ visual  ^^        _c_ cleanup
_w_ space   ^^
_i_ indent
_f_ fill"
  ("i" highlight-indentation-mode)
  ("f" auto-fill-mode)
  ("l" linum-mode)
  ("t" counsel-load-theme)
  ("s" crux-sudo-edit)
  ("c" my-cleanup)
  ("w" whitespace-mode)
  ("v" visual-line-mode))

;; edit
(defhydra hydra-edit (:color blue :hint nil :idle 1.5)
  "
^Filename^     ^Date^     ^Para^   ^File^     ^Spell^
^========^=====^====^=====^====^===^====^=====^=====^==
_nn_ name      _t_ date  _q_ fill  _e_ ediff  _i_ check
_na_ absolute  ^^        ^^        _f_ find
_nr_ relative"
  ("t" my-insert-current-date)
  ("nn" my-insert-file-name)
  ("na" my-insert-file-path-absolute)
  ("nr" my-insert-file-path-relative)
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
^Magit^   ^Grep^    ^Hunk^    ^Operation^
^=====^===^====^====^====^====^=========^
_g_ over  _h_ root  _i_ menu  _s_ stage
_l_ log   _H_ curr  _p_ prev  _c_ commit
^^        _f_ file  _n_ next  _a_ all
^^         ^^       _v_ show  _t_ time"
  ("g" magit-status-fullscreen :exit t)
  ("G" magit-status :exit t)
  ("l" magit-log-all :exit t)
  ("h" my-counsel-ag-from-project-root :exit t)
  ("H" counsel-ag :exit t)
  ("i" my-goto-git-gutter+ :exit t)
  ("f" counsel-projectile-find-file :exit t)
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
(defhydra hydra-region (:color blue :hint nil :idle 1.5)
  "
^Mark^   ^  ^MC^      ^Search^    ^Operation^
^====^===^==^==^======^======^====^=========^
_p_ para ^  _a_ all   _g_ google  _-_ align
_f_ defun^  _l_ line  _b_ baidu   _n_ narrow
_\"_ quote  _m_ mark  _B_ bing
_(_ pair"
  ("-" my-align-comment)
  ("n" my-narrow-or-widen-dwim)
  ("p" er/mark-paragraph)
  ("f" er/mark-defun)
  ("\"" er/mark-inside-quotes)
  ("(" er/mark-inside-pairs)
  ("g" prelude-google)
  ("b" prelude-baidu)
  ("B" prelude-bing)
  ("l" mc/edit-lines)
  ("a" mc/mark-all-in-region)
  ("m" mc/mark-all-like-this))

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
^Move^    ^Cross^   ^Decorator^   ^Change^    ^Tree^      ^Link^      ^Archive^
^====^====^=====^===^====^========^======^====^====^======^====^======^=======^==
_N_ next  _n_ next  _t_ todo      _←_ left    _k_ kill    _y_ stored  _a_ sibling
_P_ prev  _p_ prev  _w_ refile    _↓_ down    _s_ sort    _Y_ Chrome  _A_ subtree
^^        _u_ up    _#_ tag       _↑_ up      _>_ expand  _o_ open
^^        ^^        _,_ priority  _→_ right   _/_ narrow  _O_ dired"
  ("s" org-sort)
  ("n" outline-next-visible-heading)
  ("/" my-narrow-or-widen-dwim)
  ("a" org-archive-sibling-heading)
  ("A" org-archive-subtree-default)
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
