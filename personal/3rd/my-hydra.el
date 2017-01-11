;;; package --- Summary
;;; Commentary:

;; My hydra key binding.

;;; Code:
(use-package hydra)

;; window
(defhydra hydra-window (:hint nil :idle 1.5)
  "
^Delete^   ^Boundary^   ^Swap^     ^Text^        ^Transpose^     ^Layout^
^======^===^========^===^====^=====^====^========^=========^=====^======^==
_h_ left   _H_ left     _←_ left   _=_ increase  _fh_ flip horz  _1_ save 1
_j_ down   _J_ down     _↓_ down   _-_ decrease  _fv_ flip vert  _2_ save 2
_k_ up     _K_ up       _↑_ up     _0_ default   _rl_ rot left   _!_ load 1
_l_ right  _L_ right    _→_ right  ^^            _rr_ rot right  _@_ load 2
^^         _b_ balance  _s_ ace
^^         ^^           _u_ dupe"
  ("h" my-push-window-left :exit t)
  ("j" my-push-window-down :exit t)
  ("k" my-push-window-up :exit t)
  ("l" my-push-window-right :exit t)
  ("<left>" buf-move-left)
  ("<down>" buf-move-down)
  ("<up>" buf-move-up)
  ("<right>" buf-move-right)
  ("s" ace-swap-window :exit t)
  ("u" my-duplicate-current-buffer-in-ace-window :exit t)
  ("fh" flop-frame :exit t)
  ("fv" flip-frame :exit t)
  ("rl" rotate-frame-anticlockwise :exit t)
  ("rr" rotate-frame-clockwise :exit t)
  ("H" my-move-splitter-left)
  ("J" my-move-splitter-down)
  ("K" my-move-splitter-up)
  ("L" my-move-splitter-right)
  ("1" my-save-window-conf-1 :exit t)
  ("!" my-goto-window-conf-1 :exit t)
  ("2" my-save-window-conf-2 :exit t)
  ("@" my-goto-window-conf-2 :exit t)
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
_r_ revert"
  ("k" kill-this-buffer)
  ("b" bury-buffer)
  ("r" my-revert-buffer)
  ("z" reveal-in-osx-finder)
  ("d" counsel-goto-recent-directory))

;; jump
(defhydra hydra-jump (:color blue :hint nil :idle 1.5)
  "
^Move^    ^Scroll^  ^Avy^      ^List^
^====^====^======^==^===^======^====^======
_h_ left  _d_ down  _c_ char   _i_ imenu
_j_ down  _u_ up    _w_ word   _b_ bookmark
_k_ up    ^^        _'_ pop    _B_ set
_l_ right
_m_ mark"
  ("s" isearch-forward)
  ("c" avy-goto-char)
  ("w" avy-goto-char-timer)
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
^====^======^=====^===^====^=====
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
^Filename^     ^Insert^  ^Para^    ^File^
^========^=====^======^==^====^====^====^===
_nn_ name      _d_ date  _q_ fill  _e_ ediff
_na_ absolute  ^^        ^^        _f_ find
_nr_ relative  ^^
^^             _o_ org"
  ("d" my-insert-current-date)
  ("o" my-insert-org-clocked-task)
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
^^        _>_ end    _l_ slurp
^^        _<_ head   _a_ barf"
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
_p_ prev  _M-t_ new
_d_ curr"
  ("l" term-line-mode)
  ("c" term-char-mode)
  ("t" my-ivy-term-goto)
  ("d" my-term-switch-term-to-current-folder)
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
(defhydra hydra-transpose (:color blue :hint nil :idle 1.5)
  "
^Transpose^
^=========^
_c_ char
_w_ word
_l_ line
_s_ sexp"
  ("l" transpose-lines)
  ("w" transpose-words)
  ("c" transpose-chars)
  ("s" transpose-sexps))

;; change case
(defhydra hydra-case (:color pink :hint nil :idle 1.5)
  "
^Case^
^==========^
_c_ captial
_u_ upcase
_l_ downcase"
  ("c" subword-capitalize)
  ("u" subword-upcase)
  ("l" subword-downcase)
  ("q" nil))

;; region
(defhydra hydra-region (:color blue :hint nil :idle 1.5)
  "
^Mark^   ^  ^MC^      ^Search^    ^Operation^
^====^===^==^==^======^======^====^=========^
_p_ para ^  _a_ all   _g_ google  _-_ align
_f_ defun^  _l_ line  _b_ baidu   _n_ narrow
_\"_ quote  _m_ mark  _B_ bing
_(_ pair ^  ^^        _y_ open"
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
  ("y" browse-url)
  ("a" mc/mark-all-in-region)
  ("m" mc/mark-all-like-this))

;; special buffer
(defhydra hydra-special (:color blue :hint nil :idle 1.5)
  "
^Emacs^
^=====^====
_s_ scratch
_m_ message
_l_ async"
  ("l" (lambda () (interactive) (switch-to-buffer "*Async Shell Command*")))
  ("s" (lambda () (interactive) (switch-to-buffer "*scratch*")))
  ("m" (lambda () (interactive) (switch-to-buffer "*Messages*"))))

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
  ("a" org-archive-to-archive-sibling)
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

;; dired sort
(defhydra hydra-dired-sort (:color blue :hint nil)
  "
^Sort^
^====^==
_n_ name
_d_ date
_s_ size"
  ("d" dired-sort-ctime)
  ("n" dired-sort-name)
  ("s" dired-sort-size))

;; dired ranger
(defhydra hydra-dired-ranger (:color blue :hint nil)
  "
^Ranger^   ^Misc^
^======^===^====^===
_c_ copy   _r_ rsync
_m_ move   _u_ dupe
_p_ paste"
  ("c" dired-ranger-copy)
  ("m" dired-ranger-move)
  ("p" dired-ranger-paste)
  ("u" my-dired-duplicate-marked-files-in-current-folder)
  ("r" my-dired-rsync))

;; dired info
(defhydra hydra-dired-info (:color blue :hint nil)
  "
^Info^
^====^=====
_c_ count
_l_ lines
_s_ size
_p_ path
_P_ path e.
_o_ org"
  ("c" my-dired-get-count)
  ("s" my-dired-get-size)
  ("l" my-dired-get-lines)
  ("p" my-dired-copy-current-file-path)
  ("P" my-dired-copy-current-file-path-with-home-expanded)
  ("o" my-org-store-link))

;; python
(defhydra hydra-python (:color red :hint nil :idle 1.5)
  "
^Date^
^====^====
_c_ create"
  ("c" my-python-create-date))

;; shell
(defhydra hydra-sh (:color red :hint nil :idle 1.5)
  "
^Date^
^====^====
_c_ create"
  ("c" my-sh-create-date))

;; c-mode
(defhydra hydra-c (:color red :hint nil)
  "
^Tag^
^===^====
_i_ irony"
  ("i" irony-install-server))

(provide 'my-hydra)
;;; my-hydra.el ends here
