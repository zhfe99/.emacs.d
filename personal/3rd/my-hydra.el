;;; package --- Summary
;;; Commentary:

;; My hydra key binding.

;;; Code:
(use-package hydra)

;; window
(defhydra hydra-window (:hint nil :idle 1.5)
  "
^Move^     ^Delete^   ^Boundary^   ^Text^        ^Transpose^     ^Eyebrowse^
_←_ left   _h_ left   _H_ left     _=_ increase  _fh_ flip horz  _e_ switch
_↓_ down   _j_ down   _J_ down     _-_ decrease  _fv_ flip vert  _t_ tag
_↑_ up     _k_ up     _K_ up       _0_ default   _rl_ rot left   _,_ prev
_→_ right  _l_ right  _L_ right    ^^            _rr_ rot right  _._ next
_m_ move   ^^         _b_ balance  ^^            ^^              _c_ create
_s_ swap   ^^         ^^           ^^            ^^              _x_ close
_u_ dupe"
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
  ("m" my-move-current-buffer-in-ace-window :exit t)
  ("fh" flop-frame :exit t)
  ("fv" flip-frame :exit t)
  ("rl" rotate-frame-anticlockwise :exit t)
  ("rr" rotate-frame-clockwise :exit t)
  ("H" my-move-splitter-left)
  ("J" my-move-splitter-down)
  ("K" my-move-splitter-up)
  ("L" my-move-splitter-right)
  ("b" balance-windows :exit t)
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-adjust 0))
  ("c" eyebrowse-create-window-config :exit t)
  ("x" eyebrowse-close-window-config :exit t)
  ("e" eyebrowse-switch-to-window-config :exit t)
  ("t" eyebrowse-rename-window-config :exit t)
  ("," eyebrowse-prev-window-config :exit t)
  ("." eyebrowse-next-window-config :exit t)
  ("1" eyebrowse-switch-to-window-config-1 :exit t)
  ("2" eyebrowse-switch-to-window-config-2 :exit t)
  ("3" eyebrowse-switch-to-window-config-3 :exit t)
  ("4" eyebrowse-switch-to-window-config-4 :exit t)
  ("5" eyebrowse-switch-to-window-config-5 :exit t)
  ("6" eyebrowse-switch-to-window-config-6 :exit t)
  ("7" eyebrowse-switch-to-window-config-7 :exit t)
  ("q" nil))

;; open
(defhydra hydra-open (:color blue :hint nil :idle 1.5)
  "
^Buffer^    ^File^      ^Org^
_b_ bury    _z_ reveal  _c_ clock
_r_ revert"
  ("b" bury-buffer)
  ("r" my-revert-buffer)
  ("z" reveal-in-osx-finder)
  ("c" org-clock-goto))

;; jump
(defhydra hydra-jump (:color blue :hint nil :idle 1.5)
  "
^Move^     ^Scroll^   ^Avy^     ^List^
_h_ left   _n_ down   _c_ char  _i_ imenu
_j_ down   _p_ up     _w_ word  _b_ bookmark
_k_ up     _gg_ head  _'_ pop   _B_ set
_l_ right  _G_ end"
  ("s" isearch-forward)
  ("c" avy-goto-char)
  ("w" avy-goto-char-timer)
  ("'" avy-pop-mark)
  ("i" counsel-imenu)
  ("b" counsel-bookmark)
  ("B" bookmark-set)
  ("n" my-scroll-down-half :exit nil)
  ("p" my-scroll-up-half :exit nil)
  ("l" forward-char :exit nil)
  ("h" backward-char :exit nil)
  ("gg" beginning-of-buffer :exit nil)
  ("G" end-of-buffer :exit nil)
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
_w_ space   ^^        _u_ untabify
_i_ indent
_f_ fill"
  ("i" highlight-indentation-mode)
  ("f" auto-fill-mode)
  ("l" linum-mode)
  ("t" counsel-load-theme)
  ("s" crux-sudo-edit)
  ("c" my-cleanup)
  ("w" whitespace-mode)
  ("u" untabify)
  ("v" visual-line-mode))

;; edit
(defhydra hydra-edit (:color blue :hint nil :idle 1.5)
  "
^Filename^     ^Insert^  ^Para^    ^File^
_nn_ name      _d_ date  _q_ fill  _e_ ediff
_na_ absolute  _o_ org   ^^        _f_ find
_nr_ relative"
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
^Magit^   ^Grep^    ^Hunk^    ^Operate^   ^Proj^      ^Projectile^
_g_ over  _h_ root  _i_ menu  _s_ stage   _M-g_ proj  _o_ other file
_l_ log   _H_ curr  _p_ prev  _c_ commit
^^        _f_ file  _n_ next  _a_ all
^^        _d_ dir   _v_ show  _t_ time"
  ("M-g" counsel-projectile :exit t)
  ("g" magit-status-fullscreen :exit t)
  ("G" magit-status :exit t)
  ("l" magit-log-all :exit t)
  ("h" my-counsel-ag-from-project-root :exit t)
  ("H" counsel-ag :exit t)
  ("i" my-goto-git-gutter+ :exit t)
  ("f" counsel-projectile-find-file :exit t)
  ("d" counsel-projectile-find-dir :exit t)
  ("o" projectile-find-other-file :exit t)
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
_t_ new   _m_ matlab  _h_ zsh    _r_ w/o place  _l_ line
_n_ next  _e_ eshell  _H_ bash   _R_ w/ place   _c_ char
_p_ prev
_d_ curr"
  ("l" term-line-mode)
  ("c" term-char-mode)
  ("d" my-term-switch-term-to-current-folder)
  ("n" multi-term-next)
  ("p" multi-term-prev)
  ("m" matlab-shell)
  ("e" eshell)
  ("t" my-term-open-at-current-buffer)
  ("r" my-term-rename-as-prompt-level-1)
  ("R" my-term-rename-as-prompt-level-0)
  ("h" counsel-yank-zsh-history)
  ("H" counsel-yank-bash-history))

;; tag
(defhydra hydra-tag (:color blue :hint nil :idle 1.5)
  "
^Etags^    ^Dump^    ^Emacs^  ^Gtags^
_f_ ford  _._ ford  _>_ ford  _g_ find
_b_ back  _,_ back
_/_ hist"
  ("g" counsel-gtags-dwim)
  ("f" helm-etags+-select)
  ("b" helm-etags+-history-go-back)
  ("/" helm-etags+-history)
  ("." dumb-jump-go)
  ("," dumb-jump-back)
  (">" find-tag))

;; transpose
(defhydra hydra-transpose (:color blue :hint nil :idle 1.5)
  "
^Transpose
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
^Select^   ^Operation^  ^Sexp^
_p_ para   _-_ align    _s_ splice
_f_ defun  _n_ narrow   _r_ rewrap
_q_ quote
_[_ pair
_]_ pair"
  ("-" my-align-comment)
  ("n" my-narrow-or-widen-dwim)
  ("p" er/mark-paragraph)
  ("f" er/mark-defun)
  ("q" er/mark-inside-quotes)
  ("Q" er/mark-outside-quotes)
  ("[" er/mark-inside-pairs)
  ("]" er/mark-outside-pairs)
  ("." er/expand-region :exit nil)
  ("," er/contract-region :exit nil)
  ("s" sp-splice-sexp)
  ("r" sp-rewrap-sexp)
  ("u" er/mark-url))

;; mc
(defhydra hydra-mc (:color blue :hint nil :idle 1.5)
  "
^One^     ^Skip^    ^Many^      ^Line^  ^^  ^Insert^
_,_ prev  _<_ prev  _r_ region  _l_ line^^  _n_ number
_._ next  _>_ next  _b_ buffer  _\\^_ head  _c_ char
^^        ^^        ^^          _$_ end"
  ("r" mc/mark-all-in-region)
  ("b" mc/mark-all-like-this)
  ("l" mc/edit-lines)
  ("^" mc/edit-beginnings-of-lines)
  ("$" mc/edit-ends-of-lines)
  ("," mc/mark-previous-like-this :exit nil)
  ("." mc/mark-next-like-this :exit nil)
  ("<" mc/skip-to-previous-like-this :exit nil)
  (">" mc/skip-to-next-like-this :exit nil)
  ("n" mc/insert-numbers)
  ("c" mc/insert-letters))

;; special buffer
(defhydra hydra-special (:color blue :hint nil :idle 1.5)
  "
^Emacs^
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

;; lisp
(defhydra hydra-lisp (:color blue :hint nil :idle 1.5)
  "
^Eval^
_g_ edebug
_e_ eval"
  ("g" edebug-defun)
  ("e" eval-defun))

;; dired ranger
(defhydra hydra-dired-ranger (:color blue :hint nil)
  "
^Ranger^   ^Misc^
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
_c_ count
_l_ lines
_s_ size
_p_ path
_o_ org"
  ("c" my-dired-get-count)
  ("s" my-dired-get-size)
  ("l" my-dired-get-lines)
  ("p" my-dired-copy-current-file-path)
  ("o" my-org-store-link))

;; python
(defhydra hydra-python (:color red :hint nil :idle 1.5)
  "
^Date^
_c_ create"
  ("c" my-python-create-date))

;; shell
(defhydra hydra-sh (:color red :hint nil :idle 1.5)
  "
^Date^
_c_ create"
  ("c" my-sh-create-date))

;; c-mode
(defhydra hydra-c (:color red :hint nil)
  "
^Tag^
_i_ irony"
  ("i" irony-install-server))

(provide 'my-hydra)
;;; my-hydra.el ends here
