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
  ("b" balance-windows)
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-adjust 0))
  ("s" (lambda () (interactive) (ace-window 4)) :exit t)
  ("d" (lambda () (interactive) (ace-window 16)) :exit t)
  ("q" nil))

;; buffer
(defhydra hydra-buffer (:color blue :hint nil :idle 1.5)
  "
^Buffer^    ^File^       ^Org^        ^Toggle^
^======^====^====^=======^===^========^======^
_o_ open    _M-o_ open   _t_ todo     _l_ linum
_s_ save    _f_ project  _a_ agenda   _e_ ediff
_k_ kill    _z_ reveal   _c_ capture
_b_ bury    _d_ dired
_r_ revert  _p_ machine
_u_ dupe"
  ("o" ivy-switch-buffer)
  ("e" ediff)
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
  ("z" reveal-in-osx-finder)
  ("l" linum-mode))

;; jump
(defhydra hydra-jump (:color blue :hint nil :idle 1.5)
  "
^Jump^    ^Move^    ^Imenu^  ^Bookmark^
^====^====^====^====^=====^==^========^===
_j_ word  _f_ ford  _i_ ivy  _b_ jump
_c_ char  _F_ back  _I_ my   _B_ set
_l_ line  ^^        ^^       _k_ org-clock
_'_ pop"
  ("c" avy-goto-char)
  ("j" avy-goto-word-1)
  ("l" avy-goto-line)
  ("k" org-clock-goto)
  ("'" avy-pop-mark)
  ("i" counsel-imenu)
  ("I" my-ivy-imenu-goto)
  ("b" bookmark-jump)
  ("B" bookmark-set)
  ("f" iy-go-up-to-char)
  ("F" iy-go-to-char-backward))

;; edit
(defhydra hydra-edit (:color blue :hint nil :idle 1.5)
  "
^Line^    ^Other^
^====^=====^====^=
_↑_ up    _y_ yank
_↓_ down  _z_ zop
_d_ dup   _Z_ zap
_k_ kill  _t_ date
_q_ fill  _s_ sudo"
  ("t" my-insert-current-date)
  ("d" crux-duplicate-current-line-or-region)
  ("k" crux-kill-whole-line)
  ("K" crux-kill-line-backwards)
  ("y" counsel-yank-pop)
  ("z" zop-up-to-char)
  ("Z" zap-to-char)
  ("q" fill-paragraph)
  ("s" crux-sudo-edit)
  ("o" crux-smart-open-line)
  ("O" crux-smart-open-line-above)
  ("<up>" move-text-up :exit nil)
  ("<down>" move-text-down :exit nil))

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
  ("M-n" my-sp-up-sexp-loop)
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

;; multiple-cursors
(defhydra hydra-mc (:hint nil :idle 1.5)
  "
^Select^  ^Skip^     ^Line^   ^Multi^
^======^==^====^=====^====^===^=====^=
_._ next  _>_ next  _l_ line  _r_ all
_,_ prev  _<_ prev  _a_ head  _m_ mark
^^        ^^        _e_ tail"
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
^Select^  ^Mode^      ^History^  ^Misc^
^======^==^====^======^=======^==^====^
_t_ menu  _m_ matlab  _h_ zsh    _r_ rename
_n_ next  _e_ eshell  _H_ bash
_p_ prev  _M-t_ new"
  ("t" my-ivy-term-goto)
  ("M-t" multi-term)
  ("n" multi-term-next)
  ("p" multi-term-prev)
  ("m" matlab-shell)
  ("e" eshell)
  ("M-t" multi-term)
  ("r" my-term-rename-as-prompt)
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
(defhydra hydra-tag ()
  "tag"
  ("." helm-etags+-select "helm-etags" :exit t)
  ("/" helm-etags+-history "etags history")
  ("," helm-etags+-history-go-back "etags history go back")
  (">" find-tag "tag"))

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
  ("b" backward-word "backword")
  ("f" forward-word "forward")
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
 (defhydra hydra-gdb (:color pink :hint nil)
  "
^Move^    ^Break^
^====^====^=====^
_c_ cont  _b_ set
_n_ next  _B_ show
_s_ step
"
  ("r" gdb :exit t)
  ("B" gdb-display-breakpoints-buffer)
  ("b" gud-break :exit t)
  ("m" gdb-many-windows "many-windoes")
  ("c" gud-cont "cont")
  ("n" gud-next "next")
  ("l" gud-refresh "refresh")
  ("f" gud-finish "finish")
  ("u" gud-up "up")
  ("d" gud-down "down")
  ("s" gud-step "step")
  ("q" nil "quit"))

;; org
 (defhydra hydra-org (:color pink :hint nil :idle 1.5)
  "
^Move^    ^Cross^   ^Head^        ^Change^    ^Open^
^====^====^=====^===^====^========^======^====^====^===
_n_ next  _j_ next  _t_ todo      _←_ left    _o_ open
_p_ prev  _k_ prev  _w_ refile    _↓_ down    _O_ dired
^^        _h_ up    _s_ tag       _↑_ up
^^        ^^        _,_ priority  _→_ right"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" outline-up-heading)
  ("n" org-forward-heading-same-level)
  ("p" org-backward-heading-same-level)
  ("t" org-todo)
  ("w" org-refile)
  ("s" org-set-tags)
  ("," org-priority)
  ("<left>" org-metaleft)
  ("<right>" org-metaright)
  ("<up>" org-metaup)
  ("<down>" org-metadown)
  ("o" org-open-at-point :exit t)
  ("O" my-org-open-at-point :exit t)
  ("q" nil))

;; lisp
(defhydra hydra-lisp (:color blue :hint nil :idle 1.5)
  "
^Edebug^
---------
_g_ defun"
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
^Irony^
---------
_i_ install"
  ("i" irony-install-server))

(provide 'my-hydra)
;;; my-hydra.el ends here
