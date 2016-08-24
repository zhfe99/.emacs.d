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
_o_ open    _M-o_ open   _t_ todo
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
  ("z" reveal-in-osx-finder))

;; jump
(defhydra hydra-jump (:color blue :hint nil :idle 1.5)
  "
^Avy^     ^Iy^        ^Scroll^  ^Imenu^  ^Bookmark^  ^Misc^
^===^=====^==^========^======^==^=====^==^========^==^====^===
_j_ word  _M-j_ ford  _v_ down  _i_ list  _b_ jump    _k_ clock
_c_ char  _F_ back    _V_ up    _I_ my    _B_ set     _'_ pop
_l_ line"
  ("c" avy-goto-char)
  ("j" avy-goto-word-1)
  ("l" avy-goto-line)
  ("k" org-clock-goto)
  ("'" avy-pop-mark)
  ("i" counsel-imenu)
  ("I" my-ivy-imenu-goto)
  ("b" bookmark-jump)
  ("B" bookmark-set)
  ("v" my-scroll-down-half :exit nil)
  ("V" my-scroll-up-half :exit nil)
  ("M-j" iy-go-up-to-char)
  ("M-h" iy-go-to-char-backward)
  ("<up>" move-text-up :exit nil)
  ("<down>" move-text-down :exit nil)
  ("q" nil))

;; toggle
(defhydra hydra-toggle (:color blue :hint nil :idle 1.5)
  "
^Line^    ^Other^
^====^=====^====^=
^^       _s_ sudo"
  ("q" fill-paragraph)
  ("l" linum-mode)
  ("t" counsel-load-theme)
  ("s" crux-sudo-edit)
  ("c" my-cleanup)
  ("w" whitespace-cleanup)
  ("v" visual-line-mode))

;; yank
(defhydra hydra-yank (:color blue :hint nil :idle 1.5)
  "
^Line^    ^Other^
^====^=====^====^=
_y_ yank  _t_ date
_M-y_ pop
_l_ line"
  ("t" my-insert-current-date)
  ("M-y" crux-duplicate-current-line-or-region)
  ("y" counsel-yank-pop)
  ("l" my-avy-copy-line)
  ("o" crux-smart-open-line)
  ("O" crux-smart-open-line-above))

;; kill
(defhydra hydra-kill (:color blue :hint nil :idle 1.5)
  "
^Line^     ^Zap^        ^Parens^
^====^=====^===^========^======^=
_k_ whole  _M-k_ up-to  _s_ sp
_K_ back   _Z_ back     _i_ inner
^^         ^^           _o_ outer"
  ("s" sp-kill-hybrid-sexp)
  ("d" sp-kill-sexp)
  ("D" sp-backward-kill-sexp)
  ("k" crux-kill-whole-line)
  ("K" crux-kill-line-backwards)
  ("M-k" zop-up-to-char)
  ("Z" zap-to-char)
  ("i" change-inner)
  ("o" change-outer))

;; smartparens
(defhydra hydra-sp (:color pink :hint nil :idle 1.5)
  "
^Move^    ^Wrap^
^====^====^====^====
_f_ ford  _s_ splice
_b_ back  _r_ rewarp
_p_ in    _l_ slurp
_n_ out   _a_ barf"
  ("f" sp-forward-sexp)
  ("P" sp-down-sexp)
  ("N" sp-backward-up-sexp)
  ("M-n" my-sp-up-sexp-loop)
  ("b" sp-backward-sexp)
  ("p" sp-backward-down-sexp)
  ("n" sp-up-sexp)
  ("<down>" sp-next-sexp)
  ("<up>" sp-previous-sexp)
  ("s" sp-splice-sexp :exit t)
  ("r" sp-rewrap-sexp :exit t)
  ("l" sp-slurp-hybrid-sexp)
  ("a" sp-forward-barf-sexp)
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
(defhydra hydra-tag (:hint nil :idle 1.5)
  "
^Helm^    ^Dump^    ^Emacs^
^====^====^====^====^=====^=
_._ ford  _f_ ford  _>_ ford
_,_ back  _b_ back
_/_ hist"
  ("." helm-etags+-select :exit t)
  ("f" dumb-jump-go :exit t)
  ("b" dumb-jump-back)
  ("/" helm-etags+-history)
  ("," helm-etags+-history-go-back)
  (">" find-tag :exit t))

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
  ("i" irony-install-server)
  ("a" beginning-of-defun :exit nil)
  ("e" end-of-defun :exit nil)
  ("h" mark-defun))

(provide 'my-hydra)
;;; my-hydra.el ends here
