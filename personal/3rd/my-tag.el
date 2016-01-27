;;; package --- Summary
;;; Commentary:

;; My (c)tag setting.

;;; Code:

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
(setq tags-add-tables nil)

(require 'etags-table)
(setq tag-table-alist
      (list
       '((concat (getenv "HOME") "/.emacs.d") (concat (getenv "HOME") "/.emacs.d/TAGS"))
       '((concat (getenv "HOME") "/code/fg_demo/") (concat (getenv "HOME") "/code/pyia/TAGS"))
       '(".*\\.py$" (concat (getenv "HOME") "/code/py_lib/TAGS") (concat (getenv "HOME") "/code/py_caf/TAGS") (concat (getenv "HOME") "/code/caffe/TAGS"))
       '(".*\\.m$" (concat (getenv "HOME") "/code/mat_lib/TAGS") (concat (getenv "HOME") "/code/mat/TAGS"))
       '(".*\\.lua$" (concat (getenv "HOME") "/code/lua_lib/TAGS") (concat (getenv "HOME") "/code/lua_th/TAGS"))))
(setq etags-table-alist tag-table-alist)
(setq etags-table-search-up-depth 10)

(require 'etags-select)
(require 'helm-etags+)
(add-hook 'helm-etags+-select-hook 'etags-table-recompute)

;; auto-update tags after saving
(require 'ctags-update)
(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)
(add-hook 'sh-mode-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'python-mode-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'c++-mode-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'lua-mode-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
(setq ctags-update-delay-seconds (* 1 60))

(provide 'my-tag)
;;; my-tag.el ends here
