;;; package --- Summary
;;; Commentary:

;; My (c)tag setting.

;;; Code:

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
(setq tags-add-tables nil)

(require 'etags-table)
(setq torch-tag-path (concat (getenv "HOME") "/.torch/TAGS"))
(setq tag-table-alist
      (list
       '("/Users/feng/.emacs.d/" "/Users/feng/.emacs.d/TAGS")
       '(".*\\.py$" "/Users/feng/code/py_lib/TAGS" "/Users/feng/code/py_caf/TAGS" "/Users/feng/code/caffe/TAGS")
       '(".*\\.lua$" "/home/parallels/.torch/TAGS")))
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
(add-hook 'matlab-mode-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
(setq ctags-update-delay-seconds (* 1 60))

(provide 'my-tag)
;;; my-tag.el ends here
