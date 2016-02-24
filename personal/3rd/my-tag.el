;;; package --- Summary
;;; Commentary:

;; My (c)tag setting.

;;; Code:

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
(setq tags-add-tables nil)

(require 'etags-table)
;; (setq tag-table-alist
;;       (cond
;;        ((string-equal system-type "darwin")
;;         (list
;;          '("/Users/feng/.emacs.d/" "/Users/feng/.emacs.d/TAGS")
;;          '("/Users/feng/code/fzhou/fg_demo/" "/Users/feng/code/pyia/TAGS")
;;          '(".*\\.py$" "/Users/feng/code/py_lib/TAGS" "/Users/feng/code/py_caf/TAGS" "/Users/feng/code/caffe/TAGS")
;;          '(".*\\.m$" "/Users/feng/code/mat_lib/TAGS" "/Users/feng/code/mat/TAGS")
;;          '(".*\\.lua$" "/Users/feng/code/lua_lib/TAGS" "/Users/feng/code/lua_th/TAGS")))
;;        ((string-equal system-type "gnu/linux")
;;         (list
;;          '("/home/ma/feng/.emacs.d/" "/home/ma/feng/.emacs.d/TAGS")
;;          '("/home/ma/feng/code/fzhou/fg_demo/" "/home/ma/feng/code/pyia/TAGS")
;;          '(".*\\.py$" "/home/ma/feng/code/py_lib/TAGS" "/home/ma/feng/code/py_caf/TAGS" "/home/ma/feng/code/caffe/TAGS")
;;          '(".*\\.m$" "/home/ma/feng/code/mat_lib/TAGS" "/home/ma/feng/code/mat/TAGS")
;;          '(".*\\.lua$" "/home/ma/feng/code/lua_lib/TAGS" "/home/ma/feng/code/lua_th/TAGS")))))
;; (setq etags-table-alist tag-table-alist)
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
