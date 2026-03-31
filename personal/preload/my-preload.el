;;; package --- Summary
;;; Commentary:
;;; Code:

;; add marmalade repo into package src (prelude only includes mepla)
(require 'package)
;; (package-initialize)

(unless (fboundp 'advise-commands)
  (defmacro advise-commands (&rest _args)
    nil))

(unless (boundp 'flyspell-mode-map)
  (defvar flyspell-mode-map (make-sparse-keymap)))

;; github package
;; (require 'use-package)
(use-package reveal-in-osx-finder
  :if (string-equal system-type "darwin"))
(use-package dired-ranger
  :load-path "site-lisp/dired-hacks/")
(use-package dired-subtree
  :load-path "site-lisp/dired-hacks/")
(use-package dired-filter
  :load-path "site-lisp/dired-hacks/")
(use-package dired-narrow
  :load-path "site-lisp/dired-hacks/")

(provide 'my-preload)
;;; my-preload.el ends here
