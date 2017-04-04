;;; package --- Summary
;;; Commentary:
;;; Code:

;; add marmalade repo into package src (prelude only includes mepla)
(require 'package)
(package-initialize)

;; github package
(require 'use-package)
(use-package reveal-in-osx-finder
  :if (string-equal system-type "darwin"))
(use-package pomodoro
  :load-path "site-lisp/pomodoro/")
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
