;;; package --- Summary
;;; Commentary:
;;; Code:

;; add marmalade repo into package src (prelude only includes mepla)
(require 'package)
(package-initialize)

;; github package
(require 'use-package)
(use-package swiper
  :load-path "site-lisp/swiper/")
(use-package avy
  :load-path "site-lisp/avy/")
(use-package ivy
  :load-path "site-lisp/swiper/")
(use-package counsel
  :load-path "site-lisp/swiper/")
(use-package reveal-in-osx-finder
  :if (string-equal system-type "darwin"))
(use-package fasd
  :load-path "site-lisp/emacs-fasd/")
(use-package projectile
  :load-path "site-lisp/projectile/")
(use-package counsel-projectile
  :load-path "site-lisp/counsel-projectile/")
(use-package elpy
  :load-path "site-lisp/elpy/")
(use-package pomodoro
  :load-path "site-lisp/pomodoro/")
(use-package transpose-frame
  :load-path "site-lisp/transpose-frame/")
(use-package dired-ranger
  :load-path "site-lisp/dired-hacks/")
(use-package dired-subtree
  :load-path "site-lisp/dired-hacks/")
(use-package dired-filter
  :load-path "site-lisp/dired-hacks/")

(provide 'my-preload)
;;; my-preload.el ends here
