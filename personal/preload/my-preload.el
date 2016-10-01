;;; package --- Summary
;;; Commentary:
;;; Code:

;; add marmalade repo into package src (prelude only includes mepla)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; github package
(require 'use-package)
(use-package swiper
  :load-path "site-lisp/swiper/")
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

(provide 'my-preload)
;;; my-preload.el ends here
