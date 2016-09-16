;;; package --- Summary
;;; Commentary:
;;; Code:

;; additional packages are available in the folder "3rd"
(add-to-list 'load-path "~/.emacs.d/personal/3rd")

;; enable prelude sub-modules
(require 'prelude-company)
(require 'prelude-ido)
(require 'prelude-programming)
(require 'prelude-org)
(require 'prelude-python)
(require 'prelude-web)
(require 'prelude-css)
(require 'prelude-js)

;; add marmalade repo into package src (prelude only includes mepla)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; 3rd package
(prelude-require-package 'ace-window)
(prelude-require-package 'auctex)
(prelude-require-package 'google-c-style)
(prelude-require-package 'org-bullets)
(prelude-require-package 'buffer-move)
(prelude-require-package 'change-inner)
(prelude-require-package 'dired+)
(prelude-require-package 'dired-details+)
(prelude-require-package 'dired-sort)
(prelude-require-package 'elpy)
(prelude-require-package 'ggtags)
;; (prelude-require-package 'helm-flycheck)
(prelude-require-package 'helm)
(prelude-require-package 'hydra)
(prelude-require-package 'julia-mode)
(prelude-require-package 'markdown-mode)
(prelude-require-package 'pomodoro)
(prelude-require-package 'jump-char)
(prelude-require-package 'keyfreq)
(prelude-require-package 'lua-mode)
(prelude-require-package 'matlab-mode)
(prelude-require-package 'multi-term)
(prelude-require-package 'multifiles)
(prelude-require-package 'multiple-cursors)
(prelude-require-package 'phi-search)
(prelude-require-package 'sr-speedbar)
(prelude-require-package 'etags-table)
(prelude-require-package 'etags-select)
(prelude-require-package 'window-numbering)
(prelude-require-package 'protobuf-mode)
(prelude-require-package 'smooth-scrolling)
(prelude-require-package 'use-package)
(prelude-require-package 'iy-go-to-char)
(prelude-require-package 'imenu-anywhere)
(prelude-require-package 'counsel)
(prelude-require-package 'ivy)
(prelude-require-package 'git-gutter)
(prelude-require-package 'git-gutter+)
(prelude-require-package 'git-gutter-fringe+)
(prelude-require-package 'dash-at-point)

;; github package
(require 'use-package)
(use-package reveal-in-osx-finder
  :if (string-equal system-type "darwin"))
(use-package find-file-in-project
  :load-path "site-lisp/find-file-in-project/"
  :ensure t)
(use-package fasd
  :load-path "site-lisp/emacs-fasd/")

;; my package
(require 'my-env)
(require 'my-modeline)
(require 'my-projectile)
(require 'my-editor)
(require 'my-sp)
(require 'my-mc)
(require 'my-ivy)
(require 'my-tag)
(require 'my-company)
(require 'my-window)
(require 'my-buffer)
(require 'my-tramp)
(require 'my-matlab)
(require 'my-python)
(require 'my-lua)
(require 'my-sh)
(require 'my-web)
(require 'my-org)
(require 'my-c)
(cond
 ((string-equal system-type "darwin")
  (require 'my-latex)))
(require 'my-term)
(require 'my-git)
(require 'my-dired)
(require 'my-info)
(require 'my-hydra)
(require 'my-keymap)

(provide 'my-basic)
;;; my-basic.el ends here
