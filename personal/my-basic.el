;;; package --- Summary
;;; Commentary:
;;; Code:

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; 3rd package
(prelude-require-package 'auctex)
(prelude-require-package 'google-c-style)
(prelude-require-package 'org-bullets)
(prelude-require-package 'buffer-move)
(prelude-require-package 'dired+)
(prelude-require-package 'dired-details+)
(prelude-require-package 'dired-sort)
(prelude-require-package 'ggtags)
(prelude-require-package 'helm)
(prelude-require-package 'hydra)
(prelude-require-package 'markdown-mode)
(prelude-require-package 'jump-char)
(prelude-require-package 'lua-mode)
(prelude-require-package 'matlab-mode)
(prelude-require-package 'multi-term)
(prelude-require-package 'multiple-cursors)
(prelude-require-package 'etags-table)
(prelude-require-package 'etags-select)
(prelude-require-package 'window-numbering)
(prelude-require-package 'protobuf-mode)
(prelude-require-package 'smooth-scrolling)
(prelude-require-package 'iy-go-to-char)
(prelude-require-package 'with-editor)
(prelude-require-package 'git-gutter)
(prelude-require-package 'git-gutter+)
(prelude-require-package 'git-gutter-fringe+)
(prelude-require-package 'dash-at-point)

;; my package
;; additional packages are available in the folder "3rd"
(add-to-list 'load-path "~/.emacs.d/personal/3rd")

(require 'my-env)
(require 'my-modeline)
(require 'my-projectile)
(require 'my-editor)
(require 'my-sp)
(require 'my-mc)
(require 'my-avy)
(require 'my-ivy)
; (require 'my-tag)
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
;; (cond
;;  ((string-equal system-type "darwin")
;;   (require 'my-latex)))
(require 'my-term)
(require 'my-git)
(require 'my-dired)
(require 'my-info)
(require 'my-hydra)
(require 'my-keymap)

;; my private package
(when (file-directory-p "~/.emacs.d_private")
  (add-to-list 'load-path "~/.emacs.d_private")
  (require 'my-private))

(provide 'my-basic)
;;; my-basic.el ends here
