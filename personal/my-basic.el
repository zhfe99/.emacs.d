;;; package --- Summary
;;; Commentary:
;;; Code:

;; additional packages are available in the folder "3rd"
(add-to-list 'load-path "~/.emacs.d/personal/3rd")

;; enable prelude sub-modules
(require 'prelude-key-chord)
(require 'prelude-ido)
(require 'prelude-helm)

;; add marmalade repo into package src (prelude only includes mepla)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; 3rd package
(prelude-require-package 'reveal-in-finder)
(prelude-require-package 'phi-search)
(prelude-require-package 'ace-pinyin)
(prelude-require-package 'ace-window)
(prelude-require-package 'helm-swoop)
(prelude-require-package 'ggtags)
(prelude-require-package 'buffer-move)
(prelude-require-package 'helm-projectile)
(prelude-require-package 'julia-mode)
(prelude-require-package 'sr-speedbar)
(prelude-require-package 'ace-isearch)
(prelude-require-package 'multiple-cursors)
(prelude-require-package 'multifiles)

;; other setting
(require 'my-env)
(require 'my-powerline)
(require 'my-editor)
(require 'my-window)
;; (require 'my-evil)
(require 'my-tramp)
(require 'my-c)
(require 'my-matlab)
(require 'my-python)
(require 'my-lua)
(require 'my-sh)
(require 'my-web)
(require 'my-org)
(require 'my-latex)
(require 'my-term)
(require 'my-dired)
(require 'my-keymap)

(provide 'my-basic)
;;; my-basic.el ends here
