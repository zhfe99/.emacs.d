;;; package --- Summary
;;; Commentary:
;;; Code:

;; additional packages are available in the folder "3rd"
(add-to-list 'load-path "~/.emacs.d/personal/3rd")

;; basic setting
(setq visible-bell -1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(delete-selection-mode 1)
(setq frame-title-format '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq-default truncate-lines -1)
(server-start)
(setq kill-buffer-query-functions nil)
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)
(setq whitespace-line-column 80000)
(setq doc-view-resolution 800)
(set-default-font "Monaco 13")
(fringe-mode '(4 . 0))

;; enable prelude sub-modules
(require 'prelude-key-chord)
(require 'prelude-ido)
(require 'prelude-helm)

;; save desktop only on mac
(cond
 ((string-equal system-type "darwin")
  (desktop-save-mode 1))
 ((string-equal system-type "gnu/linux")
  (desktop-save-mode 1)))

;; add marmalade repo into package src (prelude only includes mepla)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; reveal-in-finder
(prelude-require-package 'reveal-in-finder)
(prelude-require-package 'phi-search)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)

;; projectile
(prelude-require-package 'helm-projectile)
(helm-projectile-on)

(prelude-require-package 'ace-pinyin)
(prelude-require-package 'ace-window)
(prelude-require-package 'helm-swoop)

;; projectile: remote project will expire in 1 hour
(setq projectile-file-exists-remote-cache-expire (* 60 60))

;; ispell
(setq ispell-program-name
      (cond
       ((string-equal system-type "darwin")
        "/usr/local/bin/aspell")
       ((string-equal system-type "gnu/linux")
        "/usr/bin/aspell")))

;; makefile-mode
(setq auto-mode-alist (cons '("Makefile\\." . makefile-mode) auto-mode-alist))

;; protobuf-mode
(setq auto-mode-alist (cons '("\\.prototxt$" . protobuf-mode) auto-mode-alist))

;; buffer-move
(prelude-require-package 'buffer-move)

;; my key-binding in prelude mode
(defun my-prelude-mode-keys ()
  "My keybindings for prelude-mode."
  (define-key prelude-mode-map (kbd "M-o") nil)
  (define-key prelude-mode-map (kbd "C-c s") nil)
  (define-key prelude-mode-map (kbd "<M-S-up>") nil)
  (define-key prelude-mode-map (kbd "<M-S-down>") nil)
  (define-key prelude-mode-map (kbd "<C-S-up>") nil)
  (define-key prelude-mode-map (kbd "<C-S-down>") nil))
(add-hook 'prelude-mode-hook 'my-prelude-mode-keys)

;; julia-mode
(prelude-require-package 'julia-mode)
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

;; ggtags
(prelude-require-package 'ggtags)

;; cuda-mode need be manually installed by package-list-package
;; (prelude-require-package 'cude-mode)
;; (require 'cude-mode)

;; powerline
(require 'powerline)
(powerline-default-theme)

;; change the background to darker color
(custom-set-variables
 '(powerline-default-separator 'slant))
(custom-set-faces
 '(powerline-active2 ((t (:inherit mode-line :background "#535353"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#474747")))))

;; fix the incorrect render on Emacs 24.4
(add-hook 'desktop-after-read-hook 'powerline-reset)
(defadvice desktop-kill(before clear-power-line-cache () activate)
  (set-frame-parameter nil 'powerline-cache nil))

(require 'my-env)
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
