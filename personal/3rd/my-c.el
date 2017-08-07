;;; package --- Summary
;;; Commentary:

;; My (C)Makefile, C/C++/Cuda, GDB, protobuf setting.

;;; Code:

;; (c)make file map
(add-to-list 'auto-mode-alist '("Makefile\\." . makefile-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; cuda file map
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))

;; c-mode-common-hook (c, c++, cuda)
(add-hook 'c-mode-common-hook
          (lambda()
            (subword-mode 1)
            (git-gutter+-mode)
            (linum-mode)
            (setq c-basic-offset 4)
            (highlight-indentation-mode)))

;; cmake-mode-hook
(add-hook 'cmake-mode-hook
          (lambda()
            (linum-mode)
            (git-gutter+-mode)))

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; use protobuf mode
(require 'protobuf-mode)

;; protobuf file map mode
(add-to-list 'auto-mode-alist '("\\.prototxt$" . protobuf-mode))

;; protobuf-mode-hook
(add-hook 'protobuf-mode-hook
          (lambda()
            (subword-mode 1)
            (linum-mode)
            (setq c-basic-offset 4)
            (git-gutter+-mode)))

;; gdb
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

(provide 'my-c)
;;; my-c.el ends here
