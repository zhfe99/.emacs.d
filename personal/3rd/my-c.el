;;; package --- Summary
;;; Commentary:
;;; Code:

;; cuda
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))
(add-hook 'cuda-mode-hook
          (lambda()
            (setq c-basic-offset 2)
            (ggtags-mode t)))

;; speedbar
(prelude-require-package 'sr-speedbar)

;; c++-mode
(add-hook 'c++-mode-hook
          (lambda()
            (subword-mode 1)
            (setq c-basic-offset 2)
            (ggtags-mode t)))

;; c-mode
(add-hook 'c-mode-hook
          (lambda()
            (subword-mode 1)
            (setq c-basic-offset 2)
            (ggtags-mode t)))

;; cuda-mode
(add-hook 'cuda-mode-hook
          (lambda()
            (subword-mode 1)
            (setq c-basic-offset 2)
            (ggtags-mode t)))

;; protobuf-mode
(add-hook 'protobuf-mode-hook
          (lambda()
            (subword-mode 1)
            (setq c-basic-offset 2)))

(provide 'my-c)
;;; my-c.el ends here
