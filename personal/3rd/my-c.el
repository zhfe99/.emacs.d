;;; package --- Summary
;;; Commentary:

;; My C/C++/Cuda setting.

;;; Code:

(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(require 'indent-guide)

;; cuda
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))

;; c-mode
(add-hook 'c-mode-hook
          (lambda()
            (subword-mode 1)
            (indent-guide-mode)
            (setq c-basic-offset 2)))
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; c++-mode
(add-hook 'c++-mode-hook
          (lambda()
            (subword-mode 1)
            (indent-guide-mode)
            (setq c-basic-offset 2)))
;; (add-hook 'c++-mode-hook 'google-set-c-style)

;; cuda-mode
(add-hook 'cuda-mode-hook
          (lambda()
            (subword-mode 1)
            (setq c-basic-offset 2)
            (indent-guide-mode)
            ;; (ggtags-mode t)
            ))
;; (add-hook 'cuda-mode-hook 'google-set-c-style)

;; remap M-j
(define-key c++-mode-map (kbd "M-j") 'avy-goto-word-1)
(define-key c-mode-map (kbd "M-j") 'avy-goto-word-1)
(require 'protobuf-mode)
(define-key protobuf-mode-map (kbd "M-j") 'avy-goto-word-1)

;; protobuf-mode
(setq auto-mode-alist (cons '("\\.prototxt$" . protobuf-mode) auto-mode-alist))
(add-hook 'protobuf-mode-hook
          (lambda()
            (subword-mode 1)
            (setq c-basic-offset 2)))

;; makefile-mode
(setq auto-mode-alist (cons '("Makefile\\." . makefile-mode) auto-mode-alist))

(provide 'my-c)
;;; my-c.el ends here
