;;; package --- Summary
;;; Commentary:

;; My Makefile/C/C++/Cuda setting.

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
            (git-gutter-mode)
            (setq c-basic-offset 2)))

;; use indent-gude only for mac
(cond
 ((string-equal system-type "darwin")
  (require 'indent-guide)
  (add-hook 'python-mode-hook
            '(lambda ()
               (indent-guide-mode)))))

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
            (setq c-basic-offset 2)
            (git-gutter-mode)))

;; remap M-j
(define-key c++-mode-map (kbd "M-j") 'avy-goto-word-1)
(define-key c-mode-map (kbd "M-j") 'avy-goto-word-1)
(define-key protobuf-mode-map (kbd "M-j") 'avy-goto-word-1)

(provide 'my-c)
;;; my-c.el ends here
