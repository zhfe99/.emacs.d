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
            (git-gutter+-mode)
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
            (git-gutter+-mode)))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; remap M-j
(define-key c++-mode-map (kbd "M-j") 'hydra-jump/body)
(define-key c-mode-map (kbd "M-j") 'hydra-jump/body)
(define-key protobuf-mode-map (kbd "M-j") 'hydra-jump/body)

;; gdb
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

(provide 'my-c)
;;; my-c.el ends here
