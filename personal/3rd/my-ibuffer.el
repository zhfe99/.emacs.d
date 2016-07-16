;;; package --- Summary
;;; Commentary:

;; My ibuffer setting.

;;; Code:

;; use ibuffer
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Remote" (filename . "^/scp"))
               ("Image" (mode . image-mode))
               ("Python" (mode . python-mode))
               ("Lua" (mode . lua-mode))
               ("Dired" (mode . dired-mode))
               ("Matlab" (mode . matlab-mode))
               ("Org" (or
                       (mode . org-mode)
                       (mode . markdown-mode)))
               ("C++" (or
                       (mode . makefile-mode)
                       (mode . c-mode)
                       (mode . c++-mode)
                       (mode . cuda-mode)))
               ("Tex" (or
                       (mode . latex-mode)
                       (mode . plain-tex-mode)
                       (mode . bibtex-mode)))
               ("Web" (or
                       (mode . html-mode)
                       (mode . nxml-mode)
                       (mode . web-mode)
                       (mode . js2-mode)
                       (mode . conf-mode)
                       (mode . css-mode)))
               ("Shell" (or
                         (mode . emacs-lisp-mode)
                         (mode . sh-mode)))
               ("Configuration" (mode . protobuf-mode))
               ("Console" (name . "^\\*.*\\*$"))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

(define-key ibuffer-mode-map (kbd "M-o") 'ivy-switch-buffer)

(provide 'my-ibuffer)
;;; my-ibuffer.el ends here
