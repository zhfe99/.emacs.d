;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'prelude-latex)

;; latex
(prelude-require-package 'auctex)
(add-hook 'LaTeX-mode-hook
          (lambda()
            (TeX-PDF-mode t)
            (setq TeX-save-query nil)
            (toggle-truncate-lines)))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)

;; read in PDF
(custom-set-variables
 '(LaTeX-command "latex -synctex=1")
 '(font-latex-math-environments
   (quote
    ("display" "displaymath" "equation" "eqnarray" "gather"
     "multline" "align" "alignat" "xalignat" "xxalignat" "flalign" "aligns")))
 '(TeX-view-program-list
   (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r %n %o %b"))))
 '(TeX-view-program-selection
   (quote (((output-dvi style-pstricks) "dvips and gv")
           (output-dvi "xdvi")
           (output-pdf "Skim")
           (output-html "xdg-open")))))
(setq-default TeX-master nil)

(provide 'my-latex)
;;; my-latex.el ends here
