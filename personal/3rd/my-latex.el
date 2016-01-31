;;; package --- Summary
;;; Commentary:

;; My latex setting.

;;; Code:

(require 'prelude-latex)

;; latex
(add-hook 'LaTeX-mode-hook
          (lambda()
            (TeX-PDF-mode t)
            (yas-minor-mode t)
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

(defvar my-LaTeX-no-autofill-environments
  '("equation" "equation*" "align" "aligns")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")

(defun my-LaTeX-auto-fill-function ()
  "This function checks whether point is currently inside one of
the LaTeX environments listed in
`my-LaTeX-no-autofill-environments'. If so, it inhibits automatic
filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment my-LaTeX-no-autofill-environments))))
    (when do-auto-fill
      (do-auto-fill))))

(defun my-LaTeX-setup-auto-fill ()
  "This function turns on auto-fill-mode and sets the function
used to fill a paragraph to `my-LaTeX-auto-fill-function'."
  (auto-fill-mode)
  (setq auto-fill-function 'my-LaTeX-auto-fill-function))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-setup-auto-fill)

(provide 'my-latex)
;;; my-latex.el ends here
