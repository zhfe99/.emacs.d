(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(LaTeX-fill-break-at-separators nil)
 '(TeX-view-program-list
   (quote
    (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r %n %o %b"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Skim")
     (output-html "xdg-open"))))
 '(font-latex-math-environments
   (quote
    ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "xxalignat" "flalign" "aligns")))
 '(lua-default-application "th")
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(org-clock-into-drawer "LOGBOOK")
 '(package-selected-packages
   (quote
    (zop-to-char yaml-mode window-numbering which-key wgrep web-mode volatile-highlights vkill use-package undo-tree smooth-scrolling smex smartrep smartparens smart-mode-line reveal-in-osx-finder rainbow-mode pyvenv pyenv-mode protobuf-mode peep-dired ov org-bullets operate-on-number multiple-cursors multi-term move-text matlab-mode markdown-preview-mode magit lua-mode key-chord jump-char json-mode js2-mode jedi iy-go-to-char imenu-anywhere ido-ubiquitous hydra highlight-indentation helm-flycheck grizzl google-c-style gitignore-mode gitconfig-mode git-timemachine git-gutter-fringe git-gutter-fringe+ gh ggtags flx-ido exec-path-from-shell etags-table etags-select easy-kill dumb-jump discover-my-major dired-sort dired-narrow dired-details+ dired+ diff-hl dash-at-point cython-mode cuda-mode csv-mode crux company-jedi company-irony-c-headers company-irony company-auctex company-anaconda color-theme-solarized cmake-mode change-inner cdlatex cask-mode buffer-move browse-kill-ring anzu anaphora)))
 '(powerline-default-separator (quote slant))
 '(projectile-tags-command "ctags -Re -f \"%s\" %s")
 '(safe-local-variable-values
   (quote
    ((python-indent . 2)
     (eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))
     (no-byte-compile t)
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face))))))))
 '(term-bind-key-alist
   (quote
    (("C-c C-k" . term-char-mode)
     ("C-d" . term-send-raw)
     ("C-c C-f" . term-line-mode)
     ("C-c C-c" . term-interrupt-subjob)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-s" . isearch-forward)
     ("C-m" . term-send-return)
     ("C-y" . term-paste)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-d" . term-send-forward-kill-word)
     ("<M-DEL>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-," . hydra-term/body)
     ("M-." . comint-dynamic-complete))))
 '(term-unbind-key-list (quote ("C-o" "C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
