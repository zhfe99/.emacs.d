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
(set-default-font "Monaco 12")
(fringe-mode '(4 . 0))

;; enable prelude sub-modules
(require 'prelude-key-chord)
(require 'prelude-ido)
(require 'prelude-latex)
(require 'prelude-helm)
(require 'prelude-python)
(require 'prelude-web)
(require 'prelude-js)

;; tramp
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "ssh")
(setq tramp-chunksize 500)

;; save desktop only on mac
(cond
 ((string-equal system-type "darwin")
  (desktop-save-mode 1))
 ((string-equal system-type "gnu/linux")
  (desktop-save-mode 0)))

;; add marmalade repo into package src (prelude only includes mepla)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; reveal-in-finder
(prelude-require-package 'reveal-in-finder)

;; multiple-cursor
(prelude-require-package 'multiple-cursors)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)

;; projectile
(prelude-require-package 'helm-projectile)
(helm-projectile-on)

;; ace pinyin
(prelude-require-package 'ace-pinyin)

;; ace window
(prelude-require-package 'ace-window)

;; helm-swoop
(prelude-require-package 'helm-swoop)

;; projectile: remote project will expire in 1 hour
(setq projectile-file-exists-remote-cache-expire (* 60 60))

;; environment variables
(setenv "PATH"
        (concat "/usr/local/bin:/usr/texbin"
                ":" (getenv "PATH")
                ":" (getenv "HOME") "/anaconda/bin"))
(setenv "GLOG_minloglevel" "1")
(setenv "PYTHONPATH"
        (concat (getenv "PYTHONPATH")
                ":" (getenv "HOME") "/anaconda/lib/python2.7"
                ":" (getenv "HOME") "/anaconda/lib/python2.7/site-packages"
                ":" (getenv "HOME") "/work/py"
                ":" (getenv "HOME") "/work/tool/caffe/tools/extra"
                ":" "/usr/local/ia/lib/python2.7/site-packages"))
(setenv "DYLD_FALLBACK_LIBRARY_PATH"
        (concat "/usr/local/cuda/lib:/usr/local/lib:/usr/lib"
                ":" (getenv "HOME") "/anaconda/lib"))
(setenv "PYTHONDONTWRITEBYTECODE" "1")
(setenv "s3" "/ssh:feng@skyserver3k:")
(setenv "a0" "/sshx:feng@acadia0a:")
(setenv "a3" "/sshx:feng@acadia3:")
(setenv "e1" "/ssh:ubuntu@54.69.53.225:")
(setenv "e1r" "/sshx:ubuntu@54.69.53.225|sudo:ubuntu@54.69.53.225:")
(setenv "e2" "/ssh:feng@www.cvtell.com:")
(setenv "e2r" "/ssh:root@www.cvtell.com:")
(setenv "pc" "/ssh:yahan@feng-pc:")
(setenv "pal" "/sshx:feng@10.211.55.4:")
(setenv "work" (concat (getenv "HOME") "/work/"))
(setenv "caf" (concat (getenv "HOME") "/work/py/test/caf/"))
(setenv "caffe" (concat (getenv "HOME") "/work/tool/caffe/"))
(add-to-list 'exec-path "/Users/feng/anaconda/bin")

;; ispell
(setq ispell-program-name
      (cond
       ((string-equal system-type "darwin")
        "/usr/local/bin/aspell")
       ((string-equal system-type "gnu/linux")
        "/usr/bin/aspell")))

;; dired
(setq dired-listing-switches "-alh")

;; ace-jump only search filename in dired
(add-hook 'dired-mode-hook
          (lambda ()
            (setq-local ace-jump-search-filter
                        (lambda ()
                          (get-text-property (point) 'dired-filename)))))

;; makefile-mode
(setq auto-mode-alist (cons '("Makefile\\." . makefile-mode) auto-mode-alist))

;; ibuffer
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Image" (mode . image-mode))
               ("Python" (mode . python-mode))
               ("Dired" (mode . dired-mode))
               ("Matlab" (mode . matlab-mode))
               ("Python" (mode . python-mode))
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
               ("Matlab" (mode . matlab-mode))
               ("Org" (or
                       (mode . org-mode)
                       (mode . markdown-mode)))
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
               ("Dired" (mode . dired-mode))
               ("Console" (name . "^\\*.*\\*$"))
               ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

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
    ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "xxalignat" "flalign" "aligns")))
 '(TeX-view-program-list
   (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r %n %o %b"))))
 '(TeX-view-program-selection
   (quote (((output-dvi style-pstricks) "dvips and gv")
           (output-dvi "xdvi")
           (output-pdf "Skim")
           (output-html "xdg-open")))))
(setq-default TeX-master nil)

;; buffer-move
(prelude-require-package 'buffer-move)

;; switch between horizontal split and vertical split
(defun my-toggle-window-split ()
  "Switch between horizontal split and vertical split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; matlab
(prelude-require-package 'matlab-mode)
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))

;; matlab-mode-hook
(add-hook 'matlab-mode-hook
          (lambda()
            (setq matlab-indent-function t)
            (auto-fill-mode -1)))

;; matlab-shell-mode
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; matlab install position
(cond
 ((string-equal system-type "darwin")
  (setq matlab-shell-command "/Applications/MATLAB.app/bin/matlab"))
  ((string-equal system-type "gnu/linux")
   (setq matlab-shell-command "/usr/bin/matlab")))

;; matlab startup configuration
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))

;; update modifying date field in the comment area (for matlab)
(defun my-matlab-modify-date ()
  "Update modifying date field in the comment area (for matlab)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "%   modify" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "modify xxx not found")))))

;; update creating date in the comment area (for matlab)
(defun my-matlab-create-date ()
  "Update creating date in the comment area (for matlab)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "%   create" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "create xxx not found")))))

;; update the "modify date" in the comment before saving
(add-hook 'before-save-hook
          (lambda()
            (if (eq major-mode 'matlab-mode)
                (progn
                  (message "%s is matlab-mode" (buffer-file-name))
                  (my-matlab-modify-date)))))

;; elpy for python
(prelude-require-package 'elpy)
(elpy-enable)

;; prefer to show elpy buffer in the same window
(defun my-elpy-shell-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (pop-to-buffer-same-window (process-buffer (elpy-shell-get-or-create-process))))

;; jedi for python
(prelude-require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(jedi:install-server)
(setq elpy-rpc-backend "jedi")

;; using ipython as the default python console
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--pylab")

;; update modifying date field in the comment area (for python)
(defun my-python-modify-date ()
  "Update modifying date field in the comment area (for python)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "  modify" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "modify xxx not found")))))

;; update creating date in the comment area (for python)
(defun my-python-create-date ()
  "Update creating date in the comment area (for python)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "  create" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "create xxx not found")))))

(defun my-python-save-hook ()
  "My hook for saving python file (*.py)."
  (if (eq major-mode 'python-mode)
      (progn
        (message "%s is python-mode" (buffer-file-name))
        (my-python-modify-date))))
(add-hook 'before-save-hook 'my-python-save-hook)

;; update modifying date field in the comment area (for sh)
(defun my-sh-modify-date ()
  "Update modifying date field in the comment area (for sh)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "  modify" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "modify xxx not found")))))

;; update creating date in the comment area (for sh)
(defun my-sh-create-date ()
  "Update creating date in the comment area (for sh)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "#   create" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "create xxx not found")))))

(defun my-sh-save-hook ()
  "My hook for saving python file (*.py)."
  (if (eq major-mode 'sh-mode)
      (progn
        (message "%s is sh-mode" (buffer-file-name))
        (my-sh-modify-date))))
(add-hook 'before-save-hook 'my-sh-save-hook)

;; my utility functions
(defun my-insert-double-space ()
  "Insert space so that a|bc -> a bc |."
  (interactive)
  (insert " ")
  (forward-char 2)
  (insert " "))

(defun my-insert-single-space ()
  "Insert space so that a|bc -> a b |c."
  (interactive)
  (insert " ")
  (forward-char 1)
  (insert " "))

(defun my-split-window-2-3 ()
  "Split window as 2x3."
  (interactive)
  (split-window-right)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (balance-windows)
  (windmove-left)
  (windmove-left))

(defun my-split-window-2-2 ()
  "Split window as 2x2."
  (interactive)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (windmove-left))

;; python shell (remap up key)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)

;; python mode (save C-C C-p for other use)
(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-c C-p") nil))

;; org agenda file
(setq org-agenda-files (list "~/work/dot/org/day.org"
                             "~/work/dot/org/season.org"))

;; org clock
(setq org-clock-persist 't)
(org-clock-persistence-insinuate)

;; org todo key-words
(setq org-todo-keywords '((sequence "TODO" "DOING" "CANCELED" "RUNNING" "|" "DONE" "PAUSED")))

;; key for switching between key-words
(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map "\M-o" 'org-todo-state-map)
     (define-key org-todo-state-map "t"
       #'(lambda nil (interactive) (org-todo "TODO")))
     (define-key org-todo-state-map "c"
       #'(lambda nil (interactive) (org-todo "CANCELED")))
     (define-key org-todo-state-map "i"
       #'(lambda nil (interactive) (org-todo "DOING")))
     (define-key org-todo-state-map "r"
       #'(lambda nil (interactive) (org-todo "RUNNING")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "p"
       #'(lambda nil (interactive) (org-todo "PAUSED")))))

;; reset org key to be consistent to global keys
(defun my-org-mode-keys ()
  "My keybindings for `org-mode'."
  (define-key org-mode-map (kbd "<S-up>") 'windmove-up)
  (define-key org-mode-map (kbd "<S-down>") 'windmove-down)
  (define-key org-mode-map (kbd "<S-left>") 'windmove-left)
  (define-key org-mode-map (kbd "<S-right>") 'windmove-right)
  (define-key org-mode-map (kbd "<H-up>") 'org-shiftup)
  (define-key org-mode-map (kbd "<H-down>") 'org-shiftdown)
  (define-key org-mode-map (kbd "<H-left>") 'org-shiftleft)
  (define-key org-mode-map (kbd "<H-right>") 'org-shiftright))
(add-hook 'org-mode-hook 'my-org-mode-keys)

;; start org clock when the state is switched to "doing"
(defun org-clock-in-if-doing ()
  "Clock in when the task is marked STARTED."
  (when (and (string= org-state "DOING")
             (not (string= org-last-state org-state)))
    (org-clock-in)))
(add-hook 'org-after-todo-state-change-hook
          'org-clock-in-if-doing)

;; initial visibility for org file
;; (setq org-startup-folded nil)

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
            (setq c-basic-offset 2)
            (ggtags-mode t)))

;; c-mode
(add-hook 'c-mode-hook
          (lambda()
            (setq c-basic-offset 2)
            (ggtags-mode t)))

;; dash for doc
(prelude-require-package 'dash-at-point)
(add-to-list 'dash-at-point-mode-alist '(python-mode . "python2,numpy,django,sqlalchemy,numpy,scipy"))
(add-to-list 'dash-at-point-mode-alist '(sql-mode . "psql"))

;; multi-term
(prelude-require-package 'multi-term)
(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("C-c C-f" . term-line-mode))
            (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))))

;; default shell form multi-term
(cond
 ((string-equal system-type "darwin")
  (setq multi-term-buffer-name "term" multi-term-program "/bin/bash"))
 ((string-equal system-type "gnu/linux")
  (setq multi-term-buffer-name "term" multi-term-program (concat (getenv "HOME") "/bin/bash"))))
(add-hook 'term-mode-hook
          (lambda () (setq truncate-lines 0)))

;; revert buffer without confirmation
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; powerline
(require 'powerline)
(powerline-default-theme)

;; change the background to darker color
(custom-set-faces
 '(powerline-active2 ((t (:inherit mode-line :background "#535353"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#474747")))))

;; fix the incorrect render on Emacs 24.4
(add-hook 'desktop-after-read-hook 'powerline-reset)
(defadvice desktop-kill(before clear-power-line-cache () activate)
  (set-frame-parameter nil 'powerline-cache nil))

;; web-mode
(add-hook 'web-mode-hook
          (lambda()
            (setq web-mode-markup-indent-offset 2)))

;; projectile key
(global-set-key (kbd "M-p") 'projectile-command-map)
(define-key projectile-command-map (kbd "g") 'helm-projectile-grep)
(define-key projectile-command-map (kbd "t") 'projectile-regenerate-tags)

;; my key prefix
(define-prefix-command 'my-key-map)
(global-set-key (kbd "M-m") 'my-key-map)

;; my key for coding (all left-hand characters)
(define-key my-key-map (kbd "q") 'last-kbd-macro)
;; w is available
(define-key my-key-map (kbd "e") 'ediff-files)
(define-key my-key-map (kbd "r") 'revert-buffer-no-confirm)
(define-key my-key-map (kbd "t") 'git-timemachine)
(define-key my-key-map (kbd "a") '(lambda () (interactive) (find-file "~/log/org/my/day.org")))
(define-key my-key-map (kbd "A") '(lambda () (interactive) (find-file "~/log/org/my/info.org")))
(define-key my-key-map (kbd "s") 'sr-speedbar-toggle)
(define-key my-key-map (kbd "d") 'dash-at-point)
(define-key my-key-map (kbd "f") 'find-name-dired)
(define-key my-key-map (kbd "g") 'rgrep)
;; z are available
(define-key my-key-map (kbd "x") 'package-list-packages)
(add-hook 'matlab-mode-hook
          (lambda ()
            (local-set-key (kbd "M-m c") 'my-matlab-create-date)
            (local-set-key (kbd "M-;") 'comment-dwim)))
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "M-m c") 'my-python-create-date)))
(add-hook 'sh-mode-hook
          (lambda () (local-set-key (kbd "M-m c") 'my-sh-create-date)))
;; v b are available

;; my key for shell & window management (all right-hand characters)
;; y is available
(define-key my-key-map (kbd "u") 'my-split-window-2-3)
(define-key my-key-map (kbd "U") 'my-split-window-2-2)
;; i is available
(define-key my-key-map (kbd "o") 'reveal-in-finder)
(define-key my-key-map (kbd "p") 'my-elpy-shell-switch-to-shell)
;; [ ] are available
(define-key my-key-map (kbd "|") 'my-toggle-window-split)
;; h j k ' are available
(define-key my-key-map (kbd "l") 'matlab-shell)
(define-key my-key-map (kbd "n") 'multi-term-next)
(define-key my-key-map (kbd "m") 'multi-term)
(define-key my-key-map (kbd ",") 'my-insert-double-space)
(define-key my-key-map (kbd ".") 'my-insert-single-space)

;; global key
(global-set-key (kbd "<f5>") 'kmacro-set-counter)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "<H-M-up>") 'buf-move-up)
(global-set-key (kbd "<H-M-down>") 'buf-move-down)
(global-set-key (kbd "<H-M-left>") 'buf-move-left)
(global-set-key (kbd "<H-M-right>") 'buf-move-right)
(global-set-key (kbd "C-M-k") 'sp-kill-sexp)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x O") 'ace-swap-window)
(global-set-key (kbd "C-x 0") 'ace-delete-window)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-r") 'helm-swoop)

(provide 'my-basic)
;;; my-basic.el ends here
