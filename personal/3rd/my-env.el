;;; package --- Summary
;;; Commentary:

;; Environment variables.

;;; Code:

;; basic setting
(setq visible-bell -1)
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(delete-selection-mode 1)
(setq frame-title-format '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq-default truncate-lines -1)
(setq kill-buffer-query-functions nil)
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)
(setq whitespace-line-column 80000)
(setq doc-view-resolution 800)
(fringe-mode '(8 . 0))
(setq ediff-split-window-function 'split-window-horizontally)

;; copy to Linux's
;; http://stackoverflow.com/questions/64360/how-to-copy-text-from-emacs-to-another-application-on-linux
(setq x-select-enable-clipboard t)

;; be quiet when revert-buffer
(setq auto-revert-verbose nil)

;; font
(setq-default line-spacing 0.1)
(condition-case err
    (set-default-font "Hack 14")
  (error (message "%s" (error-message-string err))))

;; environment variables
(setenv "PATH"
        (concat ":" "/Library/TeX/texbin"
                ":" (getenv "PATH")))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
(setq exec-path (append exec-path '("/Library/TeX/Root/bin/universal-darwin")))
(setenv "GLOG_minloglevel" "1")
(setenv "GTAGSLIBPATH"
        (concat (getenv "HOME") "/torch"
                ":" (getenv "HOME") "/tool/py_lua"))
(setenv "LD_LIBRARY_PATH"
        (concat (getenv "HOME") "/.apps/lib"
                ":" (getenv "LD_LIBRARY_PATH")))

;; ispell
(setq ispell-program-name
      (cond
       ((string-equal system-type "darwin")
        "/usr/local/bin/aspell")
       ((string-equal system-type "gnu/linux")
        (concat (getenv "HOME") "/apps/bin/aspell"))))

;; hide buffer when using M-&
;; http://emacs.stackexchange.com/questions/5553/async-shell-process-buffer-always-clobbers-window-arrangement
(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; Changing the Recentering Order
;; http://irreal.org/blog/?p=6436
(setq recenter-positions '(top middle bottom))

;;=================
;; open large files
;; http://stackoverflow.com/questions/18316665/how-to-improve-emacs-performace-when-view-large-file
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 200)

(defun my--is-file-large ()
  "If buffer too large and my cause performance issue."
  (< large-file-warning-threshold (buffer-size)))

(define-derived-mode my-large-file-mode fundamental-mode "LargeFile"
  "Fixes performance issues in Emacs for large files."
  ;; (setq buffer-read-only t)
  (setq bidi-display-reordering nil)
  (jit-lock-mode nil)
  (buffer-disable-undo)
  (set (make-variable-buffer-local 'global-hl-line-mode) nil)
  (set (make-variable-buffer-local 'line-number-mode) nil)
  (set (make-variable-buffer-local 'column-number-mode) nil) )

(add-to-list 'magic-mode-alist (cons #'my--is-file-large #'my-large-file-mode))

(provide 'my-env)
;;; my-env.el ends here
