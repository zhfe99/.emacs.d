;;; package --- Summary
;;; Commentary:

;; Environment variables.

;;; Code:

;; basic setting
(setq visible-bell -1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)
(delete-selection-mode 1)
(setq frame-title-format '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq-default truncate-lines -1)
(server-start)
(setq kill-buffer-query-functions nil)
(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)
(setq whitespace-line-column 80000)
(setq doc-view-resolution 800)
(fringe-mode '(4 . 0))
;; (desktop-save-mode 1)
(setq ediff-split-window-function 'split-window-horizontally)

;; font
(setq-default line-spacing 0.1)
(condition-case err
    (set-default-font "Hack 14")
  (error (message "%s" (error-message-string err))))

;; font used before
;; (set-default-font "Monaco 13")
;; (set-default-font "Source Code Pro 14")

;; environment variables
(setenv "PATH"
        (concat "/usr/local/bin"
                ":" "/Library/TeX/texbin"
                ":" (getenv "PATH")))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
(setq exec-path (append exec-path '("/Library/TeX/Root/bin/universal-darwin")))
(setenv "GLOG_minloglevel" "1")
(setenv "GTAGSLIBPATH"
        (concat (getenv "HOME") "/torch"
                ":" (getenv "HOME") "/tool/py_lua"))
(setenv "PYTHONPATH"
        (concat (getenv "HOME") "/code"
                ":" (getenv "PYTHONPATH")))

(setenv "PYTHONPATH"
        (concat (getenv "HOME") "/code/baidu/tl/tools"
                ":" (getenv "PYTHONPATH")))

;; ispell
(setq ispell-program-name
      (cond
       ((string-equal system-type "darwin")
        "/usr/local/bin/aspell")
       ((string-equal system-type "gnu/linux")
        (concat (getenv "HOME") "/apps/bin/aspell"))))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; hide buffer when using M-&
;; http://emacs.stackexchange.com/questions/5553/async-shell-process-buffer-always-clobbers-window-arrangement
(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(provide 'my-env)
;;; my-env.el ends here
