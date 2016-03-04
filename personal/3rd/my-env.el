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
(set-default-font "Hack 14")
;; (set-default-font "Monaco 13")
;; (set-default-font "Menlo 13")
;; (set-default-font "Source Code Pro 14")

;; environment variables
(setenv "PATH"
        (concat "/usr/local/bin"
                ":" "/Library/TeX/texbin"
                ":" (getenv "PATH")))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
(setenv "GLOG_minloglevel" "1")
(setenv "GTAGSLIBPATH"
        (concat (getenv "HOME") "/torch"
                ":" (getenv "HOME") "/tool/py_lua"))
(setenv "PYTHONPATH"
        (concat (getenv "HOME") "/code"
                ":" (getenv "PYTHONPATH")))

;; remote server
(setenv "ts3" "/scp:feng@skyserver3k:")
(setenv "ts4" "/scp:feng@skyserver4k:")
(setenv "ts7" "/scp:feng@skyserver7k:")
(setenv "ta0" "/scp:feng@acadia0a:")
(setenv "ta1" "/scp:feng@acadia1a:")
(setenv "ta2" "/scp:feng@acadia2a:")
(setenv "ta3" "/scp:feng@acadia3:")
(setenv "ta4" "/scp:feng@acadia4a:")
(setenv "te3" "/ssh:ubuntu@52.91.111.136:")
(setenv "te2" "/ssh:feng@www.cvtell.com:")
(setenv "te2r" "/ssh:root@www.cvtell.com:")
(setenv "tpc" "/ssh:yahan@feng-pc:")
(setenv "tpal" "/scp:parallels@10.211.55.5:")

;; ispell
(setq ispell-program-name
      (cond
       ((string-equal system-type "darwin")
        "/usr/local/bin/aspell")
       ((string-equal system-type "gnu/linux")
        "/usr/bin/aspell")))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(provide 'my-env)
;;; my-env.el ends here
