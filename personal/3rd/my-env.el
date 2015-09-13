;;; package --- Summary
;;; Commentary:
;;; Code:

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
(set-default-font "Monaco 13")
(fringe-mode '(4 . 0))
(desktop-save-mode 1)
(setq ediff-split-window-function 'split-window-horizontally)

;; environment variables
(setenv "PATH"
        (concat "/usr/local/bin:/usr/texbin"
                ":" (getenv "PATH")))
(setenv "GLOG_minloglevel" "1")
(setenv "GTAGSLIBPATH"
        (concat (getenv "HOME") "/torch"
                ":" (getenv "HOME") "/tool/py_lua"))
(setenv "PYTHONPATH"
        (concat (getenv "PYTHONPATH")
                ":"  "/usr/local/lib/python2.7"
                ":"  "/usr/local/lib/python2.7/site-packages"
                ":" (getenv "HOME") "/tool"
                ":" (getenv "HOME") "/tool/caffe/python"))
(setenv "DYLD_FALLBACK_LIBRARY_PATH"
        (concat "/usr/local/cuda/lib:/usr/local/lib:/usr/lib"))
(setenv "PYTHONDONTWRITEBYTECODE" "1")
(setenv "s3" "/scp:feng@skyserver3k:")
(setenv "s4" "/scp:feng@skyserver4k:")
(setenv "s7" "/scp:feng@skyserver7k:")
(setenv "a0" "/scp:feng@acadia0a:")
(setenv "a1" "/scp:feng@acadia1a:")
(setenv "a2" "/scp:feng@acadia2a:")
(setenv "a3" "/scp:feng@acadia3:")
(setenv "e1" "/ssh:ubuntu@54.69.53.225:")
(setenv "e1r" "/sshx:ubuntu@54.69.53.225|sudo:ubuntu@54.69.53.225:")
(setenv "e2" "/ssh:feng@www.cvtell.com:")
(setenv "e2r" "/ssh:root@www.cvtell.com:")
(setenv "pc" "/ssh:yahan@feng-pc:")
(setenv "pal" "/scp:parallels@10.211.55.5:")

;; ispell
(setq ispell-program-name
      (cond
       ((string-equal system-type "darwin")
        "/usr/local/bin/aspell")
       ((string-equal system-type "gnu/linux")
        "/usr/bin/aspell")))

(provide 'my-env)
;;; my-env.el ends here
