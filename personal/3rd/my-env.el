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
(fringe-mode '(4 . 0))
(desktop-save-mode 1)
(setq ediff-split-window-function 'split-window-horizontally)

;; font
;; (set-default-font "Monaco 13")
(set-default-font "Hack 14")
;; (set-default-font "Menlo 13")
;; (set-default-font "Source Code Pro 14")

;; environment variables
(setenv "PATH"
        (concat "/usr/local/bin:/usr/texbin"
                ":" (getenv "PATH")))
(setenv "GLOG_minloglevel" "1")
(setenv "GTAGSLIBPATH"
        (concat (getenv "HOME") "/torch"
                ":" (getenv "HOME") "/tool/py_lua"))
;; (setenv "PYTHONPATH"
;;         (concat (getenv "PYTHONPATH")
;;                 ":" "/usr/local/lib/python2.7"
;;                 ":" "/usr/local/lib/python2.7/site-packages"
;;                 ":" (getenv "HOME") "/tool"
;;                 ":" (getenv "HOME") "/tool/caffe/python"))
;; (setenv "DYLD_FALLBACK_LIBRARY_PATH"
;;         (concat "/usr/local/cuda/lib:/usr/local/lib:/usr/lib"))
;; (setenv "PYTHONDONTWRITEBYTECODE" "1")
(setenv "ts3" "/scp:feng@skyserver3k:")
(setenv "ts4" "/scp:feng@skyserver4k:")
(setenv "ts7" "/scp:feng@skyserver7k:")
(setenv "ta0" "/scp:feng@acadia0a:")
(setenv "ta1" "/scp:feng@acadia1a:")
(setenv "ta2" "/scp:feng@acadia2a:")
(setenv "ta3" "/scp:feng@acadia3:")
(setenv "ta4" "/scp:feng@acadia4a:")
(setenv "te1" "/ssh:ubuntu@54.69.53.225:")
(setenv "te1r" "/sshx:ubuntu@54.69.53.225|sudo:ubuntu@54.69.53.225:")
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

(defun git-push-remote-origin ()
  "run `git push'"
  (interactive)
  (shell-command "git add .")
  (shell-command "git commit -m \"aa\"")
  (shell-command "git push s3s master")
  (message "DONE! git push at %s" default-directory))

;; (setq shell-file-name "bash")
;; (setq shell-command-switch "-ic")
;; (setq shell-command-switch "")

;; (setq tags-table-list '("~/tool/lua_lib/TAGS"
;;                         "~/tool/lua_th/TAGS"
;;                         "~/tool/py_lib/TAGS"
;;                         "~/tool/py_caf/TAGS"
;;                         "~/tool/mat_lib/TAGS"))

(provide 'my-env)
;;; my-env.el ends here
