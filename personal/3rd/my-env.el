;;; package --- Summary
;;; Commentary:
;;; Code:

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

(provide 'my-env)
;;; my-env.el ends here
