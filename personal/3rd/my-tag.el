;;; package --- Summary
;;; Commentary:
;;; Code:

(setq tags-table-list '("~/tool/lua_lib/TAGS" "~/tool/lua_th/TAGS" "~/tool/py_lib/TAGS" "~/tool/py_caf/TAGS" "~/tool/mat_lib/TAGS" "~/proj/mat/TAGS"))

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)

(setq tags-add-tables nil)

(defun my-reset-tags-table-list ()
  "Return a list of lines of a file at filePath."
  (interactive)
  (let ((proj-root (projectile-project-root)))
    (setq filePath (concat proj-root "/deps")) ; get root_folder/depds
    (when (file-exists-p filePath)
      (setq projs (with-temp-buffer
                    (insert-file-contents filePath)
                    (split-string (buffer-string) "\n" t)))
      (setq proj-tags
            (mapcar
             (lambda (str) (concat "/Users/feng/" str "/TAGS"))
             projs
             ))
      (setq tags-table-list proj-tags)
      )))

(defun my-clear-etag-hash ()
  "Some doc"
  (interactive)
  (clrhash helm-etags-cache))

;; generate etags
(defun my-regenerate-etags ()
  "Regenerate the project's [e|g]tags."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (tags-exclude (projectile-tags-exclude-patterns))
         (default-directory project-root)
         (tags-file (expand-file-name projectile-tags-file-name))
         (command (format projectile-tags-command tags-file tags-exclude))
         shell-output exit-code)
    (with-temp-buffer
      (setq exit-code
            (call-process-shell-command command nil (current-buffer))
            shell-output (projectile-trim-string
                          (buffer-substring (point-min) (point-max)))))
    (unless (zerop exit-code)
      (error shell-output))
    (visit-tags-table tags-file)))

(require 'etags-table)
(setq tag-table-alist
      (list
       '("/Users/feng/.emacs.d/" "/Users/feng/.emacs.d/TAGS")
       '("/Users/tool/caffe/" "/Users/feng/tool/caffe/TAGS")
       '("/Users/feng/tool/char-rnn/" "/Users/feng/tool/char-rnn/TAGS" "/Users/feng/tool/lua_lib/TAGS" "/Users/feng/tool/lua_th/TAGS")
       '("/Users/feng/proj/fg_elad/" "/Users/feng/proj/fg_elad/TAGS" "/Users/feng/tool/lua_lib/TAGS" "/Users/feng/tool/lua_th/TAGS")
       '("/Users/feng/proj/fg/" "/Users/feng/proj/fg/TAGS" "/Users/feng/tool/py_lib/TAGS" "/Users/feng/tool/py_caf/TAGS")
       '(".*\\.py$" "/Users/feng/tool/py_lib/TAGS" "/Users/feng/tool/py_caf/TAGS" "/Users/feng/tool/apollocaffe/TAGS")
       '(".*\\.m$" "/Users/feng/tool/mat_lib/TAGS" "/Users/feng/proj/mat/TAGS")
       '(".*\\.lua$" "/Users/tool/lua_lib/TAGS" "/Users/tool/lua_th/TAGS")))
(setq etags-table-alist tag-table-alist)
(setq etags-table-search-up-depth 10)

(require 'etags-select)
(require 'helm-etags+)
(add-hook 'helm-etags+-select-hook 'etags-table-recompute)

;; (require 'ctags-update)
;; (setq ctags-update-delay-seconds 10)
;; (add-hook 'python-mode-hook  'turn-on-ctags-auto-update-mode)

(provide 'my-tag)
;;; my-tag.el ends here
