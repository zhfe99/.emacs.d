;;; package --- Summary
;;; Commentary:

;; My tramp setting.

;;; Code:

;; tramp
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "scp")
(setq tramp-chunksize 500)
(setq tramp-ssh-controlmaster-options "-o ControlMaster=yes -o ControlPath='tramp.%%C'")

;; disable vc on tramp
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(defun my-string-starts-with (s begins)
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

;; switch to current file on server or local
(defun my-switch-to-current-file-on-server-or-local(remote-name)
  "Switch current file on server or local."
  (interactive)
  (let (my-buffer-file-name my-buffer-file-name1)
    (if (my-string-starts-with (buffer-file-name) "/scp:")
        (progn
          (setq my-buffer-file-name (substring (buffer-file-name) (length remote-name) nil))
          (setq my-buffer-file-name1 (concat "/home/vis/feng/" my-buffer-file-name))
          (find-file my-buffer-file-name1))
      (progn
        (setq my-buffer-file-name (substring (buffer-file-name) (length "/home/vis/feng/") nil))
        (setq my-buffer-file-name1 (concat remote-name my-buffer-file-name))
        (find-file my-buffer-file-name1)))))

;; get current fold on server or local
(defun my-switch-to-current-dired-on-server-or-local(remote-name)
  "Get current fold on server or local."
  (interactive)
  (if (my-string-starts-with dired-directory "/scp:")
      (progn
        (setq my-buffer-file-name (substring dired-directory (length remote-name) nil))
        (setq my-buffer-file-name1 (concat "/home/vis/feng/" my-buffer-file-name))
        (message dired-directory)
        (message my-buffer-file-name1)
        (find-file my-buffer-file-name1))
    (progn
      (setq my-buffer-file-name (substring dired-directory (length "~/") nil))
      (setq my-buffer-file-name1 (concat remote-name my-buffer-file-name))
      (message dired-directory)
      (message my-buffer-file-name1)
      (find-file my-buffer-file-name1))))

;; get current fold on server or local
(defun my-switch-to-current-on-server-or-local(x)
  "Get current fold on server or local."
  (interactive "sEnter remote name: ")
  (setq my-remote-name (concat "/scp:" (getenv x) ":/home/vis/feng/"))
  (if (string= major-mode "dired-mode")
      (my-switch-to-current-dired-on-server-or-local my-remote-name)
    (my-switch-to-current-file-on-server-or-local my-remote-name)))

(provide 'my-tramp)
;;; my-tramp.el ends here
