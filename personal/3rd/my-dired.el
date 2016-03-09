;;; package --- Summary
;;; Commentary:

;; My dired setting.

;;; Code:

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(eval-after-load 'dired
  '(progn
     (require 'dired+)
     (require 'dired-sort)
     (global-dired-hide-details-mode)))

(setq dired-listing-switches "-alh")

;; dired omit files
(setq dired-omit-files
      (concat dired-omit-files
              "\\|^\\$RECYCLE.BIN$"
              "\\|^\\.DS_Store$"
              "\\|^\\.Trash$"
              "\\|^\\.Xauthority$"
              "\\|^\\.adobe$"
              "\\|^\\.aspell.en.prepl$"
              "\\|^\\.aspell.en.pws$"
              "\\|^\\.autojump$"
              "\\|^\\.aws$"
              "\\|^\\.backups$"
              "\\|^\\.baidu.cookies$"
              "\\|^\\.bash_history$"
              "\\|^\\.bash_sessions$"
              "\\|^\\.bashrc$"
              "\\|^\\.boot2docker$"
              "\\|^\\.bypy$"
              "\\|^\\.cache$"
              "\\|^\\.cmake$"
              "\\|^\\.conda$"
              "\\|^\\.config$"
              "\\|^\\.cups$"
              "\\|^\\.dbus$"
              "\\|^\\.dropbox$"
              "\\|^\\.editrc$"
              "\\|^\\.emacs.d.*$"
              "\\|^\\.emacs.keyfreq$"
              "\\|^\\.emacs.desktop$"
              "\\|^\\.emacs.desktop.lock$"
              "\\|^\\.erc$"
              "\\|^\\.gem$"
              "\\|^\\.fontconfig$"
              "\\|^\\.gconf$"
              "\\|^\\.gitconfig$"
              "\\|^\\.gitignore_global$"
              "\\|^\\.globalrc$"
              "\\|^\\.gnome2.*$"
              "\\|^\\.gnupg$"
              "\\|^\\.gnuplot_history$"
              "\\|^\\.gstreamer.*$"
              "\\|^\\.hgignore_global$"
              "\\|^\\.ido.last$"
              "\\|^\\.ipython$"
              "\\|^\\.julia$"
              "\\|^\\.juliarc.jl$"
              "\\|^\\.julia_history$"
              "\\|^\\.jupyter$"
              "\\|^\\.lesshst$"
              "\\|^\\.lldb$"
              "\\|^\\.local$"
              "\\|^\\.lua.*$"
              "\\|^\\.matlab$"
              "\\|^\\.matplotlib$"
              "\\|^\\.mozilla$"
              "\\|^\\.node_libraries$"
              "\\|^\\.npm$"
              "\\|^\\.nv$"
              "\\|^\\.oh-my-zsh$"
              "\\|^\\.pip$"
              "\\|^\\.pki$"
              "\\|^\\.pkl$"
              "\\|^\\.pry_history$"
              "\\|^\\.psql_history$"
              "\\|^\\.pulse.*$"
              "\\|^\\.putty$"
              "\\|^\\.pylint.d$"
              "\\|^\\.pylintrc$"
              "\\|^\\.python-eggs$"
              "\\|^\\.python_history$"
              "\\|^\\.pyvim$"
              "\\|^\\.qluahistory$"
              "\\|^\\.rbenv$"
              "\\|^\\.real$"
              "\\|^\\.recentf$"
              "\\|^\\.rnd$"
              "\\|^\\.s3cfg$"
              "\\|^\\.serverauth.*$"
              "\\|^\\.screen$"
              "\\|^\\.sh_history$"
              "\\|^\\.smex-items$"
              "\\|^\\.spacemacs$"
              "\\|^\\.ssh$"
              "\\|^\\.subversion$"
              "\\|^\\.texmf-var$"
              "\\|^\\.theano$"
              "\\|^\\.thunderbird$"
              "\\|^\\.vim_mru_files$"
              "\\|^\\.vim_runtime$"
              "\\|^\\.viminfo$"
              "\\|^\\.vimrc$"
              "\\|^\\.vnc$"
              "\\|^\\.w3m$"
              "\\|^\\.zcompdump.*$"
              "\\|^\\.zipline$"
              "\\|^\\.zprofile$"
              "\\|^\\.zsh-update$"
              "\\|^\\.zsh_history$"
              "\\|^\\flycheck_"
              "\\|flymake\\.py$"
              ))

;; ace-jump only search filename in dired
(add-hook 'dired-mode-hook
          (lambda ()
            (subword-mode 1)
            (dired-omit-mode t)
            (setq-local ace-jump-search-filter
                        (lambda ()
                          (get-text-property (point) 'dired-filename)))))

(defun ace-command-other-window (cmd &optional one-win-cmd)
  "Execute CMD in another window.
If provided, call ONE-WIN-CMD instead when there is only one window."
  (interactive "CM-x (other window) ")
  (catch 'done
    (when (and one-win-cmd
               (not (window-parent)))
      (call-interactively one-win-cmd)
      (throw 'done t))
    (let ((start-window (selected-window)))
      (unwind-protect
          (progn
            (aw-switch-to-window
             (aw-select  " Ace - Command "))
            (call-interactively cmd))
        (aw-switch-to-window start-window)))))

(defun ace-find-file ()
  "Find a file and display it in another window."
  (interactive)
  (if (not (window-parent))
      (ido-find-file-other-window)
    (let ((start-win (selected-window))
          (buf (find-file-noselect (ido-read-file-name "File: ")))
          (win (aw-select " Ace File: ")))
      (unwind-protect
          (progn
            (aw-switch-to-window win)
            (switch-to-buffer buf))
        (aw-switch-to-window start-win)))))

(defun ace-switch-buffer ()
  "Switch to another buffer in another window."
  (interactive)
  (if (not (window-parent))
      (ido-switch-buffer-other-window)
    (let ((start-win (selected-window))
          (buf (ido-read-buffer "Buffer: "))
          (win (aw-select " Ace Buffer: ")))
      (unwind-protect
          (progn
            (aw-switch-to-window win)
            (switch-to-buffer buf))
        (aw-switch-to-window start-win)))))

(defun ace-dired-find-file ()
  "Find a file and display it in another window."
  (interactive)
  (if (not (window-parent))
      (ido-find-file-other-window)
    (let ((start-win (selected-window))
          (buf (dired-get-file-for-visit))
          (win (aw-select " Ace File: ")))
      (progn
        (aw-switch-to-window win)
        (find-file buf)))))

(require 'ace-window)
(defun ace-duplicate-buffer ()
  "Switch to another buffer in another window."
  (interactive)
  (let ((start-win (selected-window))
        (buf (current-buffer))
        (win (aw-select " Ace Buffer: ")))
    (progn
      ;; (message win)
      (aw-switch-to-window win)
      (setq buf (buffer-name))
      (message buf)
      (aw-switch-to-window start-win)
      (switch-to-buffer buf)
      )))

;; remap key for dired-mode to be consistent with the setting in my-keymap.el
(define-key dired-mode-map "E" 'ace-dired-find-file)
(define-key dired-mode-map "o" 'prelude-open-with)
(define-key dired-mode-map "d" 'dired-do-delete)
(define-key dired-mode-map "i" 'dired-do-delete)
(define-key dired-mode-map (kbd "<f1>") 'org-agenda-list)
(define-key dired-mode-map (kbd "M-b") 'subword-backward)
(define-key dired-mode-map (kbd "M-i") 'helm-semantic-or-imenu)
(define-key dired-mode-map (kbd "M-l") 'dired-jump)
;; (define-key dired-mode-map (kbd "M-u") 'git-push-remote-origin)

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;;preview files in dired
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(provide 'my-dired)
;;; my-dired.el ends here
