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
              "\\|^\\._.DS_Store$"
              "\\|^\\.CFUserTextEncoding$"
              "\\|^\\.Trash$"
              "\\|^\\.Xauthority$"
              "\\|^\\.adobe$"
              "\\|^\\.anyconnect$"
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
              "\\|^\\.oracle.*$"
              "\\|^\\.pip$"
              "\\|^\\.pki$"
              "\\|^\\.pkl$"
              "\\|^\\.pry_history$"
              "\\|^\\.psql_history$"
              "\\|^\\.pulse.*$"
              "\\|^\\.putty$"
              "\\|^\\.pylint.d$"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move file here                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dash)
(require 'swiper)

;; start directory
(defvar bjm/move-file-here-start-dir (expand-file-name "~/Downloads"))

(defun bjm/move-file-here ()
  "Move file from somewhere else to here.
The file is taken from a start directory set by `bjm/move-file-here-start-dir' and moved to the current directory if invoked in dired, or else the directory containing current buffer. The user is presented with a list of files in the start directory, from which to select the file to move, sorted by most recent first."
  (interactive)
  (let (file-list target-dir file-list-sorted start-file start-file-full)
    ;; clean directories from list but keep times
    (setq file-list
          (-remove (lambda (x) (nth 1 x))
                   (directory-files-and-attributes bjm/move-file-here-start-dir)))

    ;; get target directory
    ;; http://ergoemacs.org/emacs/emacs_copy_file_path.html
    (setq target-dir
          (if (equal major-mode 'dired-mode)
              (expand-file-name default-directory)
            (if (null (buffer-file-name))
                (user-error "ERROR: current buffer is not associated with a file.")
              (file-name-directory (buffer-file-name)))))

  ;; sort list by most recent
  ;;http://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
  (setq file-list-sorted
        (mapcar #'car
                (sort file-list
                      #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

  ;; use ivy to select start-file
  (setq start-file (ivy-read
                    (concat "Move selected file to " target-dir ":")
                    file-list-sorted
                    :re-builder #'ivy--regex
                    :sort nil
                    :initial-input nil))

  ;; add full path to start file and end-file
  (setq start-file-full
        (expand-file-name start-file bjm/move-file-here-start-dir))
  (setq end-file
        (expand-file-name (file-name-nondirectory start-file) target-dir))
  (rename-file start-file-full end-file)
  (message "moved %s to %s" start-file-full end-file)))

;; Using rsync in dired
;; http://oremacs.com/2016/02/24/dired-rsync/
(defun ora-dired-rsync (dest)
  (interactive
   (list
    (expand-file-name
     (read-file-name
      "Rsync to:"
      (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files
                nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command
         "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (shell-quote-argument dest)))
    ;; run the async shell command
    (async-shell-command tmtxt/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))

;; remap key for dired-mode to be consistent with the setting in my-keymap.el
(define-key dired-mode-map "E" 'ace-dired-find-file)
(define-key dired-mode-map "o" 'prelude-open-with)
(define-key dired-mode-map "d" 'dired-do-delete)
(define-key dired-mode-map "D" 'bjm/move-file-here)
(define-key dired-mode-map "i" 'dired-do-delete)
(define-key dired-mode-map "Y" 'ora-dired-rsync)
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
(cond
 ((string-equal system-type "darwin")
  (use-package peep-dired
    :ensure t
    :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
    :bind (:map dired-mode-map
                ("P" . peep-dired)))))

(provide 'my-dired)
;;; my-dired.el ends here
