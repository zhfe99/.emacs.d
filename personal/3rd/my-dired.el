;;; package --- Summary
;;; Commentary:

;; My dired setting.

;;; Code:

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

(eval-after-load 'dired
  '(progn
     ;; (require 'dired+)
     (require 'dired-sort)
     (require 'dired-ranger)
     (require 'dired-subtree)
     (require 'dired-filter)))

;; set ls default command argument
(cond
 ((string-equal system-type "darwin")
  (setq insert-directory-program "/opt/homebrew/bin/gls")
  (setq dired-listing-switches "-f -alFh"))
 (t (setq dired-listing-switches "-alh")))

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
              "\\|^\\.boot2docker$"
              "\\|^\\.bypy$"
              "\\|^\\.cache$"
              "\\|^\\.cisco$"
              "\\|^\\.cmake$"
              "\\|^\\.conda$"
              "\\|^\\.config$"
              "\\|^\\.cups$"
              "\\|^\\.dbus$"
              "\\|^\\.dropbox$"
              "\\|^\\.editrc$"
              "\\|^\\.emacs.keyfreq$"
              "\\|^\\.emacs.desktop$"
              "\\|^\\.emacs.desktop.lock$"
              "\\|^\\.erc$"
              "\\|^\\.gem$"
              "\\|^\\.fasd$"
              "\\|^\\.fontconfig$"
              "\\|^\\.gconf$"
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
              "\\|^\\.localized$"
              "\\|^\\.lua.*$"
              "\\|^\\.matlab$"
              "\\|^\\.matplotlib$"
              "\\|^\\.mozilla$"
              "\\|^\\.node_libraries$"
              "\\|^\\.npm$"
              "\\|^\\.nv$"
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
              "\\|^\\.sogouinput$"
              "\\|^\\.subversion$"
              "\\|^\\.texmf-var$"
              "\\|^\\.theano$"
              "\\|^\\.thunderbird$"
              "\\|^\\.tramp_history$"
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
              "\\|GPATH$"
              "\\|GRTAGS$"))

;; show .bin .so .a
(setq dired-omit-extensions
      '(".o" "~" ".lbin" ".ln" ".blg" ".bbl" ".elc" ".lof"
        ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/"
        "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib"
        ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl"
        ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl"
        ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl"
        ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky"
        ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc"
        ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn"
        ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs"))

;; dired hook
(add-hook 'dired-mode-hook
          (lambda ()
            (subword-mode 1)
            (dired-omit-mode t)))

;; Find a file and display it in another window
(defun my-dired-find-file-ace-window ()
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

;; ====================
;; Using rsync in dired
;; http://oremacs.com/2016/02/24/dired-rsync/
(defun my-dired-rsync (dest)
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

;; =========
;; Duplicate
(defun my-dired-duplicate-marked-files-in-current-folder ()
  (interactive)
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files)) destfile)
    (dolist (file files)
      (setq destfile (format "%s_dup" file))
      (copy-file file destfile))
    (revert-buffer)))

;; =========
;; file info
;; Get the size of marked elements
;; http://oremacs.com/2015/01/12/dired-file-size/
(defun my-dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (string-trim
        (progn
          (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
          (match-string 1)))))))

;; Count the files insides marked elements
(defun my-dired-get-count ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process (concat (getenv "HOME") "/.dotfiles/bin/co") nil t nil files)
      (message
       "Count: %s"
       (string-trim
        (progn
          (re-search-backward "\\(^[ 0-9.,]+\\).*$")
          (match-string 1)))))))

;; Count lines of the marked files
(defun my-dired-get-lines ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/wc" nil t nil "-l" files)
      (message
       "#Lines: %s"
       (string-trim
        (progn
          (re-search-backward "\\(^[ 0-9.,]+\\).*")
          (match-string 1)))))))

;; Copy the current file path
(defun my-dired-copy-current-file-path ()
  (interactive)
  (kill-new (expand-file-name (dired-file-name-at-point))))

;; use dired quick sort
(require 'dired-quick-sort)
(add-hook 'dired-mode-hook 'dired-quick-sort)

;; auto refresh dired when file changes
;; (add-hook 'dired-mode-hook 'auto-revert-mode)

;; ======================
;; support imenu in dired
(require 'dired-imenu)

;; ==========
;; sudo
(require 'dired-toggle-sudo)
(define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
(eval-after-load 'tramp
  '(progn
     ;; Allow to use: /sudo:user@host:/path/to/file
     (add-to-list 'tramp-default-proxies-alist
	          '(".*" "\\`.+\\'" "/ssh:%h:"))))

(provide 'my-dired)
;;; my-dired.el ends here
