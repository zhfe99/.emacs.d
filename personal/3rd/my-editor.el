;;; package --- Summary
;;; Commentary:

;; My main editor setting.

;;; Code:

;; turn-on which-function-mode
;; but turn-off it for cython (.pyx, .pyd) otherwise it will be extremely slow
(which-function-mode 1)
(eval-after-load "which-func"
  '(setq which-func-modes '(java-mode c++-mode c-mode org-mode python-mode emacs-lisp-mode)))

;; isearch with selected region
(defun my-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'my-isearch-with-region)

;; insert current date
(defun my-insert-current-date ()
  "Insert current date."
  (interactive)
  (let ((time-format "%Y-%m-%d"))
    (insert (format-time-string time-format (current-time)))))

;; insert current time
(defun my-insert-current-time ()
  "Insert current time."
  (interactive)
  (let ((time-format "%H:%M:%S"))
    (insert (format-time-string time-format (current-time)))))

;; insert org current clocked task
(defun my-insert-org-clocked-task()
  "Insert org current clocked task."
  (interactive)
  (insert org-clock-current-task))

;; ==============================
;; insert file name/path at point
;; http://pragmaticemacs.com/emacs/insert-file-name/
(defun my-insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point."
  (interactive "*fInsert file name: \nP")
  (insert filename))

(defun my-insert-file-path-absolute (filename &optional args)
  "Insert name of file FILENAME into buffer after point."
  (interactive "*fInsert file name: \nP")
  (insert (expand-file-name filename)))

(defun my-insert-file-path-relative (filename &optional args)
  "Insert name of file FILENAME into buffer after point."
  (interactive "*fInsert file name: \nP")
  (insert (file-relative-name filename)))

;; =====
;; align function head comment
(defun my-align-comment()
  "Align function head comment."
  (interactive)
  (align-regexp
   (region-beginning)
   (region-end)
   (concat "\\(\\s-*\\)" " -")))

;; turn-off flycheck-mode
(global-flycheck-mode -1)
;; (require 'helm-flycheck)
;; (setq flycheck-display-errors-delay 0.9)
;; (setq flycheck-display-errors-function #'flycheck-display-error-messages)
;; (add-to-list 'display-buffer-alist (cons "\\*Flycheck error messages\\*" (cons #'display-buffer-no-window nil)))

;; narrow / widen the current region
;; or narrow / widen the current subtree if in org-mode
(defun my-narrow-or-widen-dwim ()
  "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        ((equal major-mode 'org-mode) (org-narrow-to-subtree))
        (t (error "Please select a region to narrow to"))))

;; Type M-y after C-y to activate counsel-yank-pop
(advise-commands "indent" (yank counsel-yank-pop) after
                 "If current mode is one of `prelude-yank-indent-modes', indent yanked text (with prefix arg don't indent)."
                 (if (and (not (ad-get-arg 0))
                          (not (member major-mode prelude-indent-sensitive-modes))
                          (or (derived-mode-p 'prog-mode)
                              (member major-mode prelude-yank-indent-modes)))
                     (let ((transient-mark-mode nil))
                       (yank-advised-indent-function (region-beginning) (region-end)))))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(global-fasd-mode 1)

;; ===================================
;; merge comment-line and comment-dwim
;; http://ergoemacs.org/misc/emacs_comment-line_vs_comment-dwim.html
(defun my-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.
Version 2016-10-17"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (if (eq (point) (line-end-position))
        (comment-dwim nil)
      (if (fboundp 'comment-line)
          (comment-line 1)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))))))

;; ======================================
;; Move Cursor by Paragraph or Text Block
;; http://ergoemacs.org/emacs/emacs_move_by_paragraph.html
(defun my-forward-block (&optional n)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" n)))

(defun my-backward-block (&optional n)
  "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n))
        (-i 1))
    (while (<= -i n)
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq -i n)))
      (setq -i (1+ -i)))))

;; ======================================
;; Move Cursor to Beginning of Line/Block
;; http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html
(defun my-beginning-of-line-or-block (&optional n)
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by blank lines)
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (equal n 1)
        (if (or (equal (point) (line-beginning-position))
                (equal last-command this-command )
                ;; (equal last-command 'xah-end-of-line-or-block )
                )
            (my-backward-block n)
          (beginning-of-line))
      (my-backward-block n))))

(defun my-end-of-line-or-block (&optional n)
  "Move cursor to end of line, or end of current or next text block.
 (a text block is separated by blank lines)
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (equal n 1)
        (if (or (equal (point) (line-end-position))
                (equal last-command this-command )
                ;; (equal last-command 'my-beginning-of-line-or-block )
                )
            (my-forward-block)
          (end-of-line))
      (progn (my-forward-block n)))))

;; ======================================
;; Copy text from Emacs to OS X clipboard
;; http://emacs.stackexchange.com/questions/10900/copy-text-from-emacs-to-os-x-clipboard
(defun my-pbcopy ()
  (interactive)
  (let ((deactivate-mark t))
    (call-process-region (point) (mark) "pbcopy")))

(defun my-pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun my-pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

;; copy-paste for osx
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(cond
 ((string-equal system-type "darwin")
  (progn
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))))

;; https://stackoverflow.com/questions/24620039/how-to-pbpaste-utf-8-characters-in-gnu-emacs-for-osx
(setenv "LANG" "en_US.UTF-8")

;; ===============
;; open large file
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (and (not (string-suffix-p "org" (buffer-file-name)))
             (> (buffer-size) (* 1024 102)))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (font-lock-mode -1)
    (which-function-mode -1)
    (linum-mode 0)
    (show-smartparens-global-mode -1)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

(defun etc-log-tail-handler ()
  (end-of-buffer)
  (make-variable-buffer-local 'auto-revert-interval)
  (setq auto-revert-interval 1)
  (auto-revert-set-timer)
  (make-variable-buffer-local 'auto-revert-verbose)
  (setq auto-revert-verbose nil)
  (read-only-mode t)
  (font-lock-mode -1)
  (when (fboundp 'show-smartparens-mode)
    (show-smartparens-mode 0)))

(add-hook 'auto-revert-tail-mode-hook 'etc-log-tail-handler)

;; special buffer
(defhydra hydra-special (:color blue :hint nil :idle 1.5)
  "
^Fold^           ^File^
_t_ /true_ssd    _s_ *scratch*
_T_ /training/parking_video
_f_ /face/fzhou/parking/data
_d_ /data/parkinglot
_m_ /mnt/data
"
  ("m" (lambda () (interactive) (find-file "/mnt/data/log")))
  ("t" (lambda () (interactive) (find-file "/true_ssd/data")))
  ("T" (lambda () (interactive) (find-file "/training/parking_video")))
  ("f" (lambda () (interactive) (find-file "/face/fzhou/parking/data")))
  ("d" (lambda () (interactive) (find-file "/data/parkinglot/")))
  ("s" (lambda () (interactive) (switch-to-buffer "*scratch*"))))
(global-set-key (kbd "M-p") 'hydra-special/body)

(provide 'my-editor)
;;; my-editor.el ends here
