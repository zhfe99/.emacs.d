;;; package --- Summary
;;; Commentary:

;; My ansi-term setting.

;;; Code:

;; https://github.com/xee5ch/prelude/commit/00dee5841fec8631b8ee66b752c5fb7320202686
;; remove C-a binding
(defun my-term-mode-hook ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
                (newmap (make-sparse-keymap)))
        (set-keymap-parent newmap oldmap)
        (define-key newmap (kbd "C-a") nil)
        (make-local-variable 'minor-mode-overriding-map-alist)
        (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))
(add-hook 'term-mode-hook 'my-term-mode-hook)

;; default shell form multi-term
(cond
 ((string-equal system-type "darwin")
  (setq multi-term-buffer-name "term" multi-term-program "/bin/zsh"))
 ((string-equal system-type "gnu/linux")
  (setq multi-term-buffer-name "term"
        multi-term-program (concat (getenv "APPS") "/bin/zsh"))))

;; yank bash history
(defun counsel-yank-bash-history ()
  "Yank the bash history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string
            (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Bash history:") collection))))
      (kill-new val)
      (term-paste))))

;; yank zsh history
(defun counsel-yank-zsh-history ()
  "Yank the zsh history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (mapcar (lambda (v) (replace-regexp-in-string "^: [.0-9]+:0;" "" v))
                   (split-string
                    (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                      (buffer-string))
                    "\n"
                    t))))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Zsh history:") collection))))
      (kill-new val)
      (term-paste))))

;; Get all term buffers
(defun my-term-get-all-term-buffer ()
  "Get all term buffers."
  (delq nil (mapcar (lambda (buffer)
                      (set-buffer buffer)
                      (if (equal major-mode 'term-mode)
                          (cons (buffer-name buffer) buffer)
                        nil))
                    (buffer-list))))

;; ==========================================================
;; open a new term starting from the folder of current buffer
(defun my-term-open-at-current-buffer ()
  "Open a new term starting from the folder of current buffer"
  (interactive)
  (let ((current-dired default-directory))
    (multi-term)
    (sleep-for 0.5)
    (term-send-raw-string (format "cd %s" current-dired))
    (sleep-for 0.1)
    (term-send-raw-string "\C-j")
    (sleep-for 0.1)
    (term-send-raw-string "\C-l")))

;;===========
;; Get prompt
(defun my-term-get-prompt ()
  "Get term prompt."
  (let (prompt pos)
    (term-send-raw-string "\C-l")
    (sleep-for 0.5)
    (save-excursion
      (goto-char (window-start))
      (setq prompt (buffer-substring-no-properties (window-start) (- (line-end-position) 1)))
      (setq pos (string-match " (" prompt))
      (substring prompt 0 pos))))

(defun my-term-parse-prompt (prompt level)
  "Parse prompt"
  (cond
   ((eq level 0) prompt)
   ((eq level 1) (substring prompt 0
                            (string-match " " prompt)))))

(defun my-term-rename-as-prompt-level (level)
  "Rename term as prompt."
  (let (prompt)
    (setq prompt (my-term-get-prompt))
    (rename-buffer (format "*%s*"
                           (my-term-parse-prompt prompt level)))))

;; Rename term
(defun my-term-rename-as-prompt-level-0 ()
  "Rename term as prompt."
  (interactive)
  (my-term-rename-as-prompt-level 0))

(defun my-term-rename-as-prompt-level-1 ()
  "Rename term as prompt."
  (interactive)
  (my-term-rename-as-prompt-level 1))

;; =====================
;; Open term list in ivy
(defun my-buffer-switch-in-visible-window (buffer)
  "Switch buffer in visible window"
  (let (win)
    (setq win (get-buffer-window buffer))
    (if win
        (select-window win)
      (switch-to-buffer buffer))
    (end-of-buffer)
    (previous-line)
    (term-send-raw-string "\C-e")))

(defun my-ivy-term-goto ()
  "Open term list in ivy"
  (interactive)
  (let ((buffer-list (my-term-get-all-term-buffer))
        len)
    (setq len (length buffer-list))
    (cond ((= 0 len) (my-term-open-at-current-buffer))
          ((= 1 len) (my-buffer-switch-in-visible-window (cdr (nth 0 buffer-list))))
          (t (ivy-read "terms:"
                       buffer-list
                       :action (lambda (buffer)
                                 (my-buffer-switch-in-visible-window (cdr buffer))))))))

(defun my-term-switch-term-to-current-folder ()
  "Switch term and set it to current folder"
  (interactive)
  (let (buffer-list
        current-dired
        len)
    (setq current-dired
          (if (equal major-mode 'dired-mode)
              (expand-file-name default-directory)
            (if (null (buffer-file-name))
                (user-error "ERROR: current buffer is not associated with a file.")
              (file-name-directory (buffer-file-name)))))
    (setq buffer-list (my-term-get-all-term-buffer))
    (setq len (length buffer-list))
    (cond ((= 0 len)
           (my-term-open-at-current-buffer)
           (term-send-raw-string (format "cd %s" current-dired))
           (sleep-for 0.1)
           (term-send-raw-string "\C-j"))
          ((= 1 len)
           (my-buffer-switch-in-visible-window (cdr (nth 0 buffer-list)))
           (term-send-raw-string (format "cd %s" current-dired))
           (sleep-for 0.1)
           (term-send-raw-string "\C-j"))
          (t (ivy-read "terms:"
                       buffer-list
                       :action (lambda (buffer)
                                 (my-buffer-switch-in-visible-window (cdr buffer))))))))

(ivy-set-actions
 'my-ivy-term-goto
 '(("n" (lambda (buffer) (multi-term)) "new")
   ("k" (lambda (buffer) (kill-buffer (cdr buffer))) "kill")))

(provide 'my-term)
;;; my-term.el ends here
