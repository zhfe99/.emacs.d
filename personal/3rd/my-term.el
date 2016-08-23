;;; package --- Summary
;;; Commentary:

;; My ansi-term setting.

;;; Code:

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun term-send-ad ()
  "Send \C-a\C-d in term mode."
  (interactive)
  (term-send-raw-string "\C-a\C-d"))

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
        multi-term-program (concat (getenv "HOME") "/apps/bin/zsh"))))
(add-hook 'term-mode-hook
          (lambda () (setq truncate-lines 0)))

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

;;===========
;; Get prompt
(defun my-term-get-prompt ()
  "Get term prompt."
  (let (prompt pos)
    (term-send-raw-string "\C-l")
    (sleep-for 0.5)
    (save-excursion
      (goto-char (point-min))
      (setq prompt (buffer-substring-no-properties 1 (- (line-end-position) 1)))
      (setq pos (string-match " (" prompt))
      (substring prompt 0 pos))))

;; Rename term
(defun my-term-rename-as-prompt ()
  "Rename term as prompt."
  (interactive)
  (let (prompt)
    (setq prompt (my-term-get-prompt))
    (rename-buffer (format "*%s*" prompt))))

;;======================
;; Open term list in ivy
(defun my-buffer-switch-in-visible-window (buffer)
  "Switch buffer in visible window"
  (let (win)
    (setq win (get-buffer-window buffer))
    (if win
        (select-window win)
      (switch-to-buffer buffer))
    )
  )


(defun my-ivy-term-goto ()
  "Open term list in ivy"
  (interactive)
  (let ((buffer-list (my-term-get-all-term-buffer))
        len)
    (setq len (length buffer-list))
    (cond ((= 0 len) (multi-term-next))
          ((= 1 len) (my-buffer-switch-in-visible-window (cdr (nth 0 buffer-list))))
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
