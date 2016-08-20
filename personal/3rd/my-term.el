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

(provide 'my-term)
;;; my-term.el ends here
