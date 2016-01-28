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

(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("C-c C-f" . term-line-mode))
            (add-to-list 'term-bind-key-alist '("C-d" . term-send-raw))
            (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
            (add-to-list 'term-bind-key-alist '("C-c C-a" . term-send-ad))
            (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-esc))))

;; default shell form multi-term
(cond
 ((string-equal system-type "darwin")
  (setq multi-term-buffer-name "term" multi-term-program "/home/ma/feng/apps/bin/zsh"))
 ((string-equal system-type "gnu/linux")
  (setq multi-term-buffer-name "term"
        multi-term-program (concat (getenv "HOME") "/bin/bash"))))
(add-hook 'term-mode-hook
          (lambda () (setq truncate-lines 0)))

(provide 'my-term)
;;; my-term.el ends here
