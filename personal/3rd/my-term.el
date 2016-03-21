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

;; default shell form multi-term
(cond
 ((string-equal system-type "darwin")
  (setq multi-term-buffer-name "term" multi-term-program "/bin/zsh"))
 ((string-equal system-type "gnu/linux")
  (setq multi-term-buffer-name "term"
        multi-term-program (concat (getenv "HOME") "/apps/bin/zsh"))))
(add-hook 'term-mode-hook
          (lambda () (setq truncate-lines 0)))

(provide 'my-term)
;;; my-term.el ends here
