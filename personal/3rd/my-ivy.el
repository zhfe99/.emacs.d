;;; package --- Summary
;;; Commentary:

;; My ivy setting.

;;; Code:

;; turn-off ido mode
(ido-mode -1)

(require 'cl)
(require 'ivy)
(require 'counsel)

;; enable ivy-mode
(ivy-mode)

;; combine recentf inside ivy-switch-buffer
(setq ivy-use-virtual-buffers t)

;; remove initial ^ input
(setq ivy-initial-inputs-alist nil)

;; ivy window height
(setq ivy-height 10)

;; Do not show "./" and "../" in the `counsel-find-file' completion list
(setq ivy-extra-directories nil)

;; recent directories
(defun counsel-goto-recent-directory ()
  "Recent directories"
  (interactive)
  (let (collection)
    (unless recentf-mode (recentf-mode 1))
    (setq collection
          (append (remove-duplicates
                   (mapcar 'file-name-directory recentf-list)
                   :test (lambda (x y) (or (null y) (equal x y)))
                   :from-end t)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (mapcar (lambda (v) (replace-regexp-in-string "^[.0-9]+ +" "" v))
                              (split-string (shell-command-to-string "fasd -d") "\n" t)))))
    (ivy-read "directories:" collection :action 'dired)))

;; https://github.com/abo-abo/swiper/issues/256
;; (require 'ivy_buffer_extend)

;;====================================================
;; Search or Swipe for the Current Word
;; http://pragmaticemacs.com/emacs/search-or-swipe-for-the-current-word/
;; version of ivy-yank-word to yank from start of word
(defun bjm/ivy-yank-whole-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (let (amend)
    (with-ivy-window
      ;;move to last word boundary
      (re-search-backward "\\b")
      (let ((pt (point))
            (le (line-end-position)))
        (forward-word 1)
        (if (> (point) le)
            (goto-char pt)
          (setq amend (buffer-substring-no-properties pt (point))))))
    (when amend
      (insert (replace-regexp-in-string "  +" " " amend)))))

;; start counsel-ag from project root instead of current folder
(defun my-counsel-ag-from-project-root ()
  "Make counsel-ag aware of project root directory."
  (interactive)
  (counsel-ag nil (projectile-project-root)))

(provide 'my-ivy)
;;; my-dired.el ends here
