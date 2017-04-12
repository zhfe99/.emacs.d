;;; package --- Summary
;;; Commentary:

;; My ivy setting.

;;; Code:

;; turn-off ido mode
(ido-mode -1)

(require 'cl)
(require 'ivy)
(require 'ivy-hydra)
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

;;===============================================
;; Change the binding action of ivy-switch-buffer
(ivy-set-actions
 'ivy-switch-buffer
 '(("a" (lambda (buffer)
          (aw-switch-to-window (aw-select " Ace Buffer: "))
          (switch-to-buffer buffer))
    "ace")
   ("1" (lambda (buffer)
          (delete-other-windows)
          (switch-to-buffer buffer))
    "other")
   ("2" (lambda (buffer)
          (split-window-below-and-move-there-dammit)
          (switch-to-buffer buffer))
    "below")
   ("3" (lambda (buffer)
          (split-window-right-and-move-there-dammit)
          (switch-to-buffer buffer))
    "right")
   ("k"
    (lambda (x)
      (kill-buffer x)
      (ivy--reset-state ivy-last))
    "kill")))

;;===============================================
;; Change the binding action of counsel-find-file
(ivy-set-actions
 'counsel-find-file
 '(("a" (lambda (file)
          (aw-switch-to-window (aw-select " Ace Buffer: "))
          (find-file file))
    "ace")
   ("1" (lambda (file)
          (delete-other-windows)
          (find-file file))
    "other")
   ("2" (lambda (file)
          (split-window-below-and-move-there-dammit)
          (find-file file))
    "below")
   ("3" (lambda (file)
          (split-window-right-and-move-there-dammit)
          (find-file file))
    "right")
   ("z" (lambda (d)
          (reveal-in-osx-finder-as d nil))
    "reveal")))

;;===============================================
;; Change the binding action of counsel-find-file
(defun my-open-file-with (file)
  "Open visited file in default external program."
  (let* ((open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open"))))
    (start-process "crux-open-with-process" nil open file)))

(ivy-set-actions
 'counsel-projectile-find-file
 '(("." (lambda (file)
          (dired (file-name-directory (projectile-expand-root file))))
    "directory")
   ("c" (lambda (file)
          (my-open-file-with (projectile-expand-root file)))
    "crux open")
   ("z" (lambda (file)
          (reveal-in-osx-finder-as (projectile-expand-root file) nil))
    "reveal")))

;;===========================================================
;; Change the binding action of counsel-goto-recent-directory
(ivy-set-actions
 'counsel-goto-recent-directory
 '(("a" (lambda (d)
          (aw-switch-to-window (aw-select " Ace Buffer: "))
          (dired d))
    "ace")
   ("1" (lambda (d)
          (delete-other-windows)
          (dired d))
    "other")
   ("2" (lambda (d)
          (split-window-below-and-move-there-dammit)
          (dired d))
    "below")
   ("3" (lambda (d)
          (split-window-right-and-move-there-dammit)
          (dired d))
    "right")
   ("z" (lambda (d)
          (reveal-in-osx-finder-as d nil))
    "reveal")))

;; ivy-minibuffer
(define-key ivy-minibuffer-map (kbd "M-j") 'bjm/ivy-yank-whole-word)
(define-key ivy-minibuffer-map (kbd "C-o") 'ivy-occur)

(provide 'my-ivy)
;;; my-dired.el ends here
