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

(defun ivy-imenu-get-candidates-from (alist  &optional prefix)
  (cl-loop for elm in alist
           nconc (if (imenu--subalist-p elm)
                       (ivy-imenu-get-candidates-from
                        (cl-loop for (e . v) in (cdr elm) collect
                                 (cons e (if (integerp v) (copy-marker v) v)))
                        (concat prefix (if prefix ".") (car elm)))
                   (and (cdr elm) ; bug in imenu, should not be needed.
                        (setcdr elm (copy-marker (cdr elm))) ; Same as [1].
                        (list (cons (concat prefix (if prefix ".") (car elm))
                                    (copy-marker (cdr elm))))))))

(defun my-ivy-imenu-goto ()
  "Go to buffer position"
  (interactive)
  (let ((imenu-auto-rescan t) items)
    (unless (featurep 'imenu)
      (require 'imenu nil t))
    (setq items (imenu--make-index-alist t))
    (ivy-read "imenu items:"
              (ivy-imenu-get-candidates-from (delete (assoc "*Rescan*" items) items))
              :action (lambda (k) (goto-char k)))))

(defun ivy-bookmark-goto ()
  "Open ANY bookmark"
  (interactive)
  (let (bookmarks filename)
    ;; load bookmarks
    (unless (featurep 'bookmark)
      (require 'bookmark))
    (bookmark-maybe-load-default-file)
    (setq bookmarks (and (boundp 'bookmark-alist) bookmark-alist))

    ;; do the real thing
    (ivy-read "bookmarks:"
              (delq nil (mapcar (lambda (bookmark)
                                  (let (key)
                                    ;; build key which will be displayed
                                    (cond
                                     ((and (assoc 'filename bookmark) (cdr (assoc 'filename bookmark)))
                                      (setq key (format "%s (%s)" (car bookmark) (cdr (assoc 'filename bookmark)))))
                                     ((and (assoc 'location bookmark) (cdr (assoc 'location bookmark)))
                                      ;; bmkp-jump-w3m is from bookmark+
                                      (unless (featurep 'bookmark+)
                                        (require 'bookmark+))
                                      (setq key (format "%s (%s)" (car bookmark) (cdr (assoc 'location bookmark)))))
                                     (t
                                      (setq key (car bookmark))))
                                    ;; re-shape the data so full bookmark be passed to ivy-read:action
                                    (cons key bookmark)))
                                bookmarks))
              :action (lambda (bookmark)
                        (bookmark-jump bookmark)))))

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

;;===================================
;; Add action copy-line in avy-action
;; copy line in avy-action
(defun avy-action-copy-line (pt)
  "Copy current line on pt."
  (save-excursion
    (let (str pt2)
      (goto-char pt)
      (beginning-of-line)
      (setq pt2 (point))
      (end-of-line)
      (setq str (buffer-substring pt2 (point)))
      (kill-new str)
      (message "Copied: %s" str)))
  (let ((dat (ring-ref avy-ring 0)))
    (select-frame-set-input-focus
     (window-frame (cdr dat)))
    (select-window (cdr dat))
    (goto-char (car dat))))

;; add avy-action-copy-line
(setq avy-dispatch-alist
  '((?x . avy-action-kill-move)
    (?X . avy-action-kill-stay)
    (?m . avy-action-mark)
    (?n . avy-action-copy)
    (?N . avy-action-copy-line)
    (?i . avy-action-ispell)))

;; start counsel-ag from project root instead of current folder
(defun my-counsel-ag-from-project-root ()
  "Make counsel-ag aware of project root directory."
  (interactive)
  (counsel-ag nil (projectile-project-root)))

(provide 'my-ivy)
;;; my-dired.el ends here
