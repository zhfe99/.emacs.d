;;; package --- Summary
;;; Commentary:

;; My org-mode setting.

;;; Code:

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; better bullet
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; org agenda file
(setq org-agenda-files (list
                        "~/log/org/my/todo.org"
                        "~/log/org/my/history/day.org"))

;; open todo.org
(defun my-open-todo-org ()
  (interactive)
  (find-file "~/log/org/my/todo.org"))

;; open info.org
(defun my-open-info-org ()
  (interactive)
  (find-file "~/log/org/my/info.org"))

;; open think.org
(defun my-open-think-org ()
  (interactive)
  (find-file "~/log/org/my/think.org"))

;; org clock
(setq org-clock-persist 't)
(setq org-clock-persist-query-resume nil)
(org-clock-persistence-insinuate)
(setq org-clock-out-remove-zero-time-clocks t)

;; don't show in mode-line
(setq org-clock-clocked-in-display nil)

;; org todo key-words
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WORK(w)" "RUNG(r)" "FINS(f)" "|" "DONE(d)" "HOLD(h)")))
(setq org-todo-keyword-faces
      (quote (("NEXT" :foreground "yellow" :weight bold))))

;; reset org key to be consistent to global keys
(defun my-org-mode-keys ()
  "My keybindings for `org-mode'."
  (define-key org-mode-map (kbd "<C-up>") 'org-backward-heading-same-level)
  (define-key org-mode-map (kbd "<C-down>") 'org-forward-heading-same-level)
  (define-key org-mode-map (kbd "<C-left>") 'outline-up-heading)
  (define-key org-mode-map (kbd "<C-right>") 'outline-next-visible-heading)
  (define-key org-mode-map (kbd "<S-up>") 'windmove-up)
  (define-key org-mode-map (kbd "<S-down>") 'windmove-down)
  (define-key org-mode-map (kbd "<S-left>") 'windmove-left)
  (define-key org-mode-map (kbd "<S-right>") 'windmove-right)
  (define-key org-mode-map (kbd "M-h") 'helm-mini)
  (define-key org-mode-map (kbd "<H-up>") 'my-push-window-up)
  (define-key org-mode-map (kbd "<H-down>") 'my-push-window-down)
  (define-key org-mode-map (kbd "<H-left>") 'my-push-window-left)
  (define-key org-mode-map (kbd "<H-right>") 'my-push-window-right))
(add-hook 'org-mode-hook 'my-org-mode-keys)

;; use pomodoro
(require 'pomodoro)
(defun my-pomodoro-add-to-mode-line ()
  (setq-default mode-line-format
                (cons mode-line-format '(pomodoro-mode-line-string))))
(my-pomodoro-add-to-mode-line)
(setq pomodoro-work-time 25)
(setq pomodoro-sound-player "mplayerx")
(setq pomodoro-play-sounds nil)

;; start org clock when the state is switched to WORK
(defun org-clock-in-if-work ()
  "Clock in when the task is marked STARTED."
  (if (and (string= org-state "WORK")
           (not (string= org-last-state org-state)))
      (progn
        (pomodoro-start nil)
        (org-clock-in))
    ;; stop pomodoro
    (when pomodoro-timer
      (pomodoro-stop))
    ;; clock out
    (when (and (string= org-state nil)
               (org-clocking-p))
      (org-clock-out))
    ))
(add-hook 'org-after-todo-state-change-hook
          'org-clock-in-if-work)

(setq org-log-done nil)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-sticky t)
(setq org-agenda-span 'day)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-agenda-use-time-grid t)
(setq org-agenda-todo-keyword-format "%-1s")
(setq org-agenda-prefix-format "%?-12t% s")
(setq org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "REF")))
(setq org-imenu-depth 3)

;; don't destroy window configuration
(setq org-agenda-window-setup 'current-window)
(setq org-src-window-setup 'current-window)
(defadvice org-agenda-get-restriction-and-command
    (around nm-org-agenda-get-restriction-and-command activate)
  (flet ((delete-other-windows () nil))
    ad-do-it))

(provide 'my-org)
;;; my-org.el ends here
