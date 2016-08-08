;;; package --- Summary
;;; Commentary:

;; My org-mode setting.

;;; Code:

;; set org mode for *.org and *.org_archive
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

;; bueatify bullet
(require 'org-bullets)
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)
            (git-gutter+-mode)))

;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
;; the more pointed, the more important
;; (eval-after-load 'org-bullets
;;   '(setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●" )))

;; default to indent outline
;; if set t, then org-hide-leading-stars will be always t
(setq org-startup-indented nil)

;; default to hide stars
(setq org-hide-leading-stars nil)

;; org files location
(setq org-directory "~/code/mine/org")

;; use org-completion-use
(setq org-completion-use-ido t)

;; org clock
(setq org-clock-persist 't)
(setq org-clock-persist-query-resume nil)
(org-clock-persistence-insinuate)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-log-done t)
(setq org-clock-idle-time 30)

;; Already use pomodoro. Don't need to show clock in mode-line
;; (setq org-clock-clocked-in-display nil)

;; org todo key-words
(setq org-todo-keywords
      '((sequence "TODO(t)" "WORK(w)" "RUNG(r)" "|" "DONE(d)")))

(setq org-agenda-custom-commands
      '(("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
        ;; other commands here
        ))

;; use pomodoro
;; (require 'pomodoro)
;; (defun my-pomodoro-add-to-mode-line ()
;;   (setq-default mode-line-format
;;                 (cons mode-line-format '(pomodoro-mode-line-string))))
;; (my-pomodoro-add-to-mode-line)
;; (setq pomodoro-work-time 40)
;; (setq pomodoro-play-sounds nil)

;; start org clock when the state is switched to WORK
(defun org-clock-in-if-work ()
  "Clock in when the task is marked STARTED."
  (if (and (string= org-state "WORK")
           (not (string= org-last-state org-state)))
      (progn
        ;; (pomodoro-start nil)
        (org-clock-in))
    ;; stop pomodoro
    ;; (when pomodoro-timer
    ;;   (pomodoro-stop))
    ;; clock out
    (when (and (or (string= org-state nil) (string= org-state "TODO"))
               (org-clocking-p))
      (org-clock-out))))
(add-hook 'org-after-todo-state-change-hook
          'org-clock-in-if-work)

(setq org-agenda-start-with-log-mode t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-sticky t)
(setq org-agenda-span 'day)

;; agenda format
(setq org-agenda-use-time-grid t)
(setq org-agenda-todo-keyword-format "%-1s")
(setq org-agenda-prefix-format "%?-12t% s")

(setq org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "REF")))
(setq org-imenu-depth 3)
(setq org-agenda-archives-mode t)

;; don't destroy window configuration
(setq org-agenda-window-setup 'current-window)
(setq org-src-window-setup 'current-window)
(defadvice org-agenda-get-restriction-and-command
    (around nm-org-agenda-get-restriction-and-command activate)
  (flet ((delete-other-windows () nil))
    ad-do-it))

;; get the current page from Safari
(defun jcs-get-link (link)
  "Retrieve URL from current Safari page and prompt for description.
Insert an Org link at point."
  (interactive "sLink Description: ")
  (let ((result (shell-command-to-string
                 "osascript -e 'tell application \"Safari\" to return URL of document 1'")))
    (insert (format "[[%s][%s]]" (org-trim result) link))))

;; org capture template
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/code/mine/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n")
        ("j" "Journal" entry (file+datetree "~/code/mine/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))

(defun my-org-open-at-point (&optional arg reference-buffer)
  "Open link at or after point.
If there is no link at point, this function will search forward up to
the end of the current line.
Normally, files will be opened by an appropriate application.  If the
optional prefix argument ARG is non-nil, Emacs will visit the file.
With a double prefix argument, try to open outside of Emacs, in the
application the system uses for this file type."
  (interactive "P")
  ;; if in a code block, then open the block's results
  (unless (call-interactively #'org-babel-open-src-block-result)
    (org-load-modules-maybe)
    (move-marker org-open-link-marker (point))
    (setq org-window-config-before-follow-link (current-window-configuration))
    (org-remove-occur-highlights nil nil t)
    (cond
     ((and (org-at-heading-p)
           (not (org-at-timestamp-p t))
           (not (org-in-regexp
                 (concat org-plain-link-re "\\|"
                         org-bracket-link-regexp "\\|"
                         org-angle-link-re "\\|"
                         "[ \t]:[^ \t\n]+:[ \t]*$")))
           (not (get-text-property (point) 'org-linked-text)))
      (or (let* ((lkall (org-offer-links-in-entry (current-buffer) (point) arg))
                 (lk0 (car lkall))
                 (lk (if (stringp lk0) (list lk0) lk0))
                 (lkend (cdr lkall)))
            (mapcar (lambda(l)
                      (search-forward l nil lkend)
                      (goto-char (match-beginning 0))
                      (org-open-at-point))
                    lk))
          (progn (require 'org-attach) (org-attach-reveal 'if-exists))))
     ((run-hook-with-args-until-success 'org-open-at-point-functions))
     ((and (org-at-timestamp-p t)
           (not (org-in-regexp org-bracket-link-regexp)))
      (org-follow-timestamp-link))
     ((and (or (org-footnote-at-reference-p) (org-footnote-at-definition-p))
           (not (org-in-regexp org-any-link-re)))
      (org-footnote-action))
     (t
      (let (type path link line search (pos (point)))
        (catch 'match
          (save-excursion
            (or (org-in-regexp org-plain-link-re)
                (skip-chars-forward "^]\n\r"))
            (when (org-in-regexp org-bracket-link-regexp 1)
              (setq link (org-extract-attributes
                          (org-link-unescape (org-match-string-no-properties 1))))
              (while (string-match " *\n *" link)
                (setq link (replace-match " " t t link)))
              (setq link (org-link-expand-abbrev link))
              (cond
               ((or (file-name-absolute-p link)
                    (string-match "^\\.\\.?/" link))
                (setq type "file" path link))
               ((string-match org-link-re-with-space3 link)
                (setq type (match-string 1 link) path (match-string 2 link)))
               ((string-match "^help:+\\(.+\\)" link)
                (setq type "help" path (match-string 1 link)))
               (t (setq type "thisfile" path link)))
              (throw 'match t)))

          (when (get-text-property (point) 'org-linked-text)
            (setq type "thisfile"
                  pos (if (get-text-property (1+ (point)) 'org-linked-text)
                          (1+ (point)) (point))
                  path (buffer-substring
                        (or (previous-single-property-change pos 'org-linked-text)
                            (point-min))
                        (or (next-single-property-change pos 'org-linked-text)
                            (point-max)))
                  ;; Ensure we will search for a <<<radio>>> link, not
                  ;; a simple reference like <<ref>>
                  path (concat "<" path))
            (throw 'match t))

          (save-excursion
            (when (or (org-in-regexp org-angle-link-re)
                      (let ((match (org-in-regexp org-plain-link-re)))
                        ;; Check a plain link is not within a bracket link
                        (and match
                             (save-excursion
                               (save-match-data
                                 (progn
                                   (goto-char (car match))
                                   (not (org-in-regexp org-bracket-link-regexp)))))))
                      (let ((line_ending (save-excursion (end-of-line) (point))))
                        ;; We are in a line before a plain or bracket link
                        (or (re-search-forward org-plain-link-re line_ending t)
                            (re-search-forward org-bracket-link-regexp line_ending t))))
              (setq type (match-string 1)
                    path (org-link-unescape (match-string 2)))
                (throw 'match t)))
          (save-excursion
            (when (org-in-regexp (org-re "\\(:[[:alnum:]_@#%:]+\\):[ \t]*$"))
              (setq type "tags"
                    path (match-string 1))
              (while (string-match ":" path)
                (setq path (replace-match "+" t t path)))
              (throw 'match t)))
          (when (org-in-regexp "<\\([^><\n]+\\)>")
            (setq type "tree-match"
                  path (match-string 1))
            (throw 'match t)))
        (unless path
          (user-error "No link found"))

        ;; switch back to reference buffer
        ;; needed when if called in a temporary buffer through
        ;; org-open-link-from-string
        (with-current-buffer (or reference-buffer (current-buffer))

          ;; Remove any trailing spaces in path
          (if (string-match " +\\'" path)
              (setq path (replace-match "" t t path)))
          (if (and org-link-translation-function
                   (fboundp org-link-translation-function))
              ;; Check if we need to translate the link
              (let ((tmp (funcall org-link-translation-function type path)))
                (setq type (car tmp) path (cdr tmp))))

          (cond

           ((assoc type org-link-protocols)
            (funcall (nth 1 (assoc type org-link-protocols)) path))

           ((equal type "help")
            (let ((f-or-v (intern path)))
              (cond ((fboundp f-or-v)
                     (describe-function f-or-v))
                    ((boundp f-or-v)
                     (describe-variable f-or-v))
                    (t (error "Not a known function or variable")))))

           ((equal type "mailto")
            (let ((cmd (car org-link-mailto-program))
                  (args (cdr org-link-mailto-program)) args1
                  (address path) (subject "") a)
              (if (string-match "\\(.*\\)::\\(.*\\)" path)
                  (setq address (match-string 1 path)
                        subject (org-link-escape (match-string 2 path))))
              (while args
                (cond
                 ((not (stringp (car args))) (push (pop args) args1))
                 (t (setq a (pop args))
                    (if (string-match "%a" a)
                        (setq a (replace-match address t t a)))
                    (if (string-match "%s" a)
                        (setq a (replace-match subject t t a)))
                    (push a args1))))
              (apply cmd (nreverse args1))))

           ((member type '("http" "https" "ftp" "news"))
            (browse-url
             (concat type ":"
                     (if (org-string-match-p
                          (concat "[[:nonascii:]"
                                  org-link-escape-chars-browser "]")
                          path)
                         (org-link-escape path org-link-escape-chars-browser)
                       path))))

           ((string= type "doi")
            (browse-url
             (concat org-doi-server-url
                     (if (org-string-match-p
                          (concat "[[:nonascii:]"
                                  org-link-escape-chars-browser "]")
                          path)
                         (org-link-escape path org-link-escape-chars-browser)
                       path))))

           ((member type '("message"))
            (browse-url (concat type ":" path)))

           ((string= type "tags")
            (org-tags-view arg path))

           ((string= type "tree-match")
            (org-occur (concat "\\[" (regexp-quote path) "\\]")))

           ((string= type "file")
            (if (string-match "::\\([0-9]+\\)\\'" path)
                (setq line (string-to-number (match-string 1 path))
                      path (substring path 0 (match-beginning 0)))
              (if (string-match "::\\(.+\\)\\'" path)
                  (setq search (match-string 1 path)
                        path (substring path 0 (match-beginning 0)))))
            (setq this-dired (concat (file-name-as-directory path) ".."))
            (dired this-dired))

           ((string= type "shell")
            (let ((buf (generate-new-buffer "*Org Shell Output"))
                  (cmd path))
              (if (or (and (not (string= org-confirm-shell-link-not-regexp ""))
                           (string-match org-confirm-shell-link-not-regexp cmd))
                      (not org-confirm-shell-link-function)
                      (funcall org-confirm-shell-link-function
                               (format "Execute \"%s\" in shell? "
                                       (org-add-props cmd nil
                                         'face 'org-warning))))
                  (progn
                    (message "Executing %s" cmd)
                    (shell-command cmd buf)
                    (if (featurep 'midnight)
                        (setq clean-buffer-list-kill-buffer-names
                              (cons buf clean-buffer-list-kill-buffer-names))))
                (error "Abort"))))

           ((string= type "elisp")
            (let ((cmd path))
              (if (or (and (not (string= org-confirm-elisp-link-not-regexp ""))
                           (string-match org-confirm-elisp-link-not-regexp cmd))
                      (not org-confirm-elisp-link-function)
                      (funcall org-confirm-elisp-link-function
                               (format "Execute \"%s\" as elisp? "
                                       (org-add-props cmd nil
                                         'face 'org-warning))))
                  (message "%s => %s" cmd
                           (if (equal (string-to-char cmd) ?\()
                               (eval (read cmd))
                             (call-interactively (read cmd))))
                (error "Abort"))))

           ((and (string= type "thisfile")
                 (or (run-hook-with-args-until-success
                      'org-open-link-functions path)
                     (and link
                          (string-match "^id:" link)
                          (or (featurep 'org-id) (require 'org-id))
                          (progn
                            (funcall (nth 1 (assoc "id" org-link-protocols))
                                     (substring path 3))
                            t)))))

           ((string= type "thisfile")
            (if arg
                (switch-to-buffer-other-window
                 (org-get-buffer-for-internal-link (current-buffer)))
              (org-mark-ring-push))
            (let ((cmd `(org-link-search
                         ,path
                         ,(cond ((equal arg '(4)) ''occur)
                                ((equal arg '(16)) ''org-occur))
                         ,pos)))
              (condition-case nil (let ((org-link-search-inhibit-query t))
                                    (eval cmd))
                (error (progn (widen) (eval cmd))))))

           (t (browse-url-at-point)))))))
    (move-marker org-open-link-marker nil)
    (run-hook-with-args 'org-follow-link-hook)))

;; remap org key to be consistent with global setting
(defun my-org-mode-keys ()
  "My keybindings for `org-mode'."
  (define-key org-mode-map (kbd "<C-up>") 'org-backward-heading-same-level)
  (define-key org-mode-map (kbd "<C-down>") 'org-forward-heading-same-level)
  (define-key org-mode-map (kbd "<C-left>") 'outline-up-heading)
  (define-key org-mode-map (kbd "<C-right>") 'outline-next-visible-heading)
  (define-key org-mode-map (kbd "<C-S-left>") 'org-shiftleft)
  (define-key org-mode-map (kbd "<C-S-right>") 'org-shiftright)
  (define-key org-mode-map (kbd "C-c O") 'my-org-open-at-point)
  (define-key org-mode-map (kbd "C-c C-o") 'org-open-at-point)
  (define-key org-mode-map (kbd "\e[47;C~") 'org-metaleft) ; M-left
  (define-key org-mode-map (kbd "\e[47;D~") 'org-metaright) ; M-right
  (define-key org-mode-map (kbd "\e[49;C~") 'org-shiftmetaleft) ; M-S-left
  (define-key org-mode-map (kbd "\e[49;D~") 'org-shiftmetaright) ; M-S-right
)
(add-hook 'org-mode-hook 'my-org-mode-keys)

;; remap key in org agenda
(defun my-org-agenda-mode-keys ()
  "My keybindings for `org-agenda'."
  (define-key org-agenda-mode-map (kbd "<C-S-left>") 'org-shiftleft)
  (define-key org-agenda-mode-map (kbd "<C-S-right>") 'org-shiftright))
(add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-keys)

(provide 'my-org)
;;; my-org.el ends here
