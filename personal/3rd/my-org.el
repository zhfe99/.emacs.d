;;; package --- Summary
;;; Commentary:

;; My org-mode / markdown-mode setting.

;;; Code:

;; set org mode for *.org and *.org_archive
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

;; markdown export
(require 'ox-md nil t)

;; use org habit
(require 'org-habit)
(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 7
      org-habit-following-days 7
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

;;=======
;; bullet
(require 'org-bullets)

;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
;; the more pointed, the more important
(eval-after-load 'org-bullets
  '(setq org-bullets-bullet-list '("●" "✹" "✭" "✦" "■" "▲" )))

;;=====
;; hook
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)
            (git-gutter+-mode)))
(add-hook 'markdown-mode-hook
          (lambda ()
            (git-gutter+-mode)))

;; default to indent outline
;; if set t, then org-hide-leading-stars will be always t
(setq org-startup-indented nil)

;; default to hide stars
(setq org-hide-leading-stars nil)

;; org-clock-save file location
(setq org-clock-persist-file (convert-standard-filename
                              (concat user-emacs-directory "savefile/org-clock-save.el")))

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

;; org todo key-words
(setq org-todo-keywords
      '((sequence "TODO(t)" "WORK(w)" "HOLD(h)" "RUNG(r)" "|" "DONE(d)")))

(setq org-agenda-custom-commands
      '(("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
        ;; other commands here
        ))

;;=========
;; pomodoro
(require 'pomodoro)
(defun my-pomodoro-add-to-mode-line ()
  (setq-default mode-line-format
                (cons mode-line-format '(pomodoro-mode-line-string))))
(my-pomodoro-add-to-mode-line)
(setq pomodoro-work-time 40)
(setq pomodoro-play-sounds nil)

;; don't need to show clock in mode-line because already using pomodoro
(setq org-clock-clocked-in-display nil)

;;=================
;; org clock in/out
(defun my-org-clock-in-out ()
  "Clock in when the task is marked WORK."
  (if (and (string= org-state "WORK")
           (not (string= org-last-state org-state)))

      ;; clock in: other state -> work
      (progn
        (pomodoro-start nil)
        (org-clock-in))

    ;; stop pomodoro
    (when pomodoro-timer
      (pomodoro-stop))

    ;; clock out: work -> nil, TODO, HOLD, RUNG
    (when (and (or (string= org-state nil) (string= org-state "TODO") (string= org-state "HOLD") (string= org-state "RUNG"))
               (org-clocking-p))
      (org-clock-out))))
(add-hook 'org-after-todo-state-change-hook
          'my-org-clock-in-out)

;;=======
;; agenda
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-sticky t)
(setq org-agenda-span 'day)
(setq org-agenda-use-time-grid t)
(setq org-agenda-todo-keyword-format "%-1s")
(setq org-agenda-prefix-format "%?-12t% s")
(setq org-agenda-archives-mode t)

;; clock report show "\emsp" in agenda
;; http://emacs.stackexchange.com/questions/9528/is-it-possible-to-remove-emsp-from-clock-report-but-preserve-indentation
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "*"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str ""))))
(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

;; don't destroy window configuration
(setq org-agenda-window-setup 'current-window)
(setq org-src-window-setup 'current-window)
(defadvice org-agenda-get-restriction-and-command
    (around nm-org-agenda-get-restriction-and-command activate)
  (flet ((delete-other-windows () nil))
    ad-do-it))

(setq org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "REF")))
(setq org-imenu-depth 3)

;; ======
;; capture
(require 'org-capture)
(defun my-org-capture (&optional goto keys)
  "Capture something.
\\<org-capture-mode-map>
This will let you select a template from `org-capture-templates', and then
file the newly captured information.  The text is immediately inserted
at the target location, and an indirect buffer is shown where you can
edit it.  Pressing \\[org-capture-finalize] brings you back to the previous state
of Emacs, so that you can continue your work.

When called interactively with a \\[universal-argument] prefix argument GOTO, don't capture
anything, just go to the file/headline where the selected template
stores its notes.  With a double prefix argument \
\\[universal-argument] \\[universal-argument], go to the last note
stored.

When called with a `C-0' (zero) prefix, insert a template at point.

ELisp programs can set KEYS to a string associated with a template
in `org-capture-templates'.  In this case, interactive selection
will be bypassed.

If `org-capture-use-agenda-date' is non-nil, capturing from the
agenda will use the date at point as the default date.  Then, a
`C-1' prefix will tell the capture process to use the HH:MM time
of the day at point (if any) or the current HH:MM time."
  (interactive "P")
  (when (and nil
             (eq major-mode 'org-agenda-mode))
    (setq org-overriding-default-time
          (org-get-cursor-date (equal goto 1))))
  (cond
   ((equal goto '(4)) (org-capture-goto-target))
   ((equal goto '(16)) (org-capture-goto-last-stored))
   (t
    ;; FIXME: Are these needed?
    (let* ((orig-buf (current-buffer))
           (annotation (if (and (boundp 'org-capture-link-is-already-stored)
                                org-capture-link-is-already-stored)
                           (plist-get org-store-link-plist :annotation)
                         (ignore-errors (org-store-link nil))))
           (entry (or nil (org-capture-select-template "t")))
           initial)
      (setq initial (or org-capture-initial
                        (and (org-region-active-p)
                             (buffer-substring (point) (mark)))))
      (when (stringp initial)
        (remove-text-properties 0 (length initial) '(read-only t) initial))
      (when (stringp annotation)
        (remove-text-properties 0 (length annotation)
                                '(read-only t) annotation))
      (cond
       ((equal entry "C")
        (customize-variable 'org-capture-templates))
       ((equal entry "q")
        (error "Abort"))
       (t
        (org-capture-set-plist entry)
        (org-capture-get-template)
        (org-capture-put :original-buffer orig-buf
                         :original-file (or (buffer-file-name orig-buf)
                                            (and (featurep 'dired)
                                                 (car (rassq orig-buf
                                                             dired-buffers))))
                         :original-file-nondirectory
                         (and (buffer-file-name orig-buf)
                              (file-name-nondirectory
                               (buffer-file-name orig-buf)))
                         :annotation annotation
                         :initial initial
                         :return-to-wconf (current-window-configuration)
                         :default-time
                         (or org-overriding-default-time
                             (org-current-time)))
        (org-capture-set-target-location)
        (condition-case error
            (org-capture-put :template (org-capture-fill-template))
          ((error quit)
           (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
           (error "Capture abort: %s" error)))

        (setq org-capture-clock-keep (org-capture-get :clock-keep))
        (if (equal goto 0)
            ;;insert at point
            (org-capture-insert-template-here)
          (condition-case error
              (org-capture-place-template
               (equal (car (org-capture-get :target)) 'function))
            ((error quit)
             (if (and (buffer-base-buffer (current-buffer))
                      (string-match "\\`CAPTURE-" (buffer-name)))
                 (kill-buffer (current-buffer)))
             (set-window-configuration (org-capture-get :return-to-wconf))
             (error "Capture template `%s': %s"
                    (org-capture-get :key)
                    (nth 1 error))))
          (if (and (derived-mode-p 'org-mode)
                   (org-capture-get :clock-in))
              (condition-case nil
                  (progn
                    (if (org-clock-is-active)
                        (org-capture-put :interrupted-clock
                                         (copy-marker org-clock-marker)))
                    (org-clock-in)
                    (org-set-local 'org-capture-clock-was-started t))
                (error
                 "Could not start the clock in this capture buffer")))
          (if (org-capture-get :immediate-finish)
              (org-capture-finalize)))))))))

;;========
;; browser
;; get the current page from Safari
(defun my-insert-current-safari-link (link)
  "Retrieve URL from current Safari page and prompt for description.
Insert an Org link at point."
  (interactive "sLink Description: ")
  (let ((result (shell-command-to-string
                 "osascript -e 'tell application \"Safari\" to return URL of document 1'")))
    (insert (format "[[%s][%s]]" (org-trim result) link))))

;; get the current page from Chrome
(defun my-insert-current-chrome-link ()
  "Retrieve URL from current Chrome page and prompt for description.
Insert an Org link at point."
  (interactive)
  (let ((result (shell-command-to-string
                 "osascript -e 'tell application \"Google Chrome\" to return URL of active tab of front window'"))
        (desc (shell-command-to-string
               "osascript -e 'tell application \"Google Chrome\" to return title of active tab of front window'")))
    (insert (format "[[%s][%s]]" (org-trim result) (org-trim desc)))))

;;========
;; capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/code/mine/org/life.org" "misc")
         "* TODO %?")
        ("j" "Journal" entry (file+datetree "~/code/mine/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))

;;=====
;; link
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

(defun my-org-store-link (arg)
  "\\<org-mode-map>Store an org-link to the current location.
This link is added to `org-stored-links' and can later be inserted
into an org-buffer with \\[org-insert-link].

For some link types, a prefix arg is interpreted.
For links to Usenet articles, arg negates `org-gnus-prefer-web-links'.
For file links, arg negates `org-context-in-file-links'.

A double prefix arg force skipping storing functions that are not
part of Org's core.

A triple prefix arg force storing a link for each line in the
active region."
  (interactive "P")
  (org-load-modules-maybe)
  (if (and (equal arg '(64)) (org-region-active-p))
      (save-excursion
        (let ((end (region-end)))
          (goto-char (region-beginning))
          (set-mark (point))
          (while (< (point-at-eol) end)
            (move-end-of-line 1) (activate-mark)
            (let (current-prefix-arg)
              (call-interactively 'org-store-link))
            (move-beginning-of-line 2)
            (set-mark (point)))))
    (org-with-limited-levels
     (setq org-store-link-plist nil)
     (let (link cpltxt desc description search
                txt custom-id agenda-link sfuns sfunsn)
       (cond

        ;; Store a link using an external link type
        ((and (not (equal arg '(16)))
              (setq sfuns
                    (delq
                     nil (mapcar (lambda (f)
                                   (let (fs) (if (funcall f) (push f fs))))
                                 org-store-link-functions))
                    sfunsn (mapcar (lambda (fu) (symbol-name (car fu))) sfuns))
              (or (and (cdr sfuns)
                       (funcall (intern
                                 (completing-read
                                  "Which function for creating the link? "
                                  sfunsn nil t (car sfunsn)))))
                  (funcall (caar sfuns)))
              (setq link (plist-get org-store-link-plist :link)
                    desc (or (plist-get org-store-link-plist
                                        :description) link))))

        ;; Store a link from a source code buffer
        ((org-src-edit-buffer-p)
         (let (label gc)
           (while (or (not label)
                      (save-excursion
                        (save-restriction
                          (widen)
                          (goto-char (point-min))
                          (re-search-forward
                           (regexp-quote (format org-coderef-label-format label))
                           nil t))))
             (when label (message "Label exists already") (sit-for 2))
             (setq label (read-string "Code line label: " label)))
           (end-of-line 1)
           (setq link (format org-coderef-label-format label))
           (setq gc (- 79 (length link)))
           (if (< (current-column) gc) (org-move-to-column gc t) (insert " "))
           (insert link)
           (setq link (concat "(" label ")") desc nil)))

        ;; We are in the agenda, link to referenced location
        ((equal (org-bound-and-true-p org-agenda-buffer-name) (buffer-name))
         (let ((m (or (get-text-property (point) 'org-hd-marker)
                      (get-text-property (point) 'org-marker))))
           (when m
             (org-with-point-at m
               (setq agenda-link
                     (if (org-called-interactively-p 'any)
                         (call-interactively 'org-store-link)
                       (org-store-link nil)))))))

        ((eq major-mode 'calendar-mode)
         (let ((cd (calendar-cursor-to-date)))
           (setq link
                 (format-time-string
                  (car org-time-stamp-formats)
                  (apply 'encode-time
                         (list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
                               nil nil nil))))
           (org-store-link-props :type "calendar" :date cd)))

        ((eq major-mode 'help-mode)
         (setq link (concat "help:" (save-excursion
                                      (goto-char (point-min))
                                      (looking-at "^[^ ]+")
                                      (match-string 0))))
         (org-store-link-props :type "help"))

        ((eq major-mode 'w3-mode)
         (setq cpltxt (if (and (buffer-name)
                               (not (string-match "Untitled" (buffer-name))))
                          (buffer-name)
                        (url-view-url t))
               link (url-view-url t))
         (org-store-link-props :type "w3" :url (url-view-url t)))

        ((eq major-mode 'image-mode)
         (setq cpltxt (concat "file:"
                              (abbreviate-file-name buffer-file-name))
               link cpltxt)
         (org-store-link-props :type "image" :file buffer-file-name))

        ;; In dired, store a link to the file of the current line
        ((eq major-mode 'dired-mode)
         (let ((file (dired-get-filename nil t)))
           (setq file (if file
                          (abbreviate-file-name
                           (expand-file-name (dired-get-filename nil t)))
                        ;; otherwise, no file so use current directory.
                        default-directory))
           (setq cpltxt (concat "file:" file)
                 link cpltxt
                 desc (file-name-sans-extension (file-name-nondirectory file)))))

        ((setq search (run-hook-with-args-until-success
                       'org-create-file-search-functions))
         (setq link (concat "file:" (abbreviate-file-name buffer-file-name)
                            "::" search))
         (setq cpltxt (or description link)))

        ((and (buffer-file-name (buffer-base-buffer)) (derived-mode-p 'org-mode))
         (setq custom-id (org-entry-get nil "CUSTOM_ID"))
         (cond
          ;; Store a link using the target at point
          ((org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1)
           (setq cpltxt
                 (concat "file:"
                         (abbreviate-file-name
                          (buffer-file-name (buffer-base-buffer)))
                         "::" (match-string 1))
                 link cpltxt))
          ((and (featurep 'org-id)
                (or (eq org-id-link-to-org-use-id t)
                    (and (org-called-interactively-p 'any)
                         (or (eq org-id-link-to-org-use-id 'create-if-interactive)
                             (and (eq org-id-link-to-org-use-id
                                      'create-if-interactive-and-no-custom-id)
                                  (not custom-id))))
                    (and org-id-link-to-org-use-id (org-entry-get nil "ID"))))
           ;; Store a link using the ID at point
           (setq link (condition-case nil
                          (prog1 (org-id-store-link)
                            (setq desc (or (plist-get org-store-link-plist
                                                      :description)
                                           "")))
                        (error
                         ;; Probably before first headline, link only to file
                         (concat "file:"
                                 (abbreviate-file-name
                                  (buffer-file-name (buffer-base-buffer))))))))
          (t
           ;; Just link to current headline
           (setq cpltxt (concat "file:"
                                (abbreviate-file-name
                                 (buffer-file-name (buffer-base-buffer)))))
           ;; Add a context search string
           (when (org-xor org-context-in-file-links arg)
             (let* ((ee (org-element-at-point))
                    (et (org-element-type ee))
                    (ev (plist-get (cadr ee) :value))
                    (ek (plist-get (cadr ee) :key))
                    (eok (and (stringp ek) (string-match "name" ek))))
               (setq txt (cond
                          ((org-at-heading-p) nil)
                          ((and (eq et 'keyword) eok) ev)
                          ((org-region-active-p)
                           (buffer-substring (region-beginning) (region-end)))))
               (when (or (null txt) (string-match "\\S-" txt))
                 (setq cpltxt
                       (concat cpltxt "::"
                               (condition-case nil
                                   (org-make-org-heading-search-string txt)
                                 (error "")))
                       desc (or (and (eq et 'keyword) eok ev)
                                (nth 4 (ignore-errors (org-heading-components)))
                                "NONE")))))
           (if (string-match "::\\'" cpltxt)
               (setq cpltxt (substring cpltxt 0 -2)))
           (setq link cpltxt))))

        ((buffer-file-name (buffer-base-buffer))
         ;; Just link to this file here.
         (setq cpltxt (concat "file:"
                              (abbreviate-file-name
                               (buffer-file-name (buffer-base-buffer)))))
         ;; Add a context string.
         (when (org-xor org-context-in-file-links arg)
           (setq txt (if (org-region-active-p)
                         (buffer-substring (region-beginning) (region-end))
                       (buffer-substring (point-at-bol) (point-at-eol))))
           ;; Only use search option if there is some text.
           (when (string-match "\\S-" txt)
             (setq cpltxt
                   (concat cpltxt "::" (org-make-org-heading-search-string txt))
                   desc "NONE")))
         (setq link cpltxt))

        ((org-called-interactively-p 'interactive)
         (user-error "No method for storing a link from this buffer"))

        (t (setq link nil)))

       ;; We're done setting link and desc, clean up
       (if (consp link) (setq cpltxt (car link) link (cdr link)))
       (setq link (or link cpltxt)
             desc (or desc cpltxt))
       (cond ((equal desc "NONE") (setq desc nil))
             ((and desc (string-match org-bracket-link-analytic-regexp desc))
              (let ((d0 (match-string 3 desc))
                    (p0 (match-string 5 desc)))
                (setq desc
                      (replace-regexp-in-string
                       org-bracket-link-regexp
                       (concat (or p0 d0)
                               (if (equal (length (match-string 0 desc))
                                          (length desc)) "*" "")) desc)))))

       ;; Return the link
       (if (not (and (or (org-called-interactively-p 'any)
                         executing-kbd-macro) link))
           (or agenda-link (and link (org-make-link-string link desc)))
         (push (list link desc) org-stored-links)
         (message "Stored: %s" (or desc link))
         (when custom-id
           (setq link (concat "file:" (abbreviate-file-name
                                       (buffer-file-name)) "::#" custom-id))
           (push (list link desc) org-stored-links))
         (car org-stored-links))))))

(provide 'my-org)
;;; my-org.el ends here
