;;; package --- Summary
;;; Commentary:

;; My org-mode / markdown-mode setting.

;;; Code:

;; set org mode for *.org and *.org_archive
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

;; markdown export
(require 'ox-md nil t)

;; ======
;; bullet
(require 'org-bullets)

;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
;; the more pointed, the more important
(eval-after-load 'org-bullets
  '(setq org-bullets-bullet-list '("●" "✹" "✭" "✦" "■" "▲" )))

;; ====
;; hook
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)
            (git-gutter+-mode)))
(add-hook 'markdown-mode-hook
          (lambda ()
            (git-gutter+-mode)))

;; ======
;; format
;; default to indent outline
;; if set t, then org-hide-leading-stars will be always t
(setq org-startup-indented nil)

;; default to hide stars
(setq org-hide-leading-stars nil)

;; use org-completion-use
(setq org-completion-use-ido t)

;; org imenu
(setq org-imenu-depth 3)

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
                                 (org-store-link-functions)))
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
        ((equal (bound-and-true-p org-agenda-buffer-name) (buffer-name))
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
