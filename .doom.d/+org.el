;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; source https://github.com/theianjones/dotfiles/blob/master/.doom.d/+org.el

;; (setq org_notes "~/Library/Mobile Documents/com\~apple\~CloudDocs/Notes"
(setq org_notes "~/Notes"
      org-directory org_notes
      org-ellipsis " ▼ "
      deft-directory org_notes)

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Org roam
(after! org-roam
  (setq org-roam-directory org_notes)
  (add-hook 'after-init-hook 'org-roam-mode)
  (setq org-roam-graph-viewer "/usr/bin/open")
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- SOURCE :: ${ref}"
           :unnarrowed t))))
  (setq org-roam-dailies-capture-templates
    '(("d" "daily" plain (function org-roam-capture--get-point)
      "%?"
      :immediate-finish t
      :file-name "journals/%<%Y-%m-%d>"
      :head "#+TITLE: %<%A>, %<%d> %<%B> %<%Y>"
      :unnarrowed t)))
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :config
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-incremental-search t))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :config
  (setq org-journal-dir "~/Notes/journals/")
  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %d %B %Y"))
(setq org-journal-enable-agenda-integration t)

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'orge-metaup))

;; (use-package org-roam-server
;;     :ensure t)

;; (add-hook 'org-roam-server-mode (lambda () (browse-url-chrome "http://localhost:3001")))

;; Refile a heading to another buffer
;; Allows you to refile into different files - specifically to
;; create new 'parent' headings
(setq org-refile-use-outline-path 'file)
;; makes org-refile outline working with helm/ivy
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))
