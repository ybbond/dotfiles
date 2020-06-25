;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; source https://github.com/theianjones/dotfiles/blob/master/.doom.d/+org.el

;; (setq org_notes "~/Library/Mobile Documents/com\~apple\~CloudDocs/Notes"
(setq org_notes "~/Notes"
      org-directory org_notes
      deft-directory org_notes
      org-roam-directory org_notes
      org-roam-db-location "~/Notes/org-roam.db"
      org-id-link-to-org-use-id t
      org-ellipsis " ▼ ")

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


(use-package! org-roam
  :hook
  (after-init . org-roam-mode))

(after! org-roam
  (add-hook 'after-init-hook 'org-roam-mode)
  (setq org-roam-graph-viewer "/usr/bin/open")
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+title: ${title}\n#+author: ${author}\n#+roam_key: ${ref}\n#+roam_tags: websites\n- source :: ${ref}"
           :unnarrowed t))))

  (setq org-roam-dailies-capture-templates
    '(("d" "daily" plain (function org-roam-capture--get-point)
      "%?"
      :immediate-finish t
      :file-name "journals/%<%Y-%m-%d>"
      :head "#+title: %<%A>, %<%d> %<%B> %<%Y>\n#+roam_tags: journals\n"
      :unnarrowed t)))

  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("t" "tags" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "tags/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_tags: tags"
           :unnarrowed t)
          ("p" "products" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "products/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_tags: products\n"
           :unnarrowed t)
          ("i" "people" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "people/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_tags: people\n"
           :unnarrowed t)))

(use-package! deft
  :after org
  :config
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (setq deft-incremental-search t)
  (add-hook 'after-init-hook 'org-roam-db-build-cache)
  :bind
  ("C-c n d" . deft))

(use-package! org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :config
  (setq org-journal-dir "~/Notes/journals/")
  (setq org-journal-date-prefix "#+title: ")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %d %B %Y\n#+roam_tags: journals\n"))
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
;; (setq org-refile-use-outline-path 'file)
;; makes org-refile outline working with helm/ivy
;; (setq org-outline-path-complete-in-steps nil)
;; (setq org-refile-allow-creating-parent-nodes 'confirm)
;; (defun +org/opened-buffer-files ()
;;   "Return the list of files currently opened in emacs"
;;   (delq nil
;;         (mapcar (lambda (x)
;;                   (if (and (buffer-file-name x)
;;                            (string-match "\\.org$"
;;                                          (buffer-file-name x)))
;;                       (buffer-file-name x)))
;;                 (buffer-list))))

;; (setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))
