;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; source https://github.com/theianjones/dotfiles/blob/master/.doom.d/+org.el

(setq org_notes "~/Library/Mobile Documents/com\~apple\~CloudDocs/Notes"
; (setq org_notes "~/Notes"
      org-directory org_notes
      deft-directory org_notes
      org-roam-directory org_notes
      org-roam-db-location "~/Library/Mobile Documents/com\~apple\~CloudDocs/Notes/org-roam.db"
      org-id-link-to-org-use-id t
      org-ellipsis " ▼ ")

(use-package! org-roam
  :hook
  (after-init . org-roam-mode))

(after! org-roam
  (add-hook 'after-init-hook 'org-roam-mode)
  :config
  (setq org-roam-graph-viewer "/usr/bin/open")
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+title: ${title}
#+author: ${author}
#+roam_key: ${ref}
#+roam_tags: web_references
- saved :: ${savedDate}
- source :: ${ref}

* Highlights\n"
           :immediate-finish t
           :unnarrowed t
           :config
           (add-hook! 'after-init-hook 'org-roam-db-build-cache)))))

  (setq org-roam-dailies-capture-templates
    '(("d" "daily" plain (function org-roam-capture--get-point)
      "%?"
      :file-name "journals/%<%Y-%m-%d>"
      :immediate-finish t
      :head "#+title: %<%A>, %<%d> %<%B> %<%Y>
#+roam_tags: journals\n"
      :unnarrowed t)))

  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n"
           :immediate-finish t
           :unnarrowed t)
          ("k" "kumparan" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "kumparan/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}
#+roam_tags: kumparan
- backlinks :: [[file:../20200630143644-kumparan.org][kumparan]]"
           :immediate-finish t
           :unnarrowed t)
          ("c" "companies" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}
#+roam_tags: companies\n"
           :immediate-finish t
           :unnarrowed t)
          ("p" "products" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}
#+roam_tags: products\n"
           :immediate-finish t
           :unnarrowed t)
          ("i" "people" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}
#+roam_tags: people\n"
           :immediate-finish t
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
  (setq org-journal-dir "~/Library/Mobile Documents/com\~apple\~CloudDocs/Notes/journals/"
        org-journal-date-prefix "#+title: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y\n#+roam_tags: journals\n"
        org-journal-enable-agenda-integration t
        org-journal-carryover-delete-empty-journal nil))

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'orge-metaup)
  :config
  (setq org-id-link-to-org-use-id nil
        org-pretty-entities t
        org-hide-emphasis-markers t))

(use-package! org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 3001
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

;; (add-hook 'org-roam-server-mode (lambda () (browse-url-chrome "http://localhost:3001")))

;; Refile a heading to another buffer
;; Allows you to refile into different files - specifically to
;; create new 'parent' headings
(setq org-refile-use-outline-path 'file)
;; makes org-refile outline working with helm/ivy
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(defun +org/opened-buffer-files ()
;;   "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))
