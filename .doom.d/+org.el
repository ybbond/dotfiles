;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; source https://github.com/theianjones/dotfiles/blob/master/.doom.d/+org.el

(use-package! org-roam
  :hook
  (after-init . org-roam-mode))
  (setq org_notes "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")

(after! org-roam
  (add-hook 'after-init-hook 'org-roam-mode)
  :config
  (setq org-roam-directory org_notes)
  (setq org-roam-db-location "~/org/org-roam.db")
  (setq org-roam-link-title-format "[[%s]]")
  (setq org-roam-graph-viewer "/usr/bin/open")
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
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
      :file-name "%<%Y-%m-%d>"
      :immediate-finish t
      :head "#+title: %<%A>, %<%d> %<%B> %<%Y>
#+roam_tags: journals\n
\n
* %<%A>, %<%d> %<%B> %<%Y>"
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
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}
#+roam_tags: kumparan
- backlinks :: [[file:20200630143644-kumparan.org][kumparan]]"
           :immediate-finish t
           :unnarrowed t)
          ("c" "companies" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}
#+roam_tags: companies\n"
           :immediate-finish t
           :unnarrowed t)
          ("m" "medias" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}
#+roam_tags: medias\n"
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
  (setq deft-directory org_notes)
  (add-hook 'after-init-hook 'org-roam-db-build-cache)
  :bind
  ("s-d" . deft))

(use-package! org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c b" . org-journal-previous-entry)
  ("C-c f" . org-journal-next-entry)
  :config
  (setq org-journal-dir org_notes
        org-journal-date-prefix "#+title: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y\n#+roam_tags: journals\n"))

(after! org
  :init
  (require 'find-lisp)
  (setq org-agenda-files (find-lisp-find-files org_notes "tasks\.org$"))
  (setq org-agenda-start-with-log-mode '(state))
  (setq org-agenda-start-day "-9d")
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'orge-metaup)
  :config
  (setq org-directory org_notes
        org-ellipsis " ▼ "
        org-id-link-to-org-use-id nil
        org-pretty-entities t
        org-log-done 'note
        org-log-into-drawer t
        org-log-refile t
        org-src-fontify-natively t
        org-hide-emphasis-markers t)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "INPROGRESS(p!)" "INTEST(i!)" "FEEDBACK(f!)" "HOLD(h!)" "|" "MERGED(m!)" "DONE(d@/!)" "CANCELLED(c!)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "ffff00" :weight normal :underline t)
          ("INPROGRESS" :foreground "#e39ff6" :weight normal :underline t)
          ("INTEST" :foreground "#0098dd" :weight normal :underline t)
          ("FEEDBACK" :foreground "#0098ee" :weight normal :underline t)
          ("HOLD" :foreground "#9f7efe" :weight normal :underline t)
          ("MERGED" :foreground "#50a14f" :weight normal :underline t)
          ("DONE" :foreground "#50a14f" :weight bold :underline t)
          ("CANCELLED" :foreground "#ff6480" :weight normal :underline t)
          ("[-]" :foreground "#e39ff6" :weight bold)
          ("[?]" :foreground "#9f7efe" :weight bold))))

(use-package! org-roam-server
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
