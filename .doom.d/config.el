;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Yohanes Bandung Bondowoso"
      user-mail-address "hi@ybbond.dev")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme 'doom-gruvbox)

(setq display-line-numbers-type 'relative)
(setq whitespace-action '(auto-cleanup))
(setq savehist-mode -1)

(setq company-idle-delay nil)

(setq org_dir "~/Dropbox/org/")
(setq org-adapt-indentation nil)

(global-set-key (kbd "s-b") 'ivy-switch-buffer)
(global-set-key (kbd "s-K") 'kill-buffer)
(global-set-key (kbd "s-l") 'org-roam-find-file)

;; Better Copy-Paste and Clipboard Handling
;; Disable Emacs' integration of killring with clipboard
(setq select-enable-clipboard nil)
;; Remove hook for clipboard configurations from Emacs to command mode
(remove-hook 'tty-setup-hook 'doom-init-clipboard-in-tty-emacs-h)
;; Enable ⌘+c and ⌘+v for clipboard handling
(defun rc-clipboard-yank ()
  "Copies the active region to the system clipboard."
  (interactive)
  (when (region-active-p)
    (gui-set-selection 'CLIPBOARD
      (buffer-substring (region-beginning) (region-end)))))
(defun rc-clipboard-paste ()
  "Pastes text from the system clipboard."
  (interactive)
  (let ((text (gui-get-selection 'CLIPBOARD)))
    (when text (insert-for-yank text))))
(map! :v "s-c" #'rc-clipboard-yank
      :nvi "s-v" #'rc-clipboard-paste)
(define-key! :keymaps '(evil-ex-completion-map) "s-v" #'rc-clipboard-paste)
(define-key! :keymaps +default-minibuffer-maps "s-v" #'rc-clipboard-paste)

;; General Configurations
;; (use-package! general
;;   :config
;;   (general-evil-setup))

;; Evil
(use-package! evil
  :init
  (setq evil-move-cursor-back nil)
  ;; (setq evil-respect-visual-line-mode t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-disable-insert-state-bindings t))
  ;; :general
  ;; ([remap evil-emacs-state] 'evil-normal-state)
  ;; (general-nmap "j" 'evil-next-visual-line)
  ;; (general-nmap "k" 'evil-previous-visual-line)
  ;; (general-nmap "gj" 'evil-next-line)
  ;; (general-nmap "gk" 'evil-previous-line)
  ;; (general-imap "C-f" 'delete-forward-char))
(map! :after evil
      :map evil-normal-state-map
      "C-z" 'evil-normal-state
      "j" 'evil-next-visual-line
      "k" 'evil-previous-visual-line
      "gj" 'evil-next-line
      "gk" 'evil-previous-line
      :map evil-insert-state-map
      "C-f" 'delete-forward-char
      "C-z" 'evil-normal-state)

;; Snipe
(after! evil-snipe
  (setq evil-snipe-smart-case t)
  (setq evil-snipe-scope 'whole-buffer)
  (setq evil-snipe-auto-scroll t)
  (setq evil-snipe-repeat-keys t)
  (setq evil-snipe-repeat-scope 'whole-buffer))

;; Ivy
(map! :after ivy
  :map ivy-minibuffer-map
  "S-SPC" nil
  "C-SPC" 'ivy-restrict-to-matches)

;; Dired
(after! dired
  ;; (setq dired-listing-switches "-aBhl --group-directories-first"
  (setq dired-listing-switches "-aBhl"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)))

;; Deft
(use-package! deft
  :after org
  :config
  (setq deft-recursive t)
  ;; (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (setq deft-incremental-search t)
  (setq deft-directory org_dir)
  (add-hook 'after-init-hook 'org-roam-db-build-cache)
  :bind
  ("s-d" . deft))

;; Org-roam
(use-package! org-roam
  :hook
  (after-init . org-roam-mode))
(after! org-roam
  (add-hook 'after-init-hook 'org-roam-mode)
  :config
  (setq org-roam-directory org_dir)
  (setq org-roam-db-location "~/org/org-roam.db")
  (setq org-roam-link-title-format "[[%s]]")
  (set-company-backend! 'org-mode '(company-capf))

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
           :unnarrowed t))))

;; Org-journal
(use-package! org-journal
  :bind
  ("C-c j" . org-journal-new-entry)
  ("C-c b" . org-journal-previous-entry)
  ("C-c f" . org-journal-next-entry)
  :config
  (setq org-journal-dir org_dir
        org-journal-date-prefix "#+title: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y\n"))

;; Org-mode
(after! org
  :init
  (require 'find-lisp)
  (setq org-agenda-files (find-lisp-find-files org_dir "tasks\.org$"))
  (setq org-agenda-start-with-log-mode '(state))
  (setq org-agenda-start-day "-9d")
  (map! :map org-mode-map
        :n "gj" #'evil-next-line
        :n "gk" #'evil-previous-line
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup
        :n "C-s-r" #'org-roam-insert
        :i "C-s-r" #'org-roam-insert
        :n "s-r" #'org-roam-buffer-toggle-display
        :i "s-r" #'org-roam-buffer-toggle-display)
  :config
  (setq org-directory org_dir
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
          ("[?]" :foreground "#9f7efe" :weight bold)))
  (setq org-roam-dailies-capture-templates
    '(("d" "daily" plain (function org-roam-capture--get-point)
      "%?"
      :file-name "%<%Y-%m-%d>"
      :immediate-finish t
      :head "#+title: %<%A>, %<%d> %<%B> %<%Y>
#+roam_tags: journals\n
\n
* %<%A>, %<%d> %<%B> %<%Y>"
      :unnarrowed t))))
