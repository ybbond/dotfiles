(setq user-full-name "Yohanes Bandung Bondowoso"
      user-mail-address "hi@ybbond.id")

(setq auth-sources '("~/.netrc"))

(make-directory "~/.tmp/emacs/auto-save/" t)

(setq disp-w (display-pixel-width)
      disp-h (display-pixel-height))

(cond
 ((and (= disp-w 1440) (= disp-h 900))
  (setq initial-frame-alist '((top . 190) (left . 425))))
 ((and (= disp-w 2560) (= disp-h 1440))
  (setq initial-frame-alist '((top . 350) (left . 950))))
 ((and (= disp-w 1440) (= disp-h 2560))
  (setq initial-frame-alist '((top . 950) (left . 350)))))

(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
(setq backup-by-copying t)

(setq modus-themes-subtle-line-numbers nil
      modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-hl-line '(intense accented))
(load-theme 'modus-vivendi)

;; https://gist.github.com/belak/ca1c9ae75e53324ee16e2e5289a9c4bc#package-stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; (setq use-package-always-ensure t)
(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))


(use-package helpful
  :ensure t
  :demand t
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  :bind (([remap describe-function] . helpful-function)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)))

(use-package projectile
  :ensure t
  :demand t
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy))

;; https://github.com/DiegoVicen/my-emacs#fill-the-exec-path-variable
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "DESKTOP_SESSION" "JAVA_HOME"))
  (exec-path-from-shell-initialize))

(use-package eshell-syntax-highlighting
  :ensure t
  :hook
  (eshell-mode . eshell-syntax-highlighting-mode))

(use-package paredit
  :config
  (unbind-key "C-j" paredit-mode-map))

;; (use-package slime
;;   :config
;;   (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
;;   (add-hook 'slime-repl-mode-hook
;;             (lambda ()
;;               (define-key slime-repl-mode-map
;;                 (read-kbd-macro paredit-backward-delete-key) nil))
;;             ))

(use-package sly
  :ensure t
  :init
  (sly-setup)
  :config
  ;; (setq inferior-lisp-program "ros dynamic-space-size=3gb -Q run -- --eval \"(ql:quickload :swank)\"")

  ;; (setq inferior-lisp-program "ros -Q run")

  ;; (setq inferior-lisp-program "ros run --eval '(ql:quickload :swank)' --eval '(swank:create-server :dont-close t)'")
  ;; (setq sly-contribs '(sly-mrepl sly-scratch))
  (add-to-list 'sly-contribs 'sly-asdf 'append)
  (add-hook 'sly-mrepl-mode-hook 'enable-paredit-mode))

(use-package counsel
  :ensure t
  :demand t
  :config
  (ivy-mode +1)
  (setq ivy-use-virtual-buffers t
	    ivy-count-format "%d/%d "
        ivy-wrap t)
  :bind (("C-s" . swiper)
         ;("C-s" . swiper-isearch)
         ;("C-r" . swiper-isearch-backward)
	     ("C-c h f" . counsel-describe-function)
	     ("C-c h v" . counsel-describe-variable)
	     ("M-i" . counsel-imenu)
	     ("M-x" . counsel-M-x)
         ("C-x C-b" . ivy-switch-buffer)
         ;; ("C-x b" . list-buffers)
         ("C-x b" . counsel-switch-buffer)
	     :map ivy-minibuffer-map
         ("C-h" . counsel-minibuffer-history)
	     ("RET" . ivy-alt-done)
	     ("C-j" . ivy-done)))

(use-package company
  :ensure t
  :demand t
  :config
  (setq company-tooltip-align-annotations t)
  (progn
    (add-hook 'after-init-hook 'global-company-mode))
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<backtab>")
    #'company-complete-selection))

(use-package forge
  :after magit)

(use-package git-gutter
  :ensure git-gutter-fringe
  :hook ((prog-mode . git-gutter-mode))
  :config (custom-set-variables
	       '(git-gutter:update-interval 1)))

(use-package geiser-mit
  :ensure t)

(use-package clojure-mode
  :ensure t
  :demand t
  :hook (clojure-mode . paredit-mode))

(use-package cider
  :ensure t
  :demand t
  :config
  (setq cider-font-lock-dynamically t)
  (setq cider-eldoc-display-for-symbol-at-point nil)
  :hook ((clojure-mode . cider-mode)
         (cider-mode . paredit-mode)
         (cider-repl-mode . paredit-mode) (cider-mode . company-mode)
         (cider-repl-mode . company-mode)
         (cider-mode . cider-company-enable-fuzzy-completion)
         (cider-repl-mode . cider-company-enable-fuzzy-completion)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package undo-fu
  :config
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "C-?") 'undo-fu-only-redo))

(use-package lsp-mode
  :ensure t
  :demand t
  :config
  (setq lsp-eldoc-enable-hover nil)
  :hook ((clojure-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)))
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'at-point)
  :hook (lsp-mode . lsp-ui-mode))

(use-package command-log-mode
  :ensure t
  :demand t
  :config
  (global-set-key (kbd "C-s-o") 'command-log-mode))



(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0))

(setq show-paren-when-point-inside-paren t
      show-paren-delay 0)
(show-paren-mode t)
(delete-selection-mode t)

(setq-default cursor-type 'bar)

(setq select-enable-clipboard nil)
(setq select-enable-primary nil)

(setq-default ring-bell-function 'ignore)
(setq visible-bell t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4
      js-indent-level 2
      css-indent-offset 2)

(setq recentf-exclude '(
                        "undo-fu-autoloads.el"
                        ))

(defun my-hooks ()
  (display-line-numbers-mode)
  (hl-line-mode)
  (setq truncate-lines t))
(add-hook 'prog-mode-hook #'my-hooks)

(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-step 1
      scroll-conservatively 10000
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

(setq mac-command-modifier 'super
      mac-option-modifier 'meta)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "ybbond-mba.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(git-gutter:update-interval 1)
 '(package-selected-packages
   '(forge eshell-syntax-highlighting sly-asdf sly command-log-mode ripgrep rg magit geiser-mit helpful projectile company-lsp lsp-ui lsp-mode undo-fu git-gutter-fringe counsel swiper ivy company exec-path-from-shell cider clojure-mode which-key paredit use-package macrostep)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; KEYBINDINGS

; suspend-frame
; on macOS, minimizes window
(global-unset-key (kbd "C-x C-z"))

(defun insert-new-line-below ()
  "Add a new line below the current line."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "C-o") 'insert-new-line-below)

(defun insert-new-line-above ()
  "Add a new line above the current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (newline-and-indent)
  (previous-line)
  (delete-forward-char 1)
  (end-of-line))
(global-set-key (kbd "C-S-o") 'insert-new-line-above)

(global-set-key (kbd "C-s-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key (kbd "C-s-S-j") 'join-line)

(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x o") 'delete-blank-lines)

(global-set-key (kbd "C-c [") 'flymake-goto-prev-error)
(global-set-key (kbd "C-c ]") 'flymake-goto-next-error)
(global-set-key (kbd "C-c ;") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c '") 'git-gutter:next-hunk)

(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank)

(global-set-key (kbd "C-s-u") 'scroll-down-command)
(global-set-key (kbd "C-s-d") 'scroll-up-command)

(defun hold-line-scroll-up ()
  "Scroll the page with the cursor in the same line"
  (interactive)
  (let ((next-screen-context-lines
         (count-lines (window-start) (window-end))))
    (scroll-up)))
(global-set-key (kbd "C-s-n") 'hold-line-scroll-up)

(defun hold-line-scroll-down ()
  "Scroll the page with the cursor in the same line"
  (interactive)
  (let ((next-screen-context-lines
         (count-lines (window-start) (window-end))))
    (scroll-down)))
(global-set-key (kbd "C-s-p") 'hold-line-scroll-down)

(global-set-key (kbd "C-x C-k") 'ido-kill-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun backward-paragraph-with-shift-select ()
  (interactive)
  (setq this-command-keys-shift-translated t)
  (call-interactively 'backward-paragraph))
(defun forward-paragraph-with-shift-select ()
  (interactive)
  (setq this-command-keys-shift-translated t)
  (call-interactively 'forward-paragraph))
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-{") 'backward-paragraph-with-shift-select)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-}") 'forward-paragraph-with-shift-select)


;; FUNCTIONS

(defun reload-user-init-file ()
  "Call to reload the Emacs configuration file."
  (interactive)
  (load-file user-init-file))

(defun user-init-file ()
  "Call to open the Emacs configuration file."
  (interactive)
  (find-file user-init-file))


;; PLUGINS

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'inferior-scheme-mode-hook #'enable-paredit-mode)
