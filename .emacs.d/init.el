(setq user-full-name "Yohanes Bandung Bondowoso"
      user-mail-address "hi@ybbond.id")

(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
(setq backup-by-copying t)


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
  ;; (("s-p l" . projectile--find-file))
  :config
  ;; (unbind-key "s-p l" projectile-mode-map)
  ;; (setq projectile-project-search-path '("~/pbond/"))
  (setq projectile-completion-system 'ivy))

;; https://github.com/DiegoVicen/my-emacs#fill-the-exec-path-variable
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "DESKTOP_SESSION" "JAVA_HOME"))
  (exec-path-from-shell-initialize))

(use-package paredit
  :config
  (unbind-key "C-j" paredit-mode-map))

(use-package slime
  :config
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
  (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))
            ))

(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	    ivy-count-format "%d/%d ")
  :bind (("C-s" . swiper)
	     ("C-c h f" . counsel-describe-function)
	     ("C-c h v" . counsel-describe-variable)
	     ("M-i" . counsel-imenu)
	     ("M-x" . counsel-M-x)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x b" . list-buffers)
	     :map ivy-minibuffer-map
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
  ;; (setq cider-eldoc-display-for-symbol-at-point nil)
  :hook ((clojure-mode . cider-mode)
         (cider-mode . paredit-mode)
         (cider-repl-mode . paredit-mode) (cider-mode . company-mode)
         (cider-repl-mode . company-mode)
         (cider-mode . cider-company-enable-fuzzy-completion)
         (cider-repl-mode . cider-company-enable-fuzzy-completion)))

(use-package lsp-dart
  :ensure t
  :demand t
  ;; :init
  ;; (dap-register-debug-template "Staging No Sound Null Safety"
  ;;                              (list
  ;;                               :args '(
  ;;                                       "--flavor=staging"
  ;;                                       "--no-sound-null-safety"
  ;;                                       )))
  :config
  (setq lsp-dart-sdk-dir "/Users/yohanesbandung/fvm/versions/2.5.0/bin/cache/dart-sdk/")
  (setq lsp-dart-flutter-sdk-dir "/Users/yohanesbandung/fvm/versions/2.5.0/")
  (setq lsp-dart-outline-position-params
        '((side . right)
          (slot . 2)
          (window-width . 35)))
  (setq lsp-dart-flutter-outline-position-params
        '((side . right)
          (slot . 2)
          (window-width . 35)))
  (setq lsp-dart-closing-labels-prefix " →")
  (setq lsp-dart-show-todos t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-/"))
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
         (dart-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)))
(use-package lsp-ui
  :ensure t
  :config
  (lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  :hook (lsp-mode . lsp-ui-mode))



(load-theme 'tango-dark)

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
 '(git-gutter:update-interval 1)
 '(package-selected-packages
   '(ripgrep rg magit lsp-dart geiser-mit helpful projectile company-lsp lsp-ui lsp-mode undo-fu git-gutter-fringe counsel swiper ivy company exec-path-from-shell cider clojure-mode which-key slime paredit use-package macrostep)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; KEYBINDINGS

(defun insert-new-line-below ()
  "Add a new line below the current line."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "C-o") 'insert-new-line-below)

(defun insert-new-line-above ()
  "Add a new line above the current line."
  (interactive)
  (let ((oldpos (point)))
    (previous-line)
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "C-S-o") 'insert-new-line-above)

(global-set-key (kbd "C-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key (kbd "C-S-j") 'join-line)

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

(global-unset-key (kbd "C-x C-k"))
(global-set-key (kbd "C-x C-k") 'ido-kill-buffer)

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

(defun open-user-init-file ()
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

