;;; ~/.doom.d/+ui.el -*- lexical-binding: t; -*-

;; source https://github.com/theianjones/dotfiles/blob/master/.doom.d/+ui.el


;; General Configurations
(use-package! general
  :config
  (general-evil-setup))

;; Evil Configurations
(use-package! evil
  :init
  (general-setq evil-disable-insert-state-bindings t)
  :general
  ([remap evil-emacs-state] 'evil-normal-state)
  (general-imap "C-f" 'delete-forward-char))
;; replaced by evil-better-visual-line
;; (setq evil-want-minibuffer t)

;; Better visual line
;; (use-package! evil-better-visual-line
;;   :config
;;   (evil-better-visual-line-on))

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
