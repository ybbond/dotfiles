;;; ~/.doom.d/+ui.el -*- lexical-binding: t; -*-

;; source https://github.com/theianjones/dotfiles/blob/master/.doom.d/+ui.el

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
;; (setq doom-font (font-spec :family "IBM Plex Mono" :size 14))

;; (use-package mixed-pitch
;;   :hook
;;   ;; If you want it in all text modes:
;;   (text-mode . mixed-pitch-mode))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; light theme: white
(setq doom-theme 'doom-one)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Disable clipboard altogether.
;; Use clipboard register to copy-paste with system clipboard!
(setq select-enable-clipboard nil)

;; (after! dired
;;   ;; (setq dired-listing-switches "-aBhl --group-directories-first"
;;   (setq dired-listing-switches "-aBhl"
;;         dired-dwim-target t
;;         dired-recursive-copies (quote always)
;;         dired-recursive-deletes (quote top)))
