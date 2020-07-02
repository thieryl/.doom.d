;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Thiery Louison"
      user-mail-address "tlouison@dsogroup.com")

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-agenda-files (quote ("~/play/org")))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(if (eq initial-window-system 'x)
  (toggle-frame-maximized)
  (toggle-frame-fullscreen)
  )

(setq doom-font (font-spec :family "Source Code Pro" :size 18)
      doom-big-font (font-spec :family "Source Code Pro" :size 18)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 16)
      doom-serif-font (font-spec :family "Source Code Pro" :weight 'light)
      projectile-project-search-path '("~/play/")
      +doom-dashboard-banner-file (expand-file-name "logo.png" doom-private-dir)
      org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(p)" "STARTED(s)" "WAIT(w)" "DONE(d)" | "CANCELLED(k)")
      ))

;;(setq-default header-line-format
;;             (list " " (make-string 79 ?-) "|"))
;;
;;
;; Prefix of C-x

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-x k") 'my-kill-buffer)
(global-set-key (kbd "C-x K") 'my-kill-buffer-and-jump)
(global-set-key (kbd "C-x O") 'my-prev-window)
(global-set-key (kbd "C-x c") 'magit-clone)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key [f12] 'fci-mode)
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#28323e" "#f48fb1" "#53e2ae" "#f1fa8c" "#92b6f4" "#BD99FF" "#79e6f3" "#f8f8f2"])
 '(custom-safe-themes
   (quote
    ("a92e9da0fab90cbec4af4a2035602208cebf3d071ea547157b2bfc5d9bd4d48d" "3d3807f1070bb91a68d6638a708ee09e63c0825ad21809c87138e676a60bda5d" default)))
 '(fci-rule-color "#364455")
 '(jdee-db-active-breakpoint-face-colors (cons "#181e26" "#87DFEB"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#181e26" "#53e2ae"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#181e26" "#3d4c5f"))
 '(objed-cursor-color "#f48fb1")
 '(pdf-view-midnight-colors (cons "#f8f8f2" "#323f4e"))
 '(rustic-ansi-faces
   ["#323f4e" "#f48fb1" "#53e2ae" "#f1fa8c" "#92b6f4" "#BD99FF" "#79e6f3" "#f8f8f2"])
 '(vc-annotate-background "#323f4e")
 '(vc-annotate-color-map
   (list
    (cons 20 "#53e2ae")
    (cons 40 "#87eaa2")
    (cons 60 "#bcf297")
    (cons 80 "#f1fa8c")
    (cons 100 "#f1dc83")
    (cons 120 "#f1bf7a")
    (cons 140 "#f2a272")
    (cons 160 "#e09fa1")
    (cons 180 "#ce9cd0")
    (cons 200 "#BD99FF")
    (cons 220 "#cf95e5")
    (cons 240 "#e192cb")
    (cons 260 "#f48fb1")
    (cons 280 "#c67e9c")
    (cons 300 "#986d88")
    (cons 320 "#6a5c73")
    (cons 340 "#364455")
    (cons 360 "#364455")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
