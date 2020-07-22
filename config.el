;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Thiery Louison"
      user-mail-address "thiery.louison@gmail.com")

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

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(if (eq initial-window-system 'x)
  (toggle-frame-maximized)
  (toggle-frame-fullscreen)
  )

(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 24)
      doom-big-font (font-spec :family "Fantasque Sans Mono" :size 22)
      doom-variable-pitch-font (font-spec :family "Roboto Slab" :size 16)
      doom-serif-font (font-spec :family "Roboto Slab")
      projectile-project-search-path '("~/data/code/")
      +doom-dashboard-banner-file (expand-file-name "emacs2.png" doom-private-dir)
      )

(load! "t10.custom.el" doom-private-dir)
(load! "my-agenda.el" doom-private-dir)
;;(load! "org-custom-todo.el" doom-private-dir)
;;(load! "org-bullets.el" doom-private-dir)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :height 4.0))))
 '(doom-modeline-buffer-modified ((t (:foreground "orange")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-directory "~/Sync/orgfiles")
 '(org-export-html-postamble nil)
 '(org-hide-leading-stars t)
 '(org-src-fontify-natively t)
 '(org-startup-folded (quote overview))
 '(org-startup-indented t)
 '(package-selected-packages
   (quote
    (forge htmlize org-pdfview pdf-tools atomic-chrome mingus simple-mpc pcre2el ag wgrep-ag wgrep haskell-mode aggressive-indent treemacs-projectile treemacs origami dumb-jump clj-refactor cider ggtags git-timemachine git-gutter magit hydra default-text-scale smartparens projectile auctex dired-subtree dired-narrow tern-auto-complete tern js2-refactor ac-js2 js2-mode emmet-mode web-mode iedit expand-region multiple-cursors hungry-delete beacon virtualenvwrapper elpy flycheck doom-modeline doom-themes tao-theme poet-theme faff-theme zerodark-theme alect-themes moe-theme base16-theme zenburn-theme color-theme-modern irony-eldoc company-irony company-jedi company counsel ace-window noflet org-bullets which-key posframe try edit-server-htmlize ox-reveal org-re-reveal emms)))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"]))


(use-package ox-reveal
  :ensure t
  :config
    (require 'ox-reveal)
    (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
    (setq org-reveal-mathjax t)
)
(use-package htmlize
    :ensure t)
;;(org-babel-load-file (expand-file-name "myinit.org" doom-private-dir))
(beacon-mode t)
(setq beacon-color "orange")
(setq auth-source "~/.doomd.d/authinfo.gpg")
