;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current line.
(global-hl-line-mode t)

;; Do not use `init.el` for `custom-*` code - use `custom-file.el`.
(setq custom-file "~/.emacs.d/custom-file.el")

;; Assuming that the code in custom-file is execute before the code
;; ahead of this line is not a safe assumption. So load this file
;; proactively.
(load-file custom-file)

;; Require and initialize `package`.
(require 'package)
(package-initialize)

;; Add `melpa` to `package-archives`.
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))


;; Additional packages and their configurations


(use-package ido
  :bind ("C-x C-b" . 'ibuffer)
  :init
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtaul-buffers t))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :config
  ;; Do not use a different background color for comments.
  (setq spacemacs-theme-comment-bg nil)

  ;; Comments should appear in italics.
  (setq spacemacs-theme-comment-italic t)

  :init
  ;; Use the `spacemacs-dark` theme.
  (load-theme 'spacemacs-dark))


(use-package neotree
  :ensure t
  :bind ("<f12>" . 'neotree-toggle)
  :init
  ;; slow rendering
  (setq inhibit-compacting-font-caches t)
  ;; set icons theme
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  ;; Every time when neotree window is opened,
  ;; let it find current file and jump to node
  (setq neo-smart-open t)
  ;; When running `projectile-switch-project` (C-c p p) `neotree`
  ;; whill change root automaticaly
  (setq projectile-switch-project-action 'neotree-project-action)
  ;; show hidden files
  (setq neo-show-hidden-files t))

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :defer t
  ;; Navigate in completion minibuffer with `C-n` and `C-p`.
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  ;; Provide instant autocompletion.
  (setq company-idle-delay 0.3)

  :init
  ;; Use company mode everywhere.
  (global-company-mode t))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

