;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)

;; Highlight current line.
(global-hl-line-mode t)

;; Specify the path to the custom file
(setq custom-file "~/.emacs.d/custom-file.el")

;; Check if `custom-file` exists; if not, create it with default contents
(unless (file-exists-p custom-file)
  (with-temp-file custom-file
    (insert ";; This is the custom file for Emacs customization.\n")))

;; Load the custom file. Note that `load` is used instead of `load-file`
;; because `load` doesn't throw an error if the file doesn't exist, which
;; makes it safer in case the file gets deleted after the check.
(load custom-file 'noerror)

(defalias 'yes-or-no-p 'y-or-n-p)

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

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map)))

(use-package yasnippet
  :ensure t
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
  (yas-global-mode 1))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

;; Enhance M-x to allow easier execution of commands
(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  :bind ("M-x" . smex))

(use-package geiser
  :ensure t)

(use-package quack
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
