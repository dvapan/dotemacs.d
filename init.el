;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Set default font
(set-frame-font "-CNR -CodeNewRoman Nerd Font Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1" nil t)


;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)

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
  (setq company-idle-delay 0.1)

  :init
  ;; Use company mode everywhere.
  (global-company-mode t))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package yasnippet
  :ensure t
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/"))

(yas-global-mode 1)

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

(use-package dockerfile-mode
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind 
  ("C-S-c C-S-c" . 'mc/edit-lines)
  ("C->"         . 'mc/mark-next-like-this)
  ("C-<"         . 'mc/mark-previous-like-this)
  ("C-c C-<"     . 'mc/mark-all-like-this)
  ("C-\""        . 'mc/skip-to-next-like-this)
  ("C-:"         . 'mc/skip-to-previous-like-this))

;;; Move Text
(use-package move-text
  :ensure t
  :bind
  ("M-p" . 'move-text-up)
  ("M-n" . 'move-text-down))

(use-package dired-x)

;;; dired
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alhB")

(use-package tramp
  :ensure t)

;; Some setup
(setq tramp-auto-save-directory "/tmp")

(defun my/org-mode-latex-scale ()
  (pcase major-mode
    ('org-mode
     (setq org-format-latex-options
	   (plist-put org-format-latex-options
		      :scale (+ 1.0 text-scale-mode-amount))))
    ('latex-mode
     (setq org-format-latex-options
	   (plist-put org-format-latex-options
		      :scale (+ 1.0 text-scale-mode-amount))))))
  

(add-hook 'text-scale-mode-hook #'my/org-mode-latex-scale)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-.") 'nil)

(define-key global-map (kbd "C-c C-c") 'compile)

(setq org-edit-src-content-indentation 0)
(setq org-confirm-babel-evaluate nil)

;;; c-mode
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))
