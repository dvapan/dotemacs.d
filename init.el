;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)

(windmove-default-keybindings)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

(global-set-key (kbd "M-/") 'hippie-expand)

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
(load custom-file)

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

(load-theme 'modus-vivendi)

;; Additional packages and their configurations

(use-package ido
  :bind ("C-x C-b" . 'ibuffer)
  :init
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtaul-buffers t))

(require 'ansi-color)
(defun my/colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point))))

(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)

(use-package all-the-icons
  :ensure t
  :defer t)

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

(use-package haskell-mode
  :ensure t)

(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.ghcup/bin")))
(setq exec-path (append exec-path '(expand-file-name "~/.ghcup/bin")))

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

(use-package cmake-mode
  :ensure t)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-.") 'nil)

(define-key global-map (kbd "<f9>") 'compile)


(defun duplicate-line-upd ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'duplicate-line-upd)

;; Explicitly remove C-, binding in Org-mode and Org-agenda
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-,") nil))


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
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

