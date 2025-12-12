;; Do not show the startup screen.
(setq inhibit-startup-message t)
(add-to-list 'load-path "~/.emacs.d/modes/")

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(setq-default visible-bell t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4) 
(setq-default make-backup-files nil)
(setq-default compilation-scroll-output t)
(windmove-default-keybindings)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use local fixed theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'deeper-blue t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-+")   'text-scale-increase)
(global-set-key (kbd "C-=")   'text-scale-increase)
(global-set-key (kbd "C--")   'text-scale-decrease)
(global-set-key (kbd "C-c c")  'compile)
(global-set-key (kbd "C-c f") 'find-file-at-point)
(global-set-key (kbd "C-x C-g") 'find-file-at-point)


;; Set default font size for all frames
(add-to-list 'default-frame-alist '(font . "Monospace-14"))


(defun duplicate-line-upd ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (pos-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'duplicate-line-upd)

;; Specify the path to the custom file
(setq custom-file "~/.emacs.d/custom-file.el")
(unless (file-exists-p custom-file)
  (with-temp-file custom-file
    (insert ";; This is the custom file for Emacs customization.\n")))
(load custom-file)


(require 'fasm-mode)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . fasm-mode))

;;; c-mode
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

;; Require and initialize `package`.
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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
  (setq ido-use-virtual-buffers t))

(require 'ansi-color)

(add-hook 'simpc-mode-hook
          (lambda ()
            (interactive)
            (setq-local fill-paragraph-function 'astyle-buffer)))

(defun my-colorize-compilation-buffer ()
  "Apply ANSI color codes and handle hyperlinks in the compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))
    ;; Remove unsupported hyperlinks escape sequences
    (goto-char (point-min))
    (while (re-search-forward "\033]8;;.*?\033\\\\\\(.*?\\)\033]8;;\033\\\\" nil t)
      (replace-match "\\1"))))

(add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-selection-wrap-around t)

  (setq company-backends
        '((company-dabbrev-code company-keywords company-files))))


(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

;; Enhance M-x to allow easier execution of commands
(use-package smex
  :ensure t
  :commands (smex smex-initialize)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  :bind ("M-x" . smex))

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

(use-package rust-mode
  :ensure t)

(use-package projectile
  :ensure t
  :commands (projectile-mode)
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  ;; (setq projectile-project-search-path '("~/projects/"))
  (setq projectile-completion-system 'ido)
  (setq projectile-globally-ignored-directories '("node_modules" ".git" "target"))
  (setq projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package ag
  :ensure t
  :defer t
  :commands (ag ag-regexp ag-project)
  :config
  (setq ag-highlight-search t)  ;; Highlight matches in the search results
  (setq ag-reuse-buffers t))    ;; Reuse the same buffer for results


(use-package docker-compose-mode
  :ensure t)

(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/opt/ghc/bin") ":" (expand-file-name "~/.ghcup/bin")))
(add-to-list 'exec-path (expand-file-name "~/opt/ghc/bin"))
(add-to-list 'exec-path (expand-file-name "~/.ghcup/bin"))

(use-package multiple-cursors
  :ensure t
  :commands (mc/edit-lines mc/mark-next-like-this mc/mark-previous-like-this
             mc/mark-all-like-this mc/skip-to-next-like-this mc/skip-to-previous-like-this)
  :bind 
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-\""        . mc/skip-to-next-like-this)
   ("C-:"         . mc/skip-to-previous-like-this)))

;;; Move Text
(use-package move-text
  :ensure t
  :commands (move-text-up move-text-down)
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down)))

(use-package dired-x)

;;; dired
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alhB")
(setq dired-listing-switches "-alhB --group-directories-first")


(use-package cmake-mode
  :ensure t)

compilation-error-regexp-alist-alist

(add-to-list 'compilation-error-regexp-alist
             '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
               1 2 (4) (5)))

(use-package glsl-mode
    :ensure t)

(use-package go-mode
  :ensure t)

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(defvar my/text-scale-mode-amount 1)

(defun my/text-scale-amount ()
  (if (boundp 'text-scale-mode-amount)
      text-scale-mode-amount
    my/text-scale-mode-amount))

(defun my/org-calc-latex-scale ()
  (+ 1.25 (* 0.5 (my/text-scale-amount))))

(defun my/org-set-latex-scale ()
  (setq-default org-format-latex-options
        (plist-put org-format-latex-options :scale (my/org-calc-latex-scale))))

(defun my/org-refresh-latex-previews ()
  (when (derived-mode-p 'org-mode)
    (my/org-set-latex-scale)
    (org-clear-latex-preview)
    (org-latex-preview)))

(defun my/org-init-latex-scale ()
  (my/org-set-latex-scale))

(defun my/text-scale-increase ()
  (interactive)
  (text-scale-increase 1)
  (setq my/text-scale-mode-amount (1+ my/text-scale-mode-amount))
  (my/org-refresh-latex-previews))

(defun my/text-scale-decrease ()
  (interactive)
  (text-scale-increase -1)
  (setq my/text-scale-mode-amount (1- my/text-scale-mode-amount))
  (my/org-refresh-latex-previews))

(use-package org
  :defer t
  :commands (org-latex-preview org-clear-latex-preview)
  :hook
  (org-mode . my/org-init-latex-scale)
  :bind
  (("C-=" . my/text-scale-increase)
   ("C-+" . my/text-scale-increase)
   ("C--" . my/text-scale-decrease))
  :config
  (with-eval-after-load 'org
    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale (my/org-calc-latex-scale)))
    (setq org-confirm-babel-evaluate nil)
    ;; Add cancel package for LaTeX preview
    (add-to-list 'org-latex-packages-alist '("" "cancel" t))))

;; ----------------------------------------
;; Scheme REPLs via Geiser (MIT Scheme + Racket)
;; --------------------------------------------

(use-package geiser
  :ensure t
  :init
  (setq geiser-active-implementations '(mit racket))
  (setq geiser-default-implementation 'mit)
  (setq geiser-mode-start-repl-p nil) ;; disable auto REPL
  (setq geiser-repl-query-on-kill-p nil)
  (setq geiser-log-verbose nil))      ;; silence deprecated warnings

(use-package geiser-mit
  :ensure t
  :after geiser
  :config
  (setq geiser-mit-binary "/usr/bin/mit-scheme")) ;; adjust path if needed

(use-package geiser-racket
  :ensure t
  :after geiser
  :config
  (setq geiser-racket-binary "/usr/bin/racket")) ;; adjust path if needed

;; Optional: ensure geiser-mode activates in .scm and .rkt files
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))
(add-hook 'scheme-mode-hook #'geiser-mode)

;; Manual REPL launch via: M-x geiser or M-x geiser-mit / geiser-racket
;; REPL <-> Code toggle: C-c C-z


