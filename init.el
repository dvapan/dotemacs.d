;; -*- lexical-binding: t; -*-
;; Do not show the startup screen.
(setq inhibit-startup-message t)
(add-to-list 'load-path "~/.emacs.d/modes/")

;; Don't pop a *Warnings* window for async native-compilation of packages
;; (e.g. dune.el references xref functions loaded lazily). Real errors still
;; land in the echo area; this only stops the buffer from stealing a window.
(setq native-comp-async-report-warnings-errors 'silent)

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

;; Enable relative line numbers with absolute for current line
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

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
(add-to-list 'default-frame-alist '(font . "Monospace-08"))


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


;;; c-mode (cc-mode fallback when tree-sitter grammar is missing)
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

;;; Tree-sitter grammars. php-ts-mode needs phpdoc/html/css/js/jsdoc for
;;; embedded code in templates. Versions are pinned to what the Emacs 30.2
;;; font-lock queries expect (see php-ts-mode--language-source-alist);
;;; grammars from master have incompatible node types.
(setq treesit-language-source-alist
      '((c   "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
             "master" "tsx/src")
        (php "https://github.com/tree-sitter/tree-sitter-php"
             "v0.23.11" "php/src")
        (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc")
        (html "https://github.com/tree-sitter/tree-sitter-html" "v0.23.0")
        (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.0")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                    "v0.23.0")
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.23.0")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (toml "https://github.com/ikatyang/tree-sitter-toml")))

;; Install any missing grammars on startup. Requires a C compiler (cc/gcc).
(dolist (lang (mapcar #'car treesit-language-source-alist))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

(setq major-mode-remap-alist
      '((c-mode        . c-ts-mode)
        (c++-mode      . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)))

(setq c-ts-mode-indent-offset 4
      c-ts-mode-indent-style  'bsd)

;;; TypeScript/TSX via built-in tree-sitter modes.
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;;; YAML via built-in tree-sitter mode (Symfony configs etc.).
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;;; TOML via built-in tree-sitter mode (Cargo.toml, pyproject.toml etc.).
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

;;; Plain config files via built-in conf-mode: dotenv (.env, .env.local
;;; and friends) and ignore files. .ini/.conf/.gitconfig are covered by
;;; Emacs out of the box.
(add-to-list 'auto-mode-alist '("/\\.env\\(\\.[^/]*\\)?\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/\\.\\(git\\|docker\\)ignore\\'" . conf-unix-mode))

;; ~/.local/bin holds phpactor and typescript-language-server. Emacs is
;; launched from dwm, not a login shell, so add it to PATH explicitly.
(let ((local-bin (expand-file-name "~/.local/bin")))
  (when (file-directory-p local-bin)
    (add-to-list 'exec-path local-bin)
    (setenv "PATH" (concat local-bin path-separator (getenv "PATH")))))

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
  (setq ido-use-virtual-buffers nil))

;; Disable recentf-mode to avoid file history
(recentf-mode -1)

(require 'ansi-color)

(defun my-colorize-compilation-buffer ()
  "Apply ANSI color codes and handle hyperlinks in the compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))
    ;; Remove unsupported hyperlinks escape sequences
    (goto-char (point-min))
    (while (re-search-forward "\033]8;;.*?\033\\\\\\(.*?\\)\033]8;;\033\\\\" nil t)
      (replace-match "\\1"))))

(add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)

;; Talk to compilation processes through a pipe, not a pty. CLIs that
;; animate progress with cursor save/restore (symfony, composer, docker)
;; detect the missing terminal and fall back to plain line output.
(define-advice compilation-start (:around (fn &rest args) no-pty)
  (let ((process-connection-type nil))
    (apply fn args)))

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

  ;; No buffer-content completion (dropped company-dabbrev-code / company-dabbrev).
  ;; Only semantic sources: capf (elisp etc.), keywords, files.
  (setq company-backends
        '((company-capf company-keywords company-files))))


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

;;; OCaml via opam: tuareg major mode.
;; Emacs is launched from dwm, not a login shell, so the opam bin dir is not
;; on PATH. Add it explicitly so compile can find dune/ocaml.
(let ((opam-bin (expand-file-name "~/.opam/default/bin")))
  (when (file-directory-p opam-bin)
    (add-to-list 'exec-path opam-bin)
    (setenv "PATH" (concat opam-bin path-separator (getenv "PATH")))))

(use-package tuareg
  :ensure t
  :mode (("\\.ml[iylp]?\\'" . tuareg-mode)))

(use-package dune
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

;; Haskell/GHC error format: filename.hs:line:col
(add-to-list 'compilation-error-regexp-alist
             '("\\([^ \n\t]+\\.hs\\):\\([0-9]+\\):\\([0-9]+\\)"
               1 2 3))

(use-package glsl-mode
    :ensure t)

(use-package go-mode
  :ensure t)

;;; PHP/Symfony via built-in php-ts-mode. Explicit auto-mode entry so it
;;; wins over any stale php-mode autoload.
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))

;; Twig templates: plain highlighting mode, no background processes.
(use-package twig-mode
  :ensure t
  :mode "\\.twig\\'")

;;; LSP via eglot: completion, M-., docs in echo area. No on-the-fly
;;; diagnostics: `eglot-stay-out-of' keeps server diagnostics away from
;;; flymake, so nothing is underlined or flagged while typing. Inlay
;;; hints are text over the code, so they are off too.
;; PHP server: phpactor (~/.local/bin). Per Symfony project, enable
;; container completion with a .phpactor.json: {"symfony.enabled": true}
(use-package eglot
  :hook ((php-ts-mode c-ts-mode c++-ts-mode tuareg-mode
          typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  :config
  (setq eglot-stay-out-of '(flymake)
        eglot-ignored-server-capabilities '(:inlayHintProvider)
        eglot-autoshutdown t
        eglot-events-buffer-config '(:size 0 :format full)
        eglot-sync-connect 0))

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

    ;; Enable Babel support for common languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)   ;; Emacs Lisp
       (shell . t)        ;; Shell scripts (bash, sh)
       (python . t)       ;; Python
       (C . t)            ;; C, C++, D
       (js . t)           ;; JavaScript
       (sql . t)          ;; SQL
       (scheme . t)       ;; Scheme (works with Geiser)
       (haskell . t)      ;; Haskell
       (latex . t)        ;; LaTeX
       (dot . t)          ;; Graphviz
       (makefile . t)     ;; Makefile
       (org . t)          ;; Org-mode itself
       (R . t)            ;; R
       (ruby . t)         ;; Ruby
       (perl . t)         ;; Perl
       (sed . t)          ;; sed
       (awk . t)          ;; awk
       (css . t)          ;; CSS
       (sass . t)         ;; Sass
       (calc . t)         ;; Emacs Calc
       (gnuplot . t)))    ;; Gnuplot

    ;; Additional Babel settings for better experience
    (setq org-babel-python-command "python3")
    (setq org-src-fontify-natively t)        ;; Syntax highlighting in code blocks
    (setq org-src-tab-acts-natively t)       ;; TAB acts as in the language major mode
    (setq org-src-preserve-indentation t)    ;; Preserve indentation in source blocks
    (setq org-edit-src-content-indentation 0) ;; No extra indentation in edit buffer
    ))

;; --------------------------------------------
;; Scheme and Racket: plain editing, no REPL, no checkers,
;; no background processes. Run scripts with `compile' (C-c c).
;; --------------------------------------------

;; Geiser is kept installed only because org-babel scheme blocks need it.
;; It is not hooked into scheme-mode buffers.
(use-package geiser
  :ensure t
  :defer t
  :init
  (setq geiser-active-implementations '(mit))
  (setq geiser-default-implementation 'mit)
  (setq geiser-mode-start-repl-p nil)
  (setq geiser-repl-query-on-kill-p nil)
  (setq geiser-log-verbose nil))

(use-package geiser-mit
  :ensure t
  :defer t
  :config
  (setq geiser-mit-binary "/usr/bin/mit-scheme"))

;; Geiser's autoload attaches itself to scheme-mode; detach it so
;; scheme buffers stay plain.
(remove-hook 'scheme-mode-hook 'geiser-mode--maybe-activate)

;; Same run key as racket-mode.
(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-c") #'recompile))

;; mit-scheme is not installed (xbps package: mit-scheme-c), so run plain
;; scheme files through racket. If mit-scheme appears later, the
;; equivalent command is: mit-scheme --quiet < file.scm
(add-hook 'scheme-mode-hook
          (lambda ()
            (when buffer-file-name
              (setq-local compile-command
                          (concat "racket -f "
                                  (shell-quote-argument
                                   (file-name-nondirectory buffer-file-name)))))))

;; Racket: racket-mode purely as an editing mode. Without racket-xp-mode
;; it runs no back end process at all.
;; Override geiser-racket's autoload that maps .rkt -> scheme-mode.
(use-package racket-mode
  :ensure t
  :pin melpa
  :init
  (setq auto-mode-alist
        (cons '("\\.rkt\\'" . racket-mode)
              (assoc-delete-all "\\.rkt\\'" auto-mode-alist)))
  :config
  (setq racket-program "/usr/bin/racket")
  ;; racket-mode binds C-c C-c to racket-run-module-at-point, which
  ;; starts the REPL back end. Run the script via compile instead.
  (define-key racket-mode-map (kbd "C-c C-c") #'recompile)
  (add-hook 'racket-mode-hook
            (lambda ()
              (when buffer-file-name
                (setq-local compile-command
                            (concat "racket "
                                    (shell-quote-argument
                                     (file-name-nondirectory buffer-file-name))))))))

(use-package macrostep
  :ensure t
  :bind ("C-c e" . macrostep-expand))


