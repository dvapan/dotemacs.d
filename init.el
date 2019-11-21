(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/lib/ghub/")
(add-to-list 'load-path "~/.emacs.d/lib/magit/")
(add-to-list 'load-path "~/.emacs.d/lib/company/")
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet/")
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet-snippets/")

(require 'better-defaults)
(require 'magit)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setenv "GOPATH" "/media/data/projects/go")

(require 'yasnippet)
(yas-global-mode 1)

(require 'yasnippet-snippets)          

