(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/lib/ghub/")
(add-to-list 'load-path "~/.emacs.d/lib/magit/")
(add-to-list 'load-path "~/.emacs.d/lib/company/")
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet/")
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet-snippets/")
(add-to-list 'load-path "~/.emacs.d/lib/evil/")

(require 'evil)
(evil-mode 1)

(load "better-defaults.el")
(require 'better-defaults)
(require 'magit)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)


(require 'yasnippet)
(yas-global-mode 1)

(require 'yasnippet-snippets)          

