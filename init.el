(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/lib/ghub/")
(add-to-list 'load-path "~/.emacs.d/lib/magit/")
(add-to-list 'load-path "~/.emacs.d/lib/company/")

(require 'better-defaults)
(require 'magit)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

