;;; package --- Summary
;;; Commentary:
;;; Meantub's Emacs config

;;; Code:
(mapc (lambda (mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode menu-bar-mode scroll-bar-mode))

(setq ring-bell-function 'ignore)

(global-display-line-numbers-mode)

(setq inhibit-startup-screen t)

(add-to-list 'default-frame-alist
	     '(font . "Hack-11"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Setting up packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Theme
(use-package atom-one-dark-theme
  :ensure t
  :init (load-theme 'atom-one-dark t))

(use-package rainbow-delimiters
  :ensure t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package beacon
  :ensure t
  :init (beacon-mode 1))

(use-package color-identifiers-mode
  :ensure t
  :hook (prog-mode . global-color-identifiers-mode))

;; Packages
(use-package evil
  :ensure t
  :init (evil-mode))

(use-package use-package-chords
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  (helm-mode 1))

(use-package auto-complete
  :ensure t
  :init (ac-config-default))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package pretty-mode
  :ensure t
  :init (global-pretty-mode t))

(use-package org-bullets
  :ensure t
  :defer t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Major modes
(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html\\'" "\\.php\\'" "\\.inc\\'"))

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode))

(use-package go-mode
  :ensure t
  :defer t
  :mode ("\\.go\\'"))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package org
  :ensure t
  :defer t
  :mode ("\\.org\\'" . org-mode))

(provide 'init)
;;; init.el ends here

