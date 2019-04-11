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

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" "\\.jsx\\'"))

(use-package flyspell
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    )
  (setq flyspell-auto-correct-binding (kbd "<S-f12>")))
  :config
  ;; Sets flyspell correction to use two-finger mouse click
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)

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
  :mode ("\\.org\\'" . org-mode)
  :init
  (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
			       ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
			       ((x-list-fonts "Verdana")         '(:font "Verdana"))
			       ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
			       (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
	 (base-font-color     (face-foreground 'default nil 'default))
	 (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces 'user
			    `(org-level-8 ((t (,@headline ,@variable-tuple))))
                            `(org-level-7 ((t (,@headline ,@variable-tuple))))
                            `(org-level-6 ((t (,@headline ,@variable-tuple))))
                            `(org-level-5 ((t (,@headline ,@variable-tuple))))
                            `(org-level-4 ((t (,@headline ,@variable-tuple))))
                            `(org-level-3 ((t (,@headline ,@variable-tuple))))
                            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1))))
                            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil)))))))

(provide 'init)
;;; init.el ends here

