(setq gc-cons-threshold (* 50 1000 1000))

(add-to-list 'default-frame-alist
             '(font . "Cascadia Code-12"))

(use-package atom-one-dark-theme
  :ensure t
  :init (load-theme 'atom-one-dark t))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (clojure-mode . rainbow-delimiters-mode)))

(use-package beacon
  :ensure t
  :init (beacon-mode 1))

(use-package color-identifiers-mode
  :ensure t
  :hook (prog-mode . global-color-identifiers-mode))

(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode))

(use-package dimmer
  :ensure t
  :init (dimmer-mode))

(use-package focus
  :ensure t)

(use-package composite
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (when (version<= "27.0" emacs-version)
    (let ((alist
           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36 . ".\\(?:\\(>\\)>?\\)")
             (37 . ".\\(?:\\(%\\)%?\\)")
             (38 . ".\\(?:\\(&\\)&?\\)")
             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94 . ".\\(?:\\(=\\)=?\\)")
             (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table))
  )

(use-package evil
  :ensure t
  :init (evil-mode))

(use-package use-package-chords
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1))

(use-package restart-emacs
  :ensure t
  :chords ("kk" . restart-emacs))

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

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package pretty-mode
  :ensure t
  :init (global-pretty-mode t))

(use-package esup
  :ensure t
  :pin melpa
  :commands (esup))

(use-package which-key
  :ensure t
  :init (which-key-mode t))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package web-mode
     :ensure t
     :defer t
     :mode ("\\.html\\'" "\\.php\\'" "\\.inc\\'"))

   (use-package rjsx-mode
     :ensure t
     :defer t
     :mode ("\\.js\\'" "\\.jsx\\'"))
    
   (use-package tide
     :ensure t
     :after (typescript-mode company flycheck)
     :hook ((typescript-mode . tide-setup)
            (typescript-mode . tide-hjl-identifier-mode)
            (before-save . tide-format-before-save)))

   (use-package powershell
     :ensure t
     :defer t
     :mode ("\\.ps1\\'"))

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

   (use-package lua-mode
     :ensure t
     :mode ("\\.lua\\'"))

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
			       `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.2))))
			       `(org-document-title ((t (,@headline ,@variable-tuple :height 1.3 :underline nil)))))))

(setq gc-cons-threshold (* 2 1000 1000))
