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
  
(require 'org)

(org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages
				     '((emacs-lisp . t))))

(org-babel-load-file
 (expand-file-name "settings.org"
		   user-emacs-directory))

(provide 'init)
;;; init.el ends here
