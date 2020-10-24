(message "entered ~/.config/emacs/init.el")
(unless (package-installed-p 'use-package)
  (progn
    (message "WARNING: use-package not installed!")
    (throw 'no-use-package "use-package not installed!")
    )
  )

(package-initialize)

(use-package system-packages)

(defun use-package-ensure-debian (name args _state &optional _no-refresh)
  (dolist (ensure args)
    (let ((package
           (or (and (eq ensure t) (use-package-as-symbol name))
               ensure)))
      (when package
        (require 'package)
        (unless (package-installed-p package)
          (condition-case-unless-debug err
              (progn
                (system-packages-install (concat "elpa-" (symbol-name package)))
                t)
            (error
             (display-warning 'use-package
                              (format "Failed to install %s: %s"
                                      name (error-message-string err))
                              :error))))))))

(custom-set-variables
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(fill-column 78)
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(system-packages-package-manager 'apt)
 '(system-packages-use-sudo t)
 '(tool-bar-mode nil)
 '(use-package-ensure-function 'use-package-ensure-debian)
 )

(use-package company
  :ensure t
  )

(use-package company-lsp
  :ensure t
  )

(use-package editorconfig
  :ensure t
  )

(use-package flycheck
  :ensure t
  )

(use-package graphviz-dot-mode
  :ensure t
  )

(use-package haskell-mode
  :ensure t
  )

(use-package helpful
  :ensure t
  :bind (
    ("C-h f" . 'helpful-callable)
    ("C-h v" . 'helpful-variable)
    ("C-h k" . 'helpful-key)
    )
  )

(use-package lsp-haskell
  :ensure t
  )

(use-package lsp-mode
  :ensure t
  )

(use-package lsp-ui
  :ensure t
  )

(use-package magit
  :ensure t
  :bind (
    ("C-x g" . 'magit-status)
    )
  )

(use-package org
  :ensure t
  )

(use-package paredit
  :ensure t
  )

(use-package projectile
  :ensure t
  )

(use-package solarized-theme
  :ensure t
  )

(use-package systemd
  :ensure t
  )

(use-package visual-fill-column
  :ensure t
  )

(use-package ws-butler
  :ensure t
  )

(use-package yasnippet
  :ensure t
  )

(use-package yasnippet-snippets
  :ensure t
  )

(debian-run-directories (concat (file-name-directory load-file-name) "/init.d"))
