(unless (package-installed-p 'use-package)
  (progn
    (message "WARNING: use-package not installed!")
    (throw 'no-use-package "use-package not installed!")
    )
  )

(package-initialize)

(use-package system-packages
  :custom
  (system-packages-package-manager 'apt)
  (system-packages-use-sudo t)
  )

(defun thk-state-file (path)
  (concat "~/.local/state/emacs/" path)
  )
(make-directory (thk-state-file "auto-save") t)

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-check-vc-info t)
 `(auto-save-file-name-transforms '((".*" ,(thk-state-file "auto-save/") t)))
 '(auto-save-list-file-prefix (thk-state-file "auto-save-list"))
 '(backup-by-copying t)
 `(backup-directory-alist '(("." . ,(thk-state-file "backup"))))
 '(blink-cursor-mode t)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(custom-file "~/.config/emacs/custom-save-dump-not-loaded.el")
 '(custom-safe-themes '("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default)) ; solarized
 '(delete-old-versions t)
 '(fill-column 78)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kept-new-versions 6)
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(require-final-newline nil)
 '(safe-local-variable-values '(
    (gac-automatically-push-p . t) ; git-auto-commit-mode
  ))
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(show-trailing-whitespace t)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-package-ensure-function 'use-package-ensure-debian)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 )

(use-package git-auto-commit-mode
  :ensure t
)

(use-package backup
  :custom
  (bookmark-default-file (thk-state-file "bookmarks"))
  (bookmark-save-flag 1)
)

(use-package company
  :ensure t
  )

;(use-package company-lsp
;  :ensure t
;  )

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

(use-package ido
  :ensure t
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-mode 'both nil (ido))
  (ido-save-directory-list-file (thk-state-file "ido_last"))
  (ido-use-filename-at-point 'guess)
  (ido-use-url-at-point t)
  )

(use-package lsp-haskell
  :ensure t
  :custom
  (lsp-haskell-server-args '("--debug" "--logfile" "/tmp/hls.log"))
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

(use-package markdown-mode
  :ensure t
)

(use-package org
  :ensure t
  :custom
  (initial-major-mode 'org-mode)
  (org-agenda-files '("~/org"))
  (org-clock-continuously t)
  (org-clock-persist t)
  (org-clock-persist-file (thk-state-file "org-clock-save.el"))
  (org-hide-block-startup t)
  (org-list-allow-alphabetical t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-src-fontify-natively t)
  (org-startup-folded t)
  (org-use-speed-commands t)
  )

(use-package org-drill
  :ensure t
)

(use-package paredit
  :ensure t
  )

(use-package projectile
  :ensure t
  )

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode) . #'rainbow-delimiters-mode)
  )

(use-package solarized-theme
  :ensure t
  :custom
  (custom-enabled-themes '(solarized-dark))
  )

(use-package systemd
  :ensure t
  )

(use-package time
  :custom
  (display-time-world-list
    '(("Europe/Berlin" "Berlin")
      ("America/New_York" "New York")
      ("America/Los_Angeles" "San Francisco")
      ("Europe/Bucharest" "Bucarest")
      ("Asia/Tokyo" "Tokyo")))
  )

(use-package tramp
  :custom
  (tramp-persistency-file-name (thk-state-file "tramp-connection-history"))
  )

(use-package transient
  :ensure t
  :custom
  (transient-history-file (thk-state-file "transient/history.el"))
  )

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse nil (uniquify))
)

(use-package visual-fill-column
  :ensure t
  )

(use-package woman
  :custom
  (woman-fill-column 100)
  (woman-fill-frame nil)
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
