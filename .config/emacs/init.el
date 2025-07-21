;; completion stuff:
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion.html

(setq default-directory "~/")
(set-face-attribute 'default nil :height 130)
(package-initialize)

(defun thk-state-file (path)
  (concat "~/.local/state/emacs/" path)
  )
(make-directory (thk-state-file "auto-save") t)

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
 '(completion-cycle-threshold t)
 '(column-number-mode t)
 '(custom-enabled-themes '(solarized-dark-high-contrast))
 '(custom-file "~/.config/emacs/custom-save-dump-not-loaded.el")
 '(custom-safe-themes '(
                        "69181b408ef74ce12270736cddd201de626ec5021a9b4d87fb788a18b8c59d1b" ; modus-vivendi
                        "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" ; solarized-dark-high-contrast 2.0
                        default))
 '(delete-old-versions t)
 '(editorconfig-mode t)
 '(enable-remote-dir-locals t) ; test https://debbugs.gnu.org/12145
 '(fido-vertical-mode t)
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
                                (gac-automatically-push-p . t)
                                (gac-automatically-push-p . nil)
                                        ; git-auto-commit-mode
  ))
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(show-trailing-whitespace t)
 '(tab-bar-show 1)
 '(tab-width 4)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 )

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(add-hook 'prog-mode-hook #'(lambda ()
                             (progn
                               (display-line-numbers-mode 1)
                               (show-paren-mode t)
                               (local-set-key "%" 'thk-match-paren)
                               (hs-minor-mode t)
                               )))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; see emacs FAQ: 5.27 How do I show which parenthesis matches the one I'm looking at?
(show-paren-mode 1)

;; Here is some Emacs Lisp that will make the <%> key show the
;; matching parenthesis, like in `vi'.  In addition, if the cursor
;; isn't over a parenthesis, it simply inserts a % like normal.
(defun thk-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun sm (&optional start end) "run sm with the current region as input"
  (interactive "r")
  (shell-command-on-region start end "/usr/games/sm -")
)

(use-package ace-window
  :bind (
         ("M-o" . ace-window)
         )
  )

(use-package apache-mode
 )

(use-package atomic-chrome
 )

(use-package bookmark
  :custom
  (bookmark-default-file (thk-state-file "bookmarks"))
  (bookmark-save-flag 1)
)

;; (use-package casual
;;   :ensure t
;;   :defer 1
;;   :config
;;   (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
;;   (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
;;   (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)
;;   (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
;;   (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
;;   (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
;;   (keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
;;   (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
;;   (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
;;   (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
;;   (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
;;   (keymap-global-set "M-g" #'casual-avy-tmenu)
;;   (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)
;;   (keymap-global-set "C-o" #'casual-editkit-main-tmenu)
;;   )

(use-package company
  :ensure t
  )

;(use-package company-lsp
;  :ensure t
;  )

(use-package eglot
  :ensure t
  :custom
  (eglot-connect-timeout 120)
  )

(use-package envrc
  :hook (after-init . envrc-global-mode)
)

(use-package git-auto-commit-mode
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

(use-package magit
  :ensure t
  :bind (
         ("C-x g" . 'magit-status)
         ("C-x M-g" . 'magit-dispatch)
         ("C-c g" . 'magit-file-dispatch)
    )
  )

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "pandoc -f markdown -t html")
)

(use-package nix-mode
  :ensure t
)

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
)

(use-package org
  :ensure t
  :custom
  (initial-major-mode 'org-mode)
  (org-agenda-files '("~/org"))
  (org-clock-persist t)
  (org-clock-persist-file (thk-state-file "org-clock-save.el"))
  (org-hide-block-startup t)
  (org-latex-hyperref-template
   "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 colorlinks=true,
 urlcolor=[rgb]{0.1,0.1,1},
 linkcolor=[rgb]{0,0,0}
 }")
  (org-list-allow-alphabetical t)
  (org-modules '(org-protocol))
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-src-fontify-natively t)
  (org-startup-folded t)
  (org-use-speed-commands t)
  )

;;(use-package org-drill
;;  :ensure t
;;)

(use-package paredit
  :ensure t
  )

;; (use-package projectile
;;   :ensure t
;;   )

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode) . #'rainbow-delimiters-mode)
  )

(use-package systemd
  :ensure t
  )

(use-package telega
  ;; :bind (:map global-map ("C-x c" . (lambda (p) (interactive "P")
  ;;                                   (if p (call-interactively #'telega-chat-with)
  ;;                                     (telega))))
  ;;        :map telega-chat-button-map ("a" . ignore))
  :config
  (telega-mode-line-mode 1)

  :custom
  (telega-emoji-use-images nil)
  (telega-directory "~/.local/share/telega")
  ;; (telega-completing-read-function #'completing-read)
  (telega-video-player-command "vlc")
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
;  (tramp-use-ssh-controlmaster-options nil)
  :config
  ;; make eglot over tramp happy
  ;; https://emacs.stackexchange.com/questions/74651/how-to-configure-eglot-over-tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (tramp-change-syntax 'simplified)
  )

(use-package transient
  :ensure t
  :custom
  (transient-history-file (thk-state-file "transient/history.el"))
  )

(use-package treemacs
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple)
  :bind
  (:map global-map ("C-M-t" . treemacs))
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

(use-package yaml-mode
  )

(use-package yasnippet
  :ensure t
  )

(use-package yasnippet-snippets
  :ensure t
  )

(require 'casual)
;(keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
(keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
;(keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)
;(keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
;(keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
;(keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
(keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
;(keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
;(keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
;(keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
;(keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
;(keymap-global-set "M-g" #'casual-avy-tmenu)
;(keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)
(keymap-global-set "C-o" #'casual-editkit-main-tmenu)
