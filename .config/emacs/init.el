;; -*- lexical-binding: t; -*-

;; TODO use-package-compute-statistics to t, restart Emacs and call use-package-report

;; TODO review emacs/etc/presets/newcomers-presets-mode.el
;; - https://sachachua.com/blog/2026/04/what-s-in-the-emacs-newcomers-presets-theme/


;; completion stuff:
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion.html

;;; Code:

(setq default-directory "~/")
(set-face-attribute 'default nil :height 130)
(package-initialize)

(defun thk-state-file (path)
  (concat "~/.local/state/emacs/" path))

(make-directory (thk-state-file "auto-save") t)

(use-package emacs
  :init
  (setq custom-file "~/.config/emacs/custom.el")
  (load custom-file t)
  :custom
  (async-shell-command-buffer 'new-buffer)
  (async-shell-command-display-buffer nil)
  (auto-revert-check-vc-info t)
  (auto-save-file-name-transforms `((".*" ,(thk-state-file "auto-save/") t)))
  (auto-save-list-file-prefix (thk-state-file "auto-save-list"))
  (backup-by-copying t)
  (backup-directory-alist `(("." . ,(thk-state-file "backup"))))
  (blink-cursor-mode t)
  (calendar-week-start-day 1)
  ;; (completion-cycle-threshold t)
  (column-number-mode t)
  (custom-buffer-done-kill t)
  (custom-enabled-themes '(solarized-dark-high-contrast))
  (custom-safe-themes '(
                         "69181b408ef74ce12270736cddd201de626ec5021a9b4d87fb788a18b8c59d1b" ; modus-vivendi
                         "46433a4be67ff0f415175d0876ef9435d190f3aa57bb00670febd70ba0ba52e1" ; solarized-dark-high-contrast 2.0
                         default))
  (delete-old-versions t)
  (delete-selection-mode t)
                                        ; show sizes in dires as human readable
  (dired-listing-switches "-alh")
  (editorconfig-mode t)
  (enable-remote-dir-locals t) ; test https://debbugs.gnu.org/12145
  ;; '(fido-vertical-mode t)
  (fill-column 78)
  (global-auto-revert-mode t)
  (indent-tabs-mode nil)
  (indicate-empty-lines t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (isearch-allow-motion t)
  (isearch-lazy-count t)
  (ispell-dictionary "american")
  (kept-new-versions 6)
  (line-number-mode t)
  (menu-bar-mode nil)
  (outline-minor-mode-cycle t)
  (outline-minor-mode-use-buttons 'in-margins)
  (require-final-newline nil)
  (safe-local-variable-values '(
                                 (gac-automatically-push-p . t)
                                 (gac-automatically-push-p . nil)
                                        ; git-auto-commit-mode
                                 ))
  (scroll-bar-mode nil)
  (select-enable-clipboard t)
  (set-mark-command-repeat-pop t)       ; Given `repeat-mode'.
  (shell-kill-buffer-on-exit t)
  ;; see emacs FAQ: 5.27 How do I show which parenthesis matches the one I'm looking at?
  (show-paren-mode t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-show 1)
  (tab-bar-tab-hints 1)
  (tab-width 4)
  ;; see window-min-height
  (temp-buffer-max-height 10)
  (tool-bar-mode nil)
  (use-dialog-box nil)
  (use-file-dialog nil) ; prevents endless loop with filechooser.el https://codeberg.org/rahguzar/filechooser/pulls/23
  (vc-make-backup-files t)
  (version-control t)
  (visible-bell t)
  ;; prevent frequent resizes of completions or other temp-buffer-resize-mode windows
  (window-min-height 7)

;;; newcomers presets
;; https://sachachua.com/blog/2026/04/what-s-in-the-emacs-newcomers-presets-theme/
;;;; Appearance-related options
  (font-use-system-font t)
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (mode-line-compact 'long)

;;;; Mouse-related options
  (context-menu-mode t)
  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t)
 ;; (pixel-scroll-mode t) ;; too slow
 ;; '(pixel-scroll-precision-mode t) ;; see bug#69972
  (mouse-drag-and-drop-region t)
  (mouse-drag-and-drop-region-cross-program t)
  (mouse-drag-mode-line-buffer t)
  (global-xref-mouse-mode t)

;;;; Persistence-related options
  (savehist-mode t)
  (save-place-mode t)
  (recentf-mode t)

;;;; Editing-related options
  (repeat-mode t)
  (imenu-auto-rescan t)
  (view-read-only t)
  (column-number-mode t)
  (kill-do-not-save-duplicates t)
  (scroll-error-top-bottom t)

;;;; Directory management-related options
  (dired-auto-revert-buffer t)
  (dired-mouse-drag-files t)
  (shell-command-prompt-show-cwd t)

;;;; File-related options
  (etags-regen-mode t)
  (vc-auto-revert-mode t)
  (vc-deduce-backend-nonvc-modes t)
  (vc-dir-save-some-buffers-on-revert t)
  (vc-find-revision-no-save t)
  (vc-follow-symlinks t)
  (vc-use-incoming-outgoing-prefixes t)
  (vc-dir-simple-unmark-all-files t)

;;;; Completion-related options
  (which-key-mode t)

;;;; Package-related options
  (package-autosuggest-mode t)
  (package-menu-use-current-if-no-marks nil)

;;;; Frame- and window-related options
  (frame-inhibit-implied-resize t)
  (help-window-select t)
;;  (tab-bar-history-mode t)
 ;; FIXME: If you toggle off the theme, without having run any
 ;; tab-related commands, the tab bar remains visible.
;;  (tab-bar-show 0)
  (window-combination-resize t)

;;;; Programming-related options
  (compilation-scroll-output 'first-error)

  :hook
  ((markdown-mode . visual-line-mode))
  :config
  (dolist (mode '(help-mode Info-mode))
    (add-to-list 'quit-window-kill-buffer mode))
  )

(defun thk-text-prog-mode-init ()
  (setq-local show-trailing-whitespace t)
  (delete-trailing-whitespace-mode))

(defun thk-prog-mode-init ()
  (thk-text-prog-mode-init)
  (display-line-numbers-mode)
  (electric-pair-mode)
  (flymake-mode)
  (flyspell-prog-mode)
  (hs-minor-mode)
  (show-paren-mode)
  ;; (hl-line-mode t)
  (local-set-key "%" 'thk-match-paren))

(defun thk-text-mode-init ()
  (thk-text-prog-mode-init)
  (flyspell-mode))

(add-hook 'prog-mode-hook #'thk-prog-mode-init)
(add-hook 'text-mode-hook #'thk-text-mode-init)

(defun thk-emacs-lisp-init ()
  (rainbow-delimiters-mode)
  ;; Set up outline folding and movement for top-level Emacs Lisp forms.
  (setq-local outline-regexp "^;;;; \\|^;;; \\|^(")
  (outline-minor-mode 1)

  ;; Move the current top-level form up or down.
  (local-set-key (kbd "M-<up>") #'outline-move-subtree-up)
  (local-set-key (kbd "M-<down>") #'outline-move-subtree-down))

(add-hook 'emacs-lisp-mode-hook #'thk-emacs-lisp-init)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Here is some Emacs Lisp that will make the <%> key show the
;; matching parenthesis, like in `vi'.  In addition, if the cursor
;; isn't over a parenthesis, it simply inserts a % like normal.
(defun thk-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(add-to-list 'default-frame-alist '(alpha-background . 0))

(defun thk-desktop-on ()
  (set-frame-parameter (selected-frame) 'alpha-background 0)
  ;(setq-local old-mode-line-format mode-line-format)
  ;(setq-local mode-line-format nil)
  ; Unfortunately there is no hook to add the following line at buffer creation time
  (add-hook 'after-change-functions 'thk-transparency-scratch-hook 0 t)
  )

(defun thk-desktop-off ()
  (set-frame-parameter (selected-frame) 'alpha-background 100)
  ;(if (local-variable-p old-mode-line-format)
  ;    (setq-local mode-line-format old-mode-line-format))
  )

; args parameter necessary for 'after-change-functions
(defun thk-transparency-scratch-hook (&rest args)
  "hook to disable transparency in *scratch* buffer on first keypress"
  (if (= (buffer-size (current-buffer)) 0)
      (thk-desktop-on)
    (thk-desktop-off)
    )
  )

(defun thk-transparency (&rest args)
  "transparency"
  (if (and
       (string= "*scratch*" (buffer-name))
       (= 0 (buffer-size))
       (= 1 (count-windows))
       )
      (thk-desktop-on)
    (thk-desktop-off)
    )
  )

(add-hook 'window-selection-change-functions 'thk-transparency)
(add-hook 'window-configuration-change-hook 'thk-transparency)

(use-package ace-window
  :bind (
         ("M-o" . ace-window)
         )
  )

;(use-package apache-mode)

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

(use-package completion-preview
  :ensure nil
  :demand t
  :bind
  ( :map completion-preview-active-mode-map
    ("M-i" . completion-preview-insert-word)
    ("M-n" . completion-preview-next-candidate)
    ("M-p" . completion-preview-prev-candidate)
    ("M-<return>" . completion-preview-insert)
    ;; With TAB we effectively defer to the *Completions* buffer to
    ;; show more completion candidates at once.
    ("<tab>" . completion-preview-complete))
  :custom
  (completion-preview-minimum-symbol-length 2)
  (global-completion-preview-mode 1)
  :config
  (with-eval-after-load 'org
    (add-to-list 'completion-preview-commands #'org-self-insert-command))
  )

(use-package eglot
  :ensure t
  :custom
  (eglot-connect-timeout 120)
  )

(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds
   '(("https://planet.emacslife.com/atom.xml" emacs it)
     ("http://planet.postgresql.org/rss20.xml" it)
     ("http://planet.haskell.org/atom.xml" it)
     ("http://www.planeteclipse.org/planet/atom.xml" it)
     ("https://this-week-in-rust.org/atom.xml" it)
     ("https://www.osnews.com/feed/" it)
     ("http://planet.debian.org/rss20.xml" it)
     ("https://www.anti-spiegel.ru/feed" news)
     ("https://media.ccc.de/updates.rdf" it)
    )
   (elfeed-set-max-connections 1))
  :config
  (add-to-list 'quit-window-kill-buffer 'elfeed-search-mode)
  (add-to-list 'quit-window-kill-buffer 'elfeed-show-mode)
  )

(use-package envrc
  :hook (after-init . envrc-global-mode)
  )

(use-package eww
  :ensure t
  )

(use-package git-auto-commit-mode
  :ensure t
)

;; (use-package gptel
;;   :after (f)
;;   :custom
;;   (gptel-log-level 'info)
;;   (gptel-default-mode 'org-mode)

;;   :init
;;   (setq
;;    gptel-model 'qwen2.5-coder:3b
;;    gptel-backend
;;    (gptel-make-ollama "ollama"
;;      :host "localhost:11434"
;;      :models
;;      '("qwen2.5-coder:3b"
;;        "qwen2.5-coder:7b")
;;      ))
;;   )

(use-package graphviz-dot-mode
  :ensure t
  )

;; (use-package haskell-mode
;;   :ensure t
;;   )

(use-package helpful
  :ensure t
  :bind (
    ("C-h f" . 'helpful-callable)
    ("C-h v" . 'helpful-variable)
    ("C-h k" . 'helpful-key)
    )
  :config
  (add-to-list 'quit-window-kill-buffer 'helpful-mode)
  )

(use-package magit
  :ensure t
  :bind (
         ("C-x g" . 'magit-status)
         ("C-x M-g" . 'magit-dispatch)
         ("C-c g" . 'magit-file-dispatch)
         )
  :custom
  (magit-buffer-name-format " %x%M%v: %t%x")
  (magit-show-process-buffer-hint nil)
  :config
  (dolist (mode '(magit-diff-mode))
    (add-to-list 'quit-window-kill-buffer mode))
  )

;; maybe set line-spacing 0.2
(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "pandoc -f markdown -t html --defaults=html --standalone")
  (markdown-hide-markup t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t))

(defun my/flex-noinsert-try-completion (string table pred point)
  "Flex `try-completion' that never auto-extends the input on TAB.

Keeps flex's filtering and scoring but suppresses the merge:

  - no candidates           -> nil   (no match)
  - exactly one candidate   -> complete it fully
  - two or more candidates  -> return STRING unchanged, so TAB only
							   pops the *Completions* list.

STRING, TABLE, PRED and POINT are the usual `try-completion' args."
  (let ((all (completion-flex-all-completions string table pred point)))
	(cond
	 ((null all) nil)
	 ((= (safe-length all) 1)
	  (let ((sole (car all)))
		(if (string= sole string) t (cons sole (length sole)))))
	 (t (cons string point)))))

(defun thk/minibuffer-truncate-lines ()
  "Keep minibuffer lines unwrapped."
  (setq truncate-lines t))

;; https://rahuljuliato.com/posts/completions-buffer-is-now-enough
(use-package minibuffer
  :ensure nil
  :bind (:map minibuffer-visible-completions-up-down-map
		      ("C-n" . minibuffer-next-completion)
		      ("C-p" . minibuffer-previous-completion))
  :hook ((minibuffer-setup . cursor-intangible-mode)
		 (minibuffer-setup . thk/minibuffer-truncate-lines))
  :custom
  (completion-auto-help t)
  (completion-auto-select 'second-tab)
  (completion-category-overrides '((eglot-capf (styles flex-noinsert))))
  ;; TODO: no eager display if rau buffer is shown
  ;; https://protesilaos.com/codelog/2026-07-29-emacs-default-minibuffer-completion-overview/
  ;; (completion-eager-display t)
  (completion-eager-update t)
  (completion-ignore-case t)
  (completion-show-help nil)
  (completion-show-inline-help nil)
  (completion-styles '(partial-completion flex initials))
;;  (completion-styles '(basic emacs22 flex))
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-group t)
  (completions-header-format nil)
  (completions-max-height 12)
  (completions-sort 'historical)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  ;; hide default in minibuffer when something else gets typed
  (minibuffer-electric-default-mode t)
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (minibuffer-visible-completions 'up-down)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (tab-always-indent 'complete)
  :config
  (add-to-list 'completion-styles-alist
			   '(flex-noinsert
				 my/flex-noinsert-try-completion
				 completion-flex-all-completions
				 "Flex matching that never extends input on TAB."))
  (put 'flex-noinsert 'completion--adjust-metadata
	   'completion--flex-adjust-metadata))

(use-package mixed-pitch
  :ensure t
  :hook (markdown-mode . mixed-pitch-mode))

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
;;  (initial-major-mode 'org-mode)
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
  (org-return-follows-link t)
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

(use-package systemd
  :ensure t
  )

(use-package telega
  :after (xdg)
  ;; :bind (:map global-map ("C-x c" . (lambda (p) (interactive "P")
  ;;                                   (if p (call-interactively #'telega-chat-with)
  ;;                                     (telega))))
  ;;        :map telega-chat-button-map ("a" . ignore))
  :config
  (telega-mode-line-mode 1)

  :custom
  (telega-emoji-use-images nil)
  (telega-directory  (expand-file-name "telega" (xdg-data-home)))
  (telega-cache-dir  (expand-file-name "telega" (xdg-cache-home)))
  ;; (telega-completing-read-function #'completing-read)
  (telega-video-player-command "vlc")
  )

(use-package term-sessions
  :load-path
  "/home/thk/git/emacs-term-sessions"
  :custom
  (term-sessions-history-lines 1000)
  (term-sessions-preferred-frontend 'ghostel)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*term-session:" . ((display-buffer-reuse-window
                                         display-buffer-in-tab)
                                        . ((reusable-frames . t)))))
  (add-to-list 'quit-window-kill-buffer 'term-sessions-list-mode)
  (defun term-sessions-list-open ()
    "Open session at point, reusing an existing session buffer when present."
    (interactive)
    (let ((session (term-sessions-list--entry-at-point)))
      (quit-window)
      (term-sessions-open session))))

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

(use-package trust-manager
  :defer 1
  :config
  (trust-manager-mode 1))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse nil (uniquify))
)

(use-package visual-fill-column
  :ensure t
  :hook visual-line-mode)

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

; https://kickingvegas.github.io/casual/index.html#SEC_Contents
(require 'casual)
;(keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
;(keymap-set calc-alg-map "C-o" #'casual-calc-tmenu)
(keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
; https://kickingvegas.github.io/casual/EWW-Install.html
(keymap-set eww-mode-map "C-o" #'casual-eww-tmenu)
; (keymap-set eshell-mode-map "C-o" #'casual-eshell-tmenu)
(keymap-set help-mode-map "C-o" #'casual-help-tmenu)
; not yet in nixos?
;(keymap-set image-mode-map "C-o" #'casual-image-tmenu)
;(keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)
;(keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
;(keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
;(keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
(keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
(keymap-set Man-mode-map "C-o" #'casual-man-tmenu)
;(keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
;(keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
                                        ;(keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
(keymap-set org-mode-map "M-m" #'casual-org-tmenu)
;(keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
;(keymap-global-set "M-g" #'casual-avy-tmenu)
;(keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)
(keymap-global-set "C-o" #'casual-editkit-main-tmenu)

(defun thk-profiler-toggle ()
  (interactive)
  (require 'elp)
  (if (and (fboundp 'profiler-cpu-running-p)
           (or (profiler-cpu-running-p) (profiler-memory-running-p)))
      (profiler-stop)
    (profiler-start 'cpu)))

(keymap-global-set "C-s-p" #'thk-profiler-toggle)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; https://cgit.git.savannah.gnu.org/cgit/emacs.git/commit/?id=8a88f3ed5aeece4a7c8edbebc83ed52479fbbfab
;; emacs-devel 15.7.26, New package `outline-occur'
(when (not (functionp 'outline-occur))
  (add-to-list 'display-buffer-alist
               '((major-mode . occur-mode)
		         (display-buffer-pop-up-window)
		         (post-command-select-window . t)))
  (defun outline-occur ()
  "Navigate the current buffer's outline using `occur'.
The `outline-regexp' variable is used to find the beginning of the
outline headings.  The `outline-occur-regexp' buffer-local variable
allows overriding this regexp."
  (interactive)
  (if outline-regexp
      (occur (concat "^\\(?:" outline-regexp "\\)"))
    (user-error "No outline regexp defined"))))




