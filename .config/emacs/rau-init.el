;;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(server-start)
(print "running rau-init.el" #'external-debugging-output)
(defun my-dump-buffer-to-fd (buffer-name fd-path)
  "Write the contents of BUFFER-NAME to FD-PATH (e.g., /dev/stdout).
Suppresses the 'Wrote file' message to keep the output clean."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      ;; inhibit-message prevents write-region from printing "Wrote /dev/stdout"
      (let ((inhibit-message t))
        (write-region (point-min) (point-max) fd-path nil)))))

(defun my-dump-logs-on-exit ()
  "Dump *Messages* to stdout and *Warnings* to stderr when Emacs exits.
Only runs in GUI mode to prevent duplicate output in terminal/batch modes."
  ;; In batch mode messages already go to stdout natively.
  (when (display-graphic-p)
    ;; 1. Dump *Messages* to stdout
    (my-dump-buffer-to-fd "*Messages*" "/dev/stdout")

    ;; 2. Dump *Warnings* to stderr if the buffer exists
    (when (get-buffer "*Warnings*")
      (my-dump-buffer-to-fd "*Warnings*" "/dev/stderr"))))

(defvar rau-running-commit)

(if (not (equal "t" (getenv "RAUDEV")))
    (message "loading rau from Emacs load path")
  (message "loading rau from git")
  ;; (add-hook 'kill-emacs-hook #'my-dump-logs-on-exit)
  (setq rau-running-commit (vc-working-revision "/home/thk/git/rau/rau.el"))
  (add-to-list 'load-path "/home/thk/git/rau")
  ;; (setq rau-debug t)
  ;; (setq ewc-debug t)
  )

(defun thk-bury ()
  (interactive)
  (if (string= "*scratch*" (buffer-name))
      (tab-close)
    (switch-to-buffer "*scratch*") (delete-other-windows) (thk-transparency)))

; https://www.masteringemacs.org/article/mastering-key-bindings-emacs
; info: elisp ->  command loop  -> input events  -> function keys

(defun thk-on-rau-ready ()
  (start-process "start rau-emacs-started.target" nil "systemctl" "--user" "start" "rau-emacs-started.target")
  (cl-flet
      ((run (key cmd &rest args)
         (keymap-global-set key `(lambda () (interactive) ,(append (list 'call-process cmd nil nil nil) args)))
         key)
       (set (key f) (keymap-global-set key f) key))
    (rau-bind-keys
     `((;; keys for tab-bar-mode and window management
        ,(set "s-<tab>" #'next-multiframe-window)
        ,(set "S-s-<iso-lefttab>" #'previous-window-any-frame)
        ;; TODO: move tabs to other frames with  tab-bar-move-tab-to-frame
        ,(set "s-<prior>" #'tab-previous)
        ,(set "s-<next>" #'tab-next)
        ,(set "C-s-<prior>" #'tab-bar-move-tab-backward)
        ,(set "C-s-<next>" #'tab-bar-move-tab)
        ,(set "s-<backspace>" #'thk-bury)
        ;; these are automatically bound by tab-bar-mode
        "s-0" "s-1" "s-2" "s-3" "s-4" "s-5" "s-6" "s-7" "s-8" "s-9"
        )
       (
        :needs-focus :layout 0
        "M-o" ; ace-window, bound in use-package
        ,(set "s-x" ctl-x-map)
        ,(set "s-h" help-map)
        ,(set "s-u" #'universal-argument)
        ,(set "s-X" #'execute-extended-command)
        ,(set "s-p" #'xdg-launcher-run-app)
        )
       (
        :layout 0
        ,(run "s-c" "copyq" "show")
        ,(run "s-l" "sh" "-c" "swaylock && sleep 3 && systemctl --user --signal SIGUSR1 kill swayidle.service")
        ,(run "s-P" "systemd-run" "--user" "--quiet" "--property=ExitType=cgroup" "fuzzel" "--log-level=info")
        ,(run "s-w" "systemctl" "--user" "kill" "--signal=SIGUSR1" "waybar.service")
        ,(set "s-f" #'rau-toggle-fullscreen)
        ,(set "s-d" (lambda () (interactive) (message nil)))
        )
       (
        :allow-when-locked
        ,(run "<XF86MonBrightnessUp>" "brightnessctl" "set" "4%+")
        ,(run "<XF86MonBrightnessDown>" "brightnessctl" "set" "4%-")
        ,(run "S-<XF86MonBrightnessUp>" "brightnessctl" "set" "1%+")
        ,(run "S-<XF86MonBrightnessDown>" "brightnessctl" "set" "1%-")
        ,(run "<AudioMute>" "wpctl" "set-mute" "@DEFAULT_SINK@" "toggle")
        ,(run "<AudioMicMute>" "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle")
        ,(run "<AudioRaiseVolume>" "wpctl" "set-volume" "@DEFAULT_SINK@" "0.1+")
        ,(run "<AudioLowerVolume>" "wpctl" "set-volume" "@DEFAULT_SINK@" "0.1-")
        )
       ,(set "s-<return>" #'ghostel)
       ))))

(setopt rau-backend-init-file
        (expand-file-name "rau-be-init.el" (file-name-directory load-file-name)))

(defun thk-init-rau-mode ()
  (when (eq major-mode 'rau-mode)
    (setq cursor-type nil)
    (if (fboundp #'mode-line-invisible-mode)
        (mode-line-invisible-mode)
      (setq mode-line-format nil))))

(use-package rau
  :demand t
  :custom
  (tab-bar-select-tab-modifiers '(super))
  (rau-intercept-prefixes nil)
  :hook
  (rau-ready . thk-on-rau-ready)
  (rau-mode . thk-init-rau-mode)
  :config
  ;; open rau buffers fullscreen
  (add-to-list 'display-buffer-alist '((derived-mode . rau-mode)	display-buffer-full-frame))
  (let ((appender
         (lgr-set-layout
          (lgr-appender-journald)
          (lgr-layout-format
           :format "%m"))))
    (let ((lgr (lgr-get-logger "rau")))
      (lgr-add-appender lgr appender)
      (lgr-set-threshold lgr lgr-level-warn)
      ))
  )

(print "rau enable" #'external-debugging-output)
(rau-enable) ; must be called after changes to rau-intercept-prefixes
(print "after rau enable" #'external-debugging-output)

(use-package xdg-launcher)

;; (defun thk-get-ednc-popup-frame-params ()
;;   `((parent-frame . ,(when-let* ((output-wl (rau--state-default-output-wl rau--state))
;;                                  (frame-wl (rau--output-wl-frame-wl output-wl))
;;                                  (role-data (rau--window-wl-role-data frame-wl)))
;;                        (message "parent frame for notification found.")
;;                        (rau--outputframe-emacs-frame role-data)))))

;; ednc-popup unfortunately does not work, see
;; https://codeberg.org/river/river/issues/1538

;; (use-package ednc
;;   :demand t
;;   :config
;;   (ednc-mode)
;;   )

;; (use-package ednc-popup
;;   :demand t
;;   :requires (ednc rau)
;;   :hook rau--ready-hook
;;   :load-path
;;   "/home/thk/git/emacs-ednc-popup"
;;   :config
;;   (add-hook 'ednc-notification-presentation-functions #'ednc-popup-presentation-function)
;;   :custom
;;   (ednc-popup-frame-parameters-override-fn #'thk-get-ednc-popup-frame-params)
;;   (ednc-popup-width 50)
;;   (ednc-popup-timeout 30)
;;   )
;; ;; (notifications-notify :title "testt" :body "from emacs")

;; https://superuser.com/questions/597076/gtk-3-menu-configuration-for-emacs
;; https://emacs.stackexchange.com/questions/83022/adding-a-custom-toolbar-item-scaling-the-emacs-toolbar-icons

; make it possible to use tool-bar and menu-bar, see https://codeberg.org/tazjin/rau/issues/18
;; (defun rau--window-parameters (frame window)
;;   (let*
;;       ((edges (window-inside-absolute-pixel-edges window))
;;        (toolbar-height 42)
;;        (menu-height 24)
;;        (offset (+ (if tool-bar-mode toolbar-height 0) (if menu-bar-mode menu-height 0)))
;;        )
;;     (rau-make-window-parameters
;;      (buffer-local-value 'rau-window (window-buffer window))
;;      frame
;;      (nth 0 edges)
;;      (+ (nth 1 edges) offset)
;;      (- (nth 2 edges) (nth 0 edges))
;;      (- (nth 3 edges) (nth 1 edges))))
