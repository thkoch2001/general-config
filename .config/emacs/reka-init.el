;;; -*- lexical-binding: t; -*-

(let* (
       (repo "/home/thk/git/reka/")
       (rust (concat repo "target/debug/libreka.so"))
       (lisp (concat repo "lisp/reka.el"))
      )
  (if (and (getenv "REKADEV") (file-exists-p rust))
    (progn
      (message "loading reka from git")
      (require 'libreka rust)
      (require 'reka lisp)
      )
  (progn
    (message "loading reka from Emacs load path")
    (require 'reka)
    )
  ))

(run-at-time nil nil (lambda ()
    (start-process "start reka-emacs-started.target" nil "systemctl" "--user" "start" "reka-emacs-started.target")
    ))

(customize-set-variable 'tab-bar-select-tab-modifiers '(super) "set by reka-init.el")
(customize-set-variable 'reka-intercept-prefixes '(
   "s-x" "s-u" "s-h" "s-X" "s-<tab>" "s-b" "s-d"
   "s-0" "s-1" "s-2" "s-3" "s-4" "s-5" "s-6" "s-7" "s-8" "s-9"
   )
 )

; (reka-push-intercept-prefixes)
(defun thk-reka-set-key (key cmd &rest args)
  "Bind KEY to run CMD with ARGS and add let reka forward KEY to emacs."
  (keymap-global-set key `(lambda () (interactive) ,(append (list 'call-process cmd nil nil nil) args)))
  (add-to-list 'reka-intercept-prefixes key)
  )

(thk-reka-set-key "s-c" "copyq" "show")
(thk-reka-set-key "s-l" "sh" "-c" "swaylock && sleep 3 && systemctl --user --signal SIGUSR1 kill swayidle.service")
(thk-reka-set-key "s-p" "systemd-run" "--user" "--quiet" "--property=ExitType=cgroup" "fuzzel" "--log-level=info")
(thk-reka-set-key "s-w" "systemctl" "--user" "kill" "--signal=SIGUSR1" "waybar.service")
(thk-reka-set-key "s-<return>" "systemd-run" "--user" "--quiet" "alacritty")

(keymap-global-set "s-x" ctl-x-map)
(keymap-global-set "s-h" help-map)
(keymap-global-set "s-<tab>" 'next-multiframe-window)
(keymap-global-set "S-s-<iso-lefttab>" 'previous-window-any-frame)
(keymap-global-set "s-u" 'universal-argument)
(keymap-global-set "s-X" 'execute-extended-command)
(keymap-global-set "s-b" (lambda () (interactive) (switch-to-buffer "*scratch*") (delete-other-windows) (thk-transparency)))
(keymap-global-set "s-d" (lambda () (interactive) (message nil)))

(reka-enable) ; must be called after changes to reka-intercept-prefixes

(reka-push-intercept-prefix "s-f" 'toggle-fullscreen)
(server-start)

(use-package ednc
  :init
  (ednc-mode)
  :config
  (defun ednc-message-presentation-function (old-notif notif)
    (when notif
      (let (
            (title (ednc-notification-app-name notif))
            (summary (ednc-notification-summary notif))
            (body (ednc-notification-body notif))
            )
        (message "%s: %s\n%s" title summary body)
        )
      )
    )
  (add-hook 'ednc-notification-presentation-functions
            #'ednc-message-presentation-function)
    (defun ednc-tooltip-presentation-function (old-notif notif)
    (when notif
      (let (
            (title (ednc-notification-app-name notif))
            (summary (ednc-notification-summary notif))
            (body (ednc-notification-body notif))
            )
        (x-show-tip (format "%s: %s\n%s" title summary body) nil '((right . 100) (top . 10)) 15)
        )
      )
    )
  (add-hook 'ednc-notification-presentation-functions
            #'ednc-tooltip-presentation-function)
  )

;(require 'ednc-popup "/home/thk/git/emacs-ednc-popup/ednc-popup.el")
;(add-hook 'ednc-notification-presentation-functions
;          #'ednc-popup-presentation-function)

; open reka buffers fullscreen
(add-to-list 'display-buffer-alist '((derived-mode . reka-mode)	display-buffer-full-frame))

; workaround for missing reka-mode-hook, https://codeberg.org/tazjin/reka/issues/16
(defun thk-init-reka-mode ()
  (when (eq major-mode 'reka-mode)
    (setq cursor-type nil)
    ))

(add-hook 'after-change-major-mode-hook #'thk-init-reka-mode)

