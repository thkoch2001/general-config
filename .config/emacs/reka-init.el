(setenv "RUST_LOG" (getenv "REKA_LOGLEVEL"))
(require 'reka)
(reka-enable)

(run-at-time nil nil (lambda ()
    (start-process "start reka-emacs-started.target" nil "systemctl" "--user" "start" "reka-emacs-started.target")
    ))

(server-start)
