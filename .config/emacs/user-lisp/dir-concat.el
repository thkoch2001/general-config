;;; dir-concat.el --- Concatenate directory files with extension-specific formatting -*- lexical-binding: t; -*-

;; Author: Qwen
;; Version: 1.0.0
;; Keywords: files, tools

;;; Commentary:
;; This package recursively scans a directory for files with configurable
;; extensions (default: .md and .rs) and concatenates them into a single buffer.
;; It applies specific formatting based on the file extension:
;; - Markdown (.md): Inserts a phrase after the first top-level heading.
;; - Rust (.rs): Inserts a root-relative file path as a comment at the top.

;;; Code:

(defcustom dir-concat-extensions '(".md" ".rs")
  "List of file extensions to include in the concatenation."
  :type '(repeat string)
  :group 'dir-concat)

(defcustom dir-concat-output-buffer-name "*Concatenated Files*"
  "Name of the buffer to store the concatenated output."
  :type 'string
  :group 'dir-concat)

(defun dir-concat--build-regexp (extensions)
  "Build a regexp matching any of the given EXTENSIONS at the end of a filename."
  (concat "\\(" (mapconcat #'regexp-quote extensions "\\|") "\\)\\'"))

(defun dir-concat--process-md (file root)
  "Process FILE as Markdown, adding the relative path after the first top-level heading."
  (let ((rel-path (file-relative-name file root)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward "^# .+" nil t)
          (progn
            (end-of-line)
            (insert (format "\n\n> The following is the content of file %s.\n" rel-path)))
        ;; Fallback: If no top-level heading exists, prepend one.
        (goto-char (point-min))
        (insert (format "# %s\n\n> The following is the content of file %s.\n\n"
                        (file-name-nondirectory file) rel-path)))
      (buffer-string))))

(defun dir-concat--process-rs (file root)
  "Process FILE as Rust, adding the relative path as a comment at the top."
  (let ((rel-path (file-relative-name file root)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (insert (format "// =========================================================================\n"))
      (insert (format "// File: %s\n" rel-path))
      (insert (format "// =========================================================================\n\n"))
      (buffer-string))))

(defun dir-concat--process-default (file root)
  "Fallback processor for any other configured extension. Adds a simple header."
  (let ((rel-path (file-relative-name file root)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (insert (format "\n\n;; --- File: %s ---\n\n" rel-path))
      (buffer-string))))

(defun dir-concat--not-dotfile-p (name)
  "Return non-nil if DIR does not start with a dot.
This is intended as a PREDICATE for `directory-files-recursively'
to skip hidden directories and prune the search tree early."
  (not (string-prefix-p "." name)))

;;;###autoload
(defun dir-concat-directory (root-dir)
  "Recursively scan ROOT-DIR for configured extensions and concatenate them.
The result is displayed in `dir-concat-output-buffer-name'."
  (interactive "DDirectory to scan: ")
  (let* ((canonical-root (expand-file-name root-dir))
         (regexp (dir-concat--build-regexp dir-concat-extensions))
         (files (directory-files-recursively canonical-root regexp nil #'dir-concat--not-dotfile-p))
         (output-buffer (get-buffer-create dir-concat-output-buffer-name)))

    (unless files
      (user-error "No files matching extensions %s found in %s"
                  dir-concat-extensions canonical-root))

    (with-current-buffer output-buffer
      (erase-buffer)

      ;; Sort files alphabetically for deterministic output
      (dolist (file (sort files #'string<))
        (let ((ext (file-name-extension file))
              (content ""))
          (cond
           ((string= ext "md")
            (setq content (dir-concat--process-md file canonical-root)))
           ((string= ext "rs")
            (setq content (dir-concat--process-rs file canonical-root)))
           (t
            (setq content (dir-concat--process-default file canonical-root))))

          (insert content)
          (insert "\n\n")))

      (goto-char (point-min))
      (display-buffer output-buffer))))

(provide 'dir-concat)
;;; dir-concat.el ends here
