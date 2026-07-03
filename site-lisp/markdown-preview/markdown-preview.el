
(defgroup ms/markdown-preview nil "" :group 'tools)
(defcustom ms/markdown-preview-root
  (expand-file-name "~/.emacs.d/site-lisp/markdown-preview/")
  "")
(defcustom ms/markdown-preview-output
  (expand-file-name "preview.html" temporary-file-directory)
  "")

(defun ms/markdown-preview--converter ()
  (cond ((executable-find "pandoc") "pandoc -f gfm -t html --id-prefix=''") ;; 'gfm' enables task lists and headings IDs!
        ((executable-find "markdown") "markdown -T")
        (t (error "Install pandoc or markdown"))))

(defun ms/markdown-preview ()
  (interactive)
  (let* (;; 1. Capture the markdown text from the current buffer FIRST
         (markdown-text (buffer-string))
         ;; 2. Feed that text into the shell command inside the temp buffer
         (html (with-temp-buffer
                 (insert markdown-text)
                 (shell-command-on-region
                  (point-min) (point-max)
                  (ms/markdown-preview--converter)
                  (current-buffer) t) ;; 't' replaces the temp buffer content with stdout
                 (buffer-string)))
         (tpl (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "preview-template.html"
                                   ms/markdown-preview-root))
                (buffer-string))))
    (with-temp-file ms/markdown-preview-output
      (insert (replace-regexp-in-string
               "{{CONTENT}}" html tpl t t)))
    (browse-url (concat "file://" ms/markdown-preview-output))))


(defun ms/markdown-preview ()
  (interactive)
  (let* (;; 1. Capture the markdown text from the current buffer FIRST
         (markdown-text (buffer-string))
         ;; 2. Dynamically build the file URI for your root assets directory
         (root-uri (concat "file://" (file-name-as-directory ms/markdown-preview-root)))
         ;; 3. Feed that text into the shell command inside the temp buffer
         (html (with-temp-buffer
                 (insert markdown-text)
                 (shell-command-on-region
                  (point-min) (point-max)
                  (ms/markdown-preview--converter)
                  (current-buffer) t) ;; 't' replaces the temp buffer content with stdout
                 (buffer-string)))
         (tpl (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "preview-template.html"
                                   ms/markdown-preview-root))
                (buffer-string))))
    (with-temp-file ms/markdown-preview-output
      (insert (replace-regexp-in-string
               "{{CONTENT}}" html 
               (replace-regexp-in-string "{{ROOT}}" root-uri tpl t t) 
               t t)))
    (browse-url (concat "file://" ms/markdown-preview-output))))


(defun ms/markdown-preview-refresh ()
  (when (derived-mode-p 'markdown-mode)
    (ignore-errors (ms/markdown-preview))))

(define-minor-mode ms/markdown-preview-mode
  "Auto refresh preview on save."
  :lighter " MdPrev"
  (if ms/markdown-preview-mode
      (add-hook 'after-save-hook #'ms/markdown-preview-refresh nil t)
    (remove-hook 'after-save-hook #'ms/markdown-preview-refresh t)))

(provide 'markdown-preview)
