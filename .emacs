;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)


;; -------------------------------------------------
;; Mac os X specific configuration
;; -------------------------------------------------

(defun ms-macosxp () 
 "Checks if we are running under mac os x"
 (if  (string-match "apple-darwin" (version)) t))

(if (eq (ms-macosxp) t) 
(progn 
  (message "Emacs on mac os x detected")
   ;; make command act as meta
   (setq mac-command-modifier 'meta)
   ;; unbind option key so that it can be used for other purposes - polish chars input for example
   (setq mac-option-modifier 'none)
   ;; make Emacs work on mac. Dragging and dropping a file on a non running
   ;; emacs should cause only this file to open (no additional frames)
   ;; d&d a file on a running emacs should open this file instead of appending
   ;; its contents to an existing buffer. 
   (define-key global-map [ns-drag-file] 'my-ns-open-files)
   (defun my-ns-open-files ()
     "Open files in the list `ns-input-file'."
     (interactive)
     (mapc 'find-file ns-input-file)
     (setq ns-input-file nil))
   ;; so that input file events do not cause new frame to be opened
   (setq ns-pop-up-frames nil)))


;; -------------------------------------------------
;; Packages configuration
;; -------------------------------------------------

;; Configure packages for Emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Use use-package macro to install packages easily
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)


;; -------------------------------------------------
;; Emacs general themes configuration
;; -------------------------------------------------

;; Another theme, using a more modern approach.
;; But I could not find a theme with a pure black background
(use-package doom-themes
  :init (load-theme 'doom-acario-dark t))


;; Installs the color theme package
;; Alternate method of apllying theme instead of doom-theme
;; (add-to-list 'load-path "~/.site-lisp/color-theme-6.6.0")
;; (add-to-list 'load-path "~/.site-lisp/color-theme-6.6.0/themes")
;; (require 'color-theme)
;;    (color-theme-initialize)
;;    (color-theme-midnight)
;; ;; set the color for highlighted region to some dark grey
;; ;; otherwise it is set to ns_selection_color by default which does not work well with 
;; ;; this color theme. 
;; (set-face-background 'region "#303030")
;; ;; and set its color so that it's easily visible and the text shows through
;; (set-face-background 'cursor "#008800")

;; Package for nice icons on the mode line and in dired
(use-package all-the-icons
  :if (display-graphic-p))

;; A nice modeline
;;(use-package doom-modeline
;;  :init (doom-modeline-mode 1)
;;  :custom ((doom-modeline-height 15)))


;; set initial frame(window) size
;;(set-frame-height (selected-frame) 100)
;;(set-frame-width (selected-frame) 160)

;; Cause the region to be highlighted and prevent region-based commands
;; from running when the mark isn't active.
(pending-delete-mode t)
 (setq transient-mark-mode t)

;; Cause emacs to ask every time it quits. 
(setq kill-emacs-query-functions
  (list (function (lambda ()
                    (ding)
                    (y-or-n-p "Really quit? ")))))

;;Fonts are automatically highlighted.  For more information
;;type M-x describe-mode font-lock-mode 
(global-font-lock-mode t)

;; standard one was Monaco 12
;; default one can be displayed by running M-x describe-font
(if (eq (ms-macosxp) t) 
    (set-frame-font "Monaco 10"))

;; to see matching parens
(show-paren-mode t)

;; remove the menu and button bars to save screen space
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Do not show the starup screen and message
(setq inhibit-startup-screen t)

;; set tab distance to something, so it doesn't change randomly and confuse people
(setq c-basic-offset 4)

;; tab 4
(setq default-tab-width 4)

;; tab for nxml mode
(setq nxml-child-indent 4)

;; (for display line number at mode-line)
(line-number-mode t)
;; (for display point in line at mode-line)
(column-number-mode t)

;; blink cursor because in Emacs 24 on mac os x text under the cursor does not show well
(blink-cursor-mode 1)

;; Line numbers mode enabled by default (newer version, not linum mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;(global-display-line-numbers-mode t)

;; use cua mode only for rectangles
(setq cua-enable-cua-keys nil) 
(cua-mode t)

;; (for display date and time)
(setq display-time-day-and-date t
      display-time-24hr-format t)
(setq display-time-format "%m/%d(%a) %H:%M")
(display-time)

;; to show full file path - usefull when comparing 2 files of the same name
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; set the calendar to start weeks on monday
(setq calendar-week-start-day 1)

;; have smooth scrolling
(add-to-list 'load-path "~/.site-lisp/smooth-scrolling")
(require 'smooth-scrolling)
(setq smooth-scroll-margin 2)

;; be able to invoke M-x with other shortcut. C-c C-m if I miss C-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; set the spellchecking program to aspell
(setq ispell-program-name "aspell")

;; enable flyspell-mode by default for text mode
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

;; Fix mouse for flyspell to pick up 2 finger clicks
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

; return a backup file path of a give file path
; with full directory mirroring from a root dir
; non-existant dir will be created
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
   If the new path's directories does not exist, create them."
  (let (backup-root bpath)
    (setq backup-root "~/.emacs.d/emacs-backup")
    (setq bpath (concat backup-root fpath "~"))
    (make-directory (file-name-directory bpath) bpath)
    bpath
  )
)
;; for all those backup~ files
(setq make-backup-file-name-function 'my-backup-file-name)

;; Start emacs as a server so that it can be used by various clients as an editor
(server-start)

;; emacs yaml mode
(if (eq (ms-macosxp) t)
    (progn
      (add-to-list 'load-path "~/.site-lisp/yaml")
      (require 'yaml-mode)
      (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
      (add-hook 'yaml-mode-hook
		'(lambda ()
		   (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
      ;; add indent highlights to yaml
      (add-to-list 'load-path "~/.site-lisp/highlight-indent-guides")
      (require 'highlight-indent-guides)
      (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)))


;; tweak registers behaviour to have a way to quickly store point
;; position Allows to move quickly from some point in a file to some
;; other location and then go back I do not need multiple point
;; locations store because it's confusing and I would not remember
;; them anyway
(defun ms-store-point-location-in-register ()
  "store current point position in register P for quick reference"
  (interactive)
  (point-to-register ?P))

(defun ms-jump-point-to-location-in-register ()
  "jump to point position stored in register P"
  (interactive)
  (jump-to-register ?P))

(global-set-key (kbd "C-x r SPC") 'ms-store-point-location-in-register)
(global-set-key (kbd "C-x r j") 'ms-jump-point-to-location-in-register)

;; enable winner mode so that we can undo window changes easily
(winner-mode 1)

; Sets Ctrl-c Ctrl-c key combination for easy commenting
; out of selected lines.
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

(defun ms-my-python-mode-hook ()
  "My hook for `Python mode'."
  (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
  )

(add-hook 'python-mode-hook 'ms-my-python-mode-hook)

 (defun my-c-mode-common-hook ()
  "My hook for C languages family mode"
  (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Settings for Octave Mode
(setq inferior-octave-program "/usr/local/bin/octave")

;; Setting of markdown command, installed separatey through Brew
(setq markdown-command "/usr/local/bin/markdown")

;; Settings for Treemacs
(use-package treemacs
  :ensure t)

;; Settings for Swift (taken from wwift.org emacs documentation page

;;; Locate sourcekit-lsp
(defun ms-find-sourcekit-lsp ()
  (or (executable-find "sourcekit-lsp")
      (and (eq system-type 'darwin)
           (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp")))
      "/usr/local/swift/usr/bin/sourcekit-lsp"))


;; Taken from here: https://danielde.dev/blog/emacs-for-swift-development
;; adds a print statment for the variable under point
(defun ms-print-swift-var-under-point()
  (interactive)
  (if (string-match-p (string (preceding-char)) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
      (backward-sexp)
    nil)
  (kill-sexp)
  (yank)
  (move-end-of-line nil)
  (newline)
  (insert "print(\"")
  (yank)
  (insert ": \\(")
  (yank)
  (insert ")\")")
  (indent-for-tab-command))



;; using flycheck instead of default flymake for showing errors
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; .editorconfig file support
(use-package editorconfig
    :ensure t
    :config (editorconfig-mode +1))

;; Swift editing support
(use-package swift-mode
    :ensure t
    :mode "\\.swift\\'"
    :interpreter "swift"
    :bind (("C-c l" . ms-print-swift-var-under-point)))

;; Rainbow delimiters makes nested delimiters easier to understand
(use-package rainbow-delimiters
    :ensure t
    :hook ((prog-mode . rainbow-delimiters-mode)))

;; Company mode (completion)
(use-package company
    :ensure t
    :config
    (global-company-mode +1))

;; Used to interface with swift-lsp.
(use-package lsp-mode
    :ensure t
    :commands lsp
    :hook ((swift-mode . lsp)))

;; lsp-mode's UI modules
(use-package lsp-ui
    :ensure t)

;; sourcekit-lsp support
(use-package lsp-sourcekit
    :ensure t
    :after lsp-mode
    :custom
    (lsp-sourcekit-executable (ms-find-sourcekit-lsp) "Find sourcekit-lsp"))


;; Disable LSP for Swift
(add-hook 'swift-mode-hook (lambda () (lsp-mode -1)))

;; Enable Flycheck
(add-hook 'swift-mode-hook #'flycheck-mode)

;; Swift settings end


;; End of file.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treemacs doom-quit doom-modeline all-the-icons doom-themes use-package markdown-mode ## swift-mode terraform-mode hcl-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Some unused code

;; (defun ms-emacs26p () 
;;  "Checks if we are calling emacs version 26"
;;   (if (string-match "Emacs 26" (version)) t))


