;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)


;; -------------------------------------------------
;; Mac os X specific configuration
;; -------------------------------------------------

(defun ms/macosxp () 
 "Checks if we are running under mac os x"
 (if  (string-match "apple-darwin" (version)) t))

(if (eq (ms/macosxp) t) 
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
   (define-key global-map [ns-drag-file] 'ms/ns-open-files)
   (defun ms/ns-open-files ()
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

;; So that random cache files are not put in various places
;; for example for svg-lib cached icons
(use-package no-littering)

;; -------------------------------------------------
;; Emacs general themes configuration
;; -------------------------------------------------

;; Another theme, using a more modern approach.
;; But I could not find a theme with a pure black background
(use-package doom-themes
  :init (load-theme 'doom-acario-dark t))


;; Package for nice icons on the mode line and in dired
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; set initial frame(window) size
(set-frame-height (selected-frame) 70)
(set-frame-width (selected-frame) 160)

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
(if (eq (ms/macosxp) t) 
    (set-frame-font "Monaco 11"))

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

;; have smooth scrolling, that's some old solution, I've changed it to ultra scroll.
;; both seem to be working similarly though
;; (add-to-list 'load-path "~/.site-lisp/smooth-scrolling")
;; (require 'smooth-scrolling)
;; (setq smooth-scroll-margin 2)

(use-package ultra-scroll
  ;:vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))


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

;; return a backup file path of a give file path
;; with full directory mirroring from a root dir
;; non-existant dir will be created
(defun ms/backup-file-name (fpath)
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
(setq make-backup-file-name-function 'ms/backup-file-name)

;; Start emacs as a server so that it can be used by various clients as an editor
(server-start)

;; emacs yaml mode
(if (eq (ms/macosxp) t)
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
(defun ms/store-point-location-in-register ()
  "store current point position in register P for quick reference"
  (interactive)
  (point-to-register ?P))

(defun ms/jump-point-to-location-in-register ()
  "jump to point position stored in register P"
  (interactive)
  (jump-to-register ?P))

(global-set-key (kbd "C-x r SPC") 'ms/store-point-location-in-register)
(global-set-key (kbd "C-x r j") 'ms/jump-point-to-location-in-register)

;; enable winner mode so that we can undo window changes easily
(winner-mode 1)

; Sets Ctrl-c Ctrl-c key combination for easy commenting
; out of selected lines.
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

(defun ms/python-mode-hook ()
  "My hook for `Python mode'."
  (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
  )

(add-hook 'python-mode-hook 'ms/python-mode-hook)

 (defun ms/mode-common-hook ()
  "My hook for C languages family mode"
  (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
  )

(add-hook 'c-mode-common-hook 'ms/c-mode-common-hook)

;; Settings for Octave Mode
(setq inferior-octave-program "/usr/local/bin/octave")

;; Setting of markdown command, installed separatey through Brew
(setq markdown-command "/usr/local/bin/markdown")

;; Settings for Treemacs
(use-package treemacs
  :ensure t)

;; Settings for ido mode
(require 'ido)
(ido-mode t)


;; New Swift Mode settings by Gemini
;; Using eglot and flymake instead of lsp-mode and flycheck

;; Alternative settings for Swift can be taken from wwift.org emacs documentation page
;; but are not used here

;; 1. Setup Tree-sitter grammar for Swift
;; This tells Emacs where to download the grammar if it's not installed.
(setq treesit-language-source-alist
      '((swift "/Users/skoleckm/.emacs.d/tree-sitter-swift")))

;; Helper to install the grammar if missing
(unless (treesit-language-available-p 'swift)
  (treesit-install-language-grammar 'swift))

;; Some custom Function to be able to easily
;; insert a print statement for some variable
(defun ms/print-swift-var-under-point ()
  "Inserts a print statement for the variable under point at the next line.
     Example: varName -> print(\"varName: \(varName)\")"
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (if symbol
        (progn
          (move-end-of-line nil)
          (newline-and-indent)
          (insert (format "print(\"%s: \\(%s)\")" symbol symbol)))
      (message "No symbol found under point"))))


;; 2. Install and Configure swift-ts-mode
;; This is the modern, tree-sitter aware replacement for standard swift-mode.
;; Available on MELPA.
(use-package swift-ts-mode
  :ensure t
  :mode "\\.swift\\'"
  :hook (swift-ts-mode . eglot-ensure) ;; Automatically start LSP
  :bind (:map swift-ts-mode-map
	      ("C-c p" . ms/print-swift-var-under-point))
  :config
  ;; Configure Eglot to find sourcekit-lsp if it's not in your PATH
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '(swift-ts-mode . ("xcrun" "sourcekit-lsp")))))


;; 4. Optional: Eglot UI tweaks for a cleaner experience
(use-package eglot
  :ensure nil ;; Built-in
  :bind (:map eglot-mode-map
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename)
	      ("C-c h" . eldoc))
  :custom
  (eglot-events-buffer-size 0) ;; Disable logging for better performance
  (eglot-shutdown t) ;; Shutdown servier when last buffer is closed
  :config
  (setq flymake-show-diagnostics-at-end-of-line t))

;; 5. Keybinding
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") #'flymake-show-buffer-diagnostics))

(with-eval-after-load 'swfit-ts-mode
  (define-key swift-ts-mode-map (kbd "C-c p") 'ms/print-swift-var-under-point))

;; 6. Suggested by ChatGPT to have similiar experience as lsp-mode
;;   and nice code completion
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode) ;; adds documentation popup
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  ;; Popupinfo behavior
  (corfu-popupinfo-delay 0.3)
  (corfu-popupinfo-max-height 40)
  (corfu-popupinfo-min-width 30)
  (corfu-popupinfo-max-width 70))

;; to set the highlight in Corfu code completion popup
(custom-set-faces
 '(corfu-current
   ((t
     :background "#002366" ; dark blue
     :foreground "#ffffff" ; white text
     :weight bold))))

;; ;; 7. Suggested by ChatGPT to have some better docs
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p t))

;; So that eldoc help buffer and minibuffer have better contrast
(custom-set-faces
 '(markdown-code-face
   ((t :background "#202020"))))


;; nice icons in corfu popup for designating function, field, member etc
;; config taken from here:
;; https://kristofferbalintona.me/posts/202202270056/#what-is-corfu-how-does-it-differ-from-company
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)) ; Enable `kind-icon'


(setq flymake-show-diagnostics-at-end-of-line t)
(global-set-key (kbd "C-c ! l") #'flymake-show-buffer-diagnostics)


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


