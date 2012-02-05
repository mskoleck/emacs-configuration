;; Copyright (C) 2009  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;(require 'ac-mode)
;(add-hook 'find-file-hooks 'ac-mode-without-exception)

;; @see http://github.com/m2ym/auto-complete
;; @see http://www.emacswiki.org/emacs/AutoComplete
(require 'auto-complete)
(require 'auto-complete-config)

;; @see http://nschum.de/src/emacs/company-mode/
;; @see http://github.com/buzztaiki/auto-complete/blob/master/ac-company.el
(require 'ac-company)
(ac-company-define-source ac-source-company-xcode company-xcode)

(setq ac-modes (append ac-modes '(rst-mode)))
(setq ac-modes (append ac-modes '(objc-mode)))

(add-hook 'objc-mode-hook
          (lambda ()
            (define-key objc-mode-map "TAB" 'ac-complete)
            (push 'ac-source-company-xcode ac-sources)
            (push 'ac-source-c++-keywords ac-sources)
          ))


(global-auto-complete-mode t)


(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "M-/") 'ac-stop)


(setq ac-auto-start nil)

;(setq ac-auto-start 2)

(ac-set-trigger-key "TAB")

(setq ac-candidate-max 20)

(provide 'init_ac)
;;; init_ac.el ends here
