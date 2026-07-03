(defun ms-xcode-build-and-run ()
  "Calls build and run command from xcode"
  (interactive) 
  (do-applescript 
   (concat "tell application \"Xcode\" \r"
		   "    activate \r"
		   ;;  "tell application \"System Events\" \r"
		   ;; "	 tell process \"Xcode\" \r"
		   ;; "	 click menu item \"Stop\" of menu 1 of menu bar item \"Run\" of menu bar 1 \r"
;;		    "	 click menu item \"Build and Run\" of menu 1 of menu bar item \"Build\" of menu bar 1 \r"
		   	;; "	 end tell \r"
		    ;; "	 end tell \r"
		   "end tell \r")))


(global-set-key (kbd "M-r") 'ms-xcode-build-and-run)

(provide 'build-and-run)

