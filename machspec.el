;; All the "portable" settings
;; These are useful for only the macOS cocoa version
(load "~/CodeBase/emacs_dev/emacs-common.el")

;; Settings for machine specific customization

;; Set the initial frame to be of a specific shape
(set-frame-height (selected-frame) 0)
(set-frame-width (selected-frame) 60)
(set-frame-position (selected-frame) 0 0) ;; in pixels
(suspend-frame)

;; All new frames will have a good stat
(defun frame-size-and-position-fonc (new-frame)  
  "Setup the default parameters of a new frame"
  (select-frame new-frame)
  (message "Setting up frame position and size")
  (set-frame-height (selected-frame) 49)
  (set-frame-width (selected-frame) 116)
  (set-frame-position (selected-frame) 300 0) ;; in pixels
  )

(add-hook 'after-make-frame-functions 'frame-size-and-position-fonc)

;; Font
(set-face-attribute 'default nil
		    :family "Monaco" :height 130)
