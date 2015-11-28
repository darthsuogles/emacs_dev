;; Settings for machine specific customization

;; Set the initial frame to be of a specific shape
(set-frame-height (selected-frame) 0)
(set-frame-width (selected-frame) 60)
(set-frame-position (selected-frame) 0 0)
(suspend-frame)

;; All new frames will have a good stat
(defun frame-size-and-position-fonc (new-frame)  
  "Setup the default parameters of a new frame"
  (select-frame new-frame)
  (message "Setting up frame position and size")
  (set-frame-height (selected-frame) 72)
  (set-frame-width (selected-frame) 113)
  (set-frame-position (selected-frame) 500 0) ;; in pixels

  ;; (message "Setting up font")
  ;; (set-face-attribute 'default nil :family "Consolas")
  ;; (set-face-attribute 'default nil :height 160)
  )

(add-hook 'after-make-frame-functions 'frame-size-and-position-fonc)
