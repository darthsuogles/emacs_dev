
;; All the "portable" settings
(load "~/CodeBase/emacs_dev/emacs-common.el")

(load-theme 'tango-dark)

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

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-agenda-files (quote ("~/CodeBase/neuroimg/neuroimg.org")))
;;  '(send-mail-function (quote mailclient-send-it))
;;  '(tabbar-separator (quote (0.5)))
;;  '(tool-bar-mode nil))
