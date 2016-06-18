;; additional .el load path
(setq elisp_path "~/CodeBase/emacs_dev")

;; <<<<<<< HEAD
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; (add-to-list 'auto-mode-alist '("\\.sclpt\\'" . scala-mode))
;; =======
(defun scala-insert-left-arrow () (interactive) (insert "<-"))
(defun scala-insert-typesafe-arrow () (interactive) (insert "~>"))

(defun scala-spark-shell () (interactive) (scala-run-scala "./spark-shell.sh"))

;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-initialize)
;; (unless (package-installed-p 'scala-mode2)
;;   (package-refresh-contents) (package-install 'scala-mode2))
(add-to-list 'load-path (concat elisp_path "/scala-mode2"))
(require 'scala-mode2)
(add-to-list 'auto-mode-alist '("\\.sclpt\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(defun scala-mode2-keybindings ()
  (local-set-key (kbd "C-c C-r") 'scala-eval-region)
  (local-set-key (kbd "C-c C-c") 'scala-eval-definition)
  (local-set-key (kbd "C-c C-z") 'scala-spark-shell)
  (local-set-key (kbd "M-_") 'scala-insert-left-arrow)
  (local-set-key (kbd "M-+") 'scala-insert-typesafe-arrow)
  )
(add-hook 'scala-mode2-hook 'scala-mode2-keybindings)
 
;; (use-package scala-mode2
;;   :mode ("\\.sclpt\\'" . scala-mode)
;;   :bind (:map scala-mode-map
;;               ("C-c C-r" . scala-eval-region)
;;               ("C-c C-c" . scala-eval-definition)
;;               ("C-c C-z" . scala-spark-shell)
;;               ("M-_"     . scala-insert-left-arrow)
;;               ("M-+"     . scala-insert-typesafe-arrow))
;;   )

;; For now, don't bother to use ensime
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-to-list 'load-path (concat elisp_path "/emacs-sbt-mode"))
;; Scal SBT mode custom keys
(use-package sbt-mode
  :commands sbt-start sbt-command  
  :init
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-name "./scala_exec.sh -b")
  :bind (("C-c C-b C-c" . sbt-start)
         :map scala-mode-map
         ("C-c C-r" . sbt-send-region)
         ("C-c C-p" . sbt-paste-region)
         ("C-c C-z" . sbt-start))
  )

;; (defun sbt:paste-region (start end &optional no-exit)
;;   "Send region (from START to END) using :paste REPL command.

;; Using the EOF mode instead of C-D to avoid troubles

;; If NO-EXIT is non-zero, this function will not end the paste
;; mode."
;;   (unless (comint-check-proc (sbt:buffer-name))
;;     (error "sbt is not running in buffer %s" (sbt:buffer-name)))
;;   (save-excursion
;;     (goto-char end)
;;     (skip-syntax-forward ">")
;;     (forward-comment (- (point-max)))
;;     (setq end (point)))
;;   (save-excursion
;;     (goto-char start)
;;     (forward-comment (point-max))
;;     (setq start (point)))
;;   (unless (> end start) (error "mark a region of code first"))
;;   (display-buffer (sbt:buffer-name))
;;   (let ((submode (buffer-local-value 'sbt:submode
;;                                      (get-buffer (sbt:buffer-name)))))    
;;     (while (eq submode 'sbt)
;;       (sbt-command "console") (error "must wait till we are in console mode"))
;;     (message "submode: %s" submode)
;;     (unless (eq submode 'paste-mode)
;;       (comint-send-string (sbt:buffer-name) ":paste\n"))
;;     (comint-send-region (sbt:buffer-name) start end)
;;     (message "sbt code segment sent")
;;     (comint-send-string (sbt:buffer-name) "\n\^D")
;;     (message "sbt console paste termination signal sent")) 
;;   )

;; (defvar ammonite-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
;;   "Prompt for `run-scala-ammonite'.")

(defun sbt:paste-region-ammonite (start end)
  "Send region (from START to END) using :paste REPL command."
  (unless (comint-check-proc (sbt:buffer-name))
    (error "sbt is not running in buffer %s" (sbt:buffer-name)))  
  (save-excursion
    (goto-char end)
    (skip-syntax-forward ">")
    (forward-comment (- (point-max)))
    (setq end (point)))
  (save-excursion
    (goto-char start)
    (forward-comment (point-max))
    (setq start (point)))
  (unless (> end start) (error "mark a region of code first"))
  (display-buffer (sbt:buffer-name))
  (let ((submode (buffer-local-value 'sbt:submode
                                     (get-buffer (sbt:buffer-name)))))    
    (while (eq submode 'sbt)
      (sbt-command "console") (error "must wait till we are in console mode"))
    (message "submode: %s" submode)
    (unless (eq submode 'paste-mode)
      (comint-send-string (sbt:buffer-name) ":paste\n"))
    (comint-send-region (sbt:buffer-name) start end)
    (message "sbt code segment sent")
    (comint-send-string (sbt:buffer-name) "\n\^D")
    (message "sbt console paste termination signal sent")) 
  )
