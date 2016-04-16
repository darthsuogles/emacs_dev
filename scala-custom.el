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

(use-package scala-mode2
  :mode ("\\.sclpt\\'" . scala-mode)
  :bind (:map scala-mode-map
              ("C-c C-r" . scala-eval-region)
              ("C-c C-c" . scala-eval-definition)
              ("C-c C-z" . scala-spark-shell)
              ("M-_"     . scala-insert-left-arrow)
              ("M-+"     . scala-insert-right-arrow))
  )

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; >>>>>>> 78a6cb7cce819b9afc36f913e2b2ec45c0e3b12e

(add-to-list 'load-path (concat elisp_path "/emacs-sbt-mode"))
;; Scal SBT mode custom keys
(use-package sbt-mode
  :commands sbt-start sbt-command  
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  :bind (("C-c C-b C-r" . sbt-paste-region)
         ("C-c C-b C-c" . sbt-start))
  )

(defun sbt:paste-region (start end &optional no-exit)
  "Send region (from START to END) using :paste REPL command.

Using the EOF mode instead of C-D to avoid troubles

If NO-EXIT is non-zero, this function will not end the paste
mode."
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
