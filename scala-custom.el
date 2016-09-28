;; additional .el load path
(setq elisp_path "~/CodeBase/emacs_dev")

;; (use-package ensime
;;   :pin melpa-stable)
;; ;; =======
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
(add-hook 'scala-mode-hook 'scala-mode2-keybindings)

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
  :bind (("C-c C-b C-c" . sbt-start)
         :map scala-mode-map
         ("C-c C-r" . sbt-send-region)
         ("C-c C-p" . sbt-paste-region)
         ("C-c C-a" . sbt-paste-region-ammonite)
         ("C-c C-z" . sbt-start))
  )

(defun sbt-paste-region-ammonite (&optional no-exit)
  "Send the selected region (between the mark and the current
point) to the sbt process of the current buffer's sbt project
using :paste REPL command.  Whitespace and comments at the
beginning or end of the region are not sent.  If the optional
NO-EXIT is non-zero, it will not exit the :paste session, so that
subsequent call to this function may provide additional input."
  (interactive "P")
  ;; TODO: Currently, NO-EXIT does not work correctly.
  ;; (sbt:paste-region (region-beginning) (region-end) arg)
  (sbt-paste-region-ammonite-impl (region-beginning) (region-end)))

(defun sbt-paste-region-ammonite-impl (start end)
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
    (comint-send-string (sbt:buffer-name) "{\n")
    (comint-send-region (sbt:buffer-name) start end)
    (comint-send-string (sbt:buffer-name) "\n}\n")
    (message "sbt console paste termination signal sent"))
  )
