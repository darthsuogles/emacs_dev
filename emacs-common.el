;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variable definition

;; additional .el load path
(setq elisp_path "~/CodeBase/emacs_dev")
(add-to-list 'load-path elisp_path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs server
(server-start)

;;--------------------------------------------------------------------
;; Also add the package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("marmalade" . "https://marmalade-repo.org/packages/")
;;                          ("melpa" . "https://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------------
;; ANSI color

;; comint install
;; Also remember to set TERM accordingly (xterm-256color)
(add-to-list 'load-path (concat elisp_path "/xterm-color"))
(require 'xterm-color)
(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
       (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

;; ;; comint uninstall
;; (progn (remove-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
;;        (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;;        (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region))


;; You can also use it with eshell (and thus get color output from system ls):
(require 'eshell)
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))
(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------------
;; IPython
;;
;; Warning: do not use the master branch python.el
;;          do not use el-get to install python
;; Please byte-compile the emacs-24 branch
;;(load (concat elisp_path "/python.el"))
(require 'python)
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--pylab"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;--------------------------------------------------------------------
;; Scala
(require 'scala-mode2)
(defun scala-spark-shell ()
  (interactive)
  (scala-run-scala "~/spark/spark-shell.sh"))
(defun scala-mode2-keymap ()
  "Modify mode for scala sbt"
  (local-set-key (kbd "C-c C-r") 'scala-eval-region)
  (local-set-key (kbd "C-c C-c") 'scala-eval-definition)
  (local-set-key (kbd "C-c C-z") 'scala-spark-shell)
  ;; more here
  )
(add-hook 'scala-mode-hook 'scala-mode2-keymap)

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-to-list 'load-path (concat elisp_path "/emacs-sbt-mode"))
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

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

(defun scala-sbt-mode-keymap ()
  "Modify mode for scala sbt"
  (local-set-key (kbd "C-c C-b C-r") 'sbt-paste-region)
  (local-set-key (kbd "C-c C-b C-c") 'sbt-start)
  ;; more here
  )
;; add to hook
;;(add-hook 'scala-mode-hook 'scala-sbt-mode-keymap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode (site-lisp is the install location)
(load "orgmode-custom.el")

;;--------------------------------------------------------------------
;; R (ESS mode) now in melpa
;; (setq ess-lispdir (concat elisp_path "/ESS/lisp"))
;; (add-to-list 'load-path ess-lispdir)
;; (load (concat ess-lispdir "/ess-site.el"))

;; web-mode @ http://web-mode.org
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defun web-mode-custom-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  )
(add-hook 'web-mode-hook  'web-mode-custom-hook)
(setq-default indent-tabs-mode t)

;; Coq
(load (concat elisp_path "/prfgnrl/generic/proof-site.el"))
;;(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;; (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell for spell checking
;; Ref: http://www.emacswiki.org/emacs/FlySpell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t) 

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;;(require 'reftex)
;; ;; spell checking for the comments
;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (flyspell-prog-mode)
;; 	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual stuffs, should be put here
;; in case they are modified by other modes

;; Get switching buffer helper mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)

;; global settings
;;(tool-bar-mode nil) ;; turning off the tool-bar
(transient-mark-mode t) 

;; All about the meta keys
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)
;;(setq setq mac-command-key-is-meta t)
;;(setq mac-command-key 'meta)

;; Substitude certain patterns (Greek letters or math symbols)
;; with unicode
(load "emacs-rc-pretty-lambda.el")

;; New in Emacs 24, color theme, loaded at last
(load-theme 'solarized-dark "NO-CONFIRM")

(setq ring-bell-function 'ignore)
(setq visible-bell nil)
(setq global-visual-line-mode t)
(setq word-wrap t) ;; line wrapping

(setq ns-pop-up-frames nil) ;; open new frames inside existing ones
