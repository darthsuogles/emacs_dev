;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variable definition

;; additional .el load path
(setq elisp_path "~/CodeBase/emacs_dev")
(add-to-list 'load-path elisp_path)

;; Very basic key modifications for OSX
;; Duplicated at the end of the file for fear that some mode might modify them
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs server
(server-start)

;;--------------------------------------------------------------------
;; Also add the package
(require 'package)
(add-to-list 'load-path (concat elisp_path "/use-package"))
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'async))
(require 'use-package) ;; get the dependencies
(dired-async-mode 1) ;; 

(defvar package-depslist
  '(helm s company magit projectile dash async use-package evil zoom-frm
         web-mode ess lua-mode z3-mode
         ensime scala-mode2 sbt-mode
         solarized-theme xterm-color)
  "A list of dependencies to be installed")

(require 'cl) ;; use the common-lisp extension
(defun package-deps-installed-p ()
  "Return true if the dependencies are all installed"
  (loop for pkg in package-depslist
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (package-deps-installed-p)
  (package-refresh-contents)
  (dolist (pkg package-depslist)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'company-statistics-mode t)
)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-c C-m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
;;(key-chord-define-global "fm" 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-=") 'zoom-frm-in)
(global-set-key (kbd "C--") 'zoom-frm-out)
(global-set-key (kbd "C-\\") 'zoom-frm-unzoom)

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
;; (require 'python)
;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args "--pylab"
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;  "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;  "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :init
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
  )

;;--------------------------------------------------------------------
;; Scala
(load "scala-custom.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode (site-lisp is the install location)
(load "orgmode-custom.el")

;;--------------------------------------------------------------------
;; R (ESS mode) now in melpa
;; (setq ess-lispdir (concat elisp_path "/ESS/lisp"))
;; (add-to-list 'load-path ess-lispdir)
;; (load (concat ess-lispdir "/ess-site.el"))

;; web-mode @ http://web-mode.org
(use-package web-mode
  :mode ("\\.html?\\'" 
         "\\.php\\'")
  :init (setq
         web-mode-markup-indent-offset 2
         web-mode-css-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-indent-style 2)
  :config (setq-default indent-tabs-mode t)
  )

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
;; Global Settings
;; - in case they are modified by other modes

;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5 
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; modes
(electric-indent-mode 0)

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
