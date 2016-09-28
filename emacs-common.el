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
;; Package managing 
;; (eval-when-compile
;;   (require 'use-package))
;; (require 'diminish)                ;; if you use :diminish
;; (require 'bind-key)                ;; if you use any :bind variant

(require 'package)
(add-to-list 'load-path (concat elisp_path "/use-package"))
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
		    ("melpa-stable" . "https://stable.melpa.org/packages/")
                    ))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'async))
(require 'use-package) ;; get the dependencies
(dired-async-mode 1) ;; 

(defvar package-depslist
  '(helm s company magit projectile dash async 
         use-package evil helm-flx swiper-helm
         web-mode ess lua-mode z3-mode
         ensime sbt-mode
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

(use-package helm
  :commands helm-config
  :init (setq 
         helm-apropos-fuzzy-match t
         helm-split-window-in-side-p t
         helm-move-to-line-cycle-in-source t
         helm-ff-search-library-in-sexp t
         helm-scroll-amount 8
         helm-ff-file-name-history-use-recentf t
         helm-buffers-fuzzy-matching t
         helm-recentf-fuzzy-match t
         helm-semantic-or-imenu t)
  :config (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-c C-m" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x c a" . helm-apropos)
         ("C-c h" . helm-apropos)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
)

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  ;;(add-hook 'after-init-hook 'company-statistics-mode t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------------
;; ANSI color

;; comint install
;; Also remember to set TERM accordingly (xterm-256color)
(use-package xterm-color
  :load-path "xterm-color/"
  :config
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
  (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)
)

;; You can also use it with eshell (and thus get color output from system ls):
(use-package eshell
  :init
  (add-hook 'eshell-mode-hook
            (lambda () (setq xterm-color-preserve-properties t)))
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------------
;; IPython
;;
;; Warning: do not use the master branch python.el
;; Please byte-compile the emacs-24 branch
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args "--pylab"
   )
  )

;;--------------------------------------------------------------------
;; Scala
(load "scala-custom.el")

;;--------------------------------------------------------------------
;; Lua / Torch7
(require 'torch-mode)
(autoload 'torch-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . torch-mode))
(add-to-list 'interpreter-mode-alist '("th" . torch-mode))
;; (setq 
;;  torch-default-application "th"
;;  torch-default-command-switches '("-i" "-g" "-a" "torch_repl_init.lua")
;;  )  

;; (use-package torch-mode
;;   :load-path elisp_path
;;   :pin manual
;;   :mode "\\.lua\\'"
;;   :interpreter "th"
;;   :init
;;   (setq 
;;    torch-default-application "th"
;;    torch-default-command-switches '("-i" "-g" "-a" "torch_repl_init.lua")
;;    )  
;;   :bind
;;   (:map torch-mode-map
;;         ("C-c C-z" . run-torch)
;;         ("C-c C-c" . torch-send-defun) 
;;         ("C-c C-l" . torch-send-current-line)
;;         ("C-c C-r" . torch-send-region)
;;    )
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode (site-lisp is the install location)
(load "orgmode-custom.el")

(require 'org2blog-autoloads)
(setq org2blog/wp-blog-alist
      '(("quantipress"
         :url "http://wordpress.quantifind.com/xmlrpc.php"
         :username "philip"
         :default-categories ("data-science" "emacs")
         :tags-as-categories nil)
        ))



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
(use-package flyspell
  :commands (flyspell-delay-command tex-mode-flyspell-verify)
  :config
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (setq flyspell-sort-corrections nil)
  (setq flyspell-doublon-as-error-flag nil)
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode nil)))) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Settings
;; - in case they are modified by other modes

;; Substitude certain patterns (Greek letters or math symbols)
;; with unicode
(load "emacs-rc-pretty-lambda.el")

;; New in Emacs 24, color theme, loaded at last
(load-theme 'solarized-dark "NO-CONFIRM")

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

(setq 
 ring-bell-function 'ignore
 visible-bell nil
 global-visual-line-mode t
 word-wrap t
 ns-pop-up-frames nil)

