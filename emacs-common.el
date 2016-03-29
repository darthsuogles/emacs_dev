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

(add-hook 'after-init-hook 'global-company-mode)
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
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

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

;; global settings
(tool-bar-mode nil) ;; turning off the tool-bar
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
