;;; emacs-common.el -- source of emacs customization
;;; Commentary:
;;;
;;; Code:
;;;

(defconst codebase-root (expand-file-name "~/CodeBase"))
(defconst emacs-dev-root (concat codebase-root "/emacs_dev"))
(defconst sygadry-tools-emacs-root (concat codebase-root "/sygaldry/tools/emacs"))

(add-to-list 'load-path emacs-dev-root)
(add-to-list 'load-path sygadry-tools-emacs-root)

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
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'async))

(defconst package-depslist
  '(
    async
    bazel-mode
    company
    clang-format
    dash
    dap-mode
    elpy
    ensime
    ess
    evil
    flycheck-pyre
    go-mode
    helm
    helm-flx
    helm-lsp
    lua-mode
    lsp-mode
    lsp-ui
    magit
    magit-popup
    markdown-mode
    projectile
    s
    solarized-theme
    swiper-helm
    terraform-mode
    tuareg
    use-package
    utop
    web-mode
    xterm-color
    z3-mode
    )
  "A list of dependencies to be installed.")

(require 'cl) ;; use the common-lisp extension
(defun package-deps-installed-p()
  "Return non-nil if the dependencies are all installed."
  (loop for pkg in package-depslist
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (package-deps-installed-p)
  (package-refresh-contents)
  (dolist (pkg package-depslist)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------------
;; Load packages with `use-package`
;;
;;--------------------------------------------------------------------

(require 'use-package) ;; get the dependencies
(dired-async-mode 1) ;;

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

;; Enable prettify symbols (Emacs 24.4+)
(global-prettify-symbols-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------------
;; ANSI color

;; comint install
;; Also remember to set TERM accordingly (xterm-256color)
;; Keep up-to-date https://github.com/atomontage/xterm-color
(use-package xterm-color
  ;; :load-path "xterm-color/"
  :config
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
	(remove 'ansi-color-process-output comint-output-filter-functions))
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
;; Programming Language Environment
;;
;;--------------------------------------------------------------------

(dap-mode 1)
(dap-ui-mode 1)

(use-package lsp-mode
  :hook
  (go-mode . lsp)
  (c++-mode . lsp)
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;;(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


;;--------------------------------------------------------------------
;; Python

;; (require 'flycheck-pyre)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-pyre-setup))

(defun python-mode-flycheck-checkers-hook ()
  "Configure all your checkers for `python-mode` here."
  (flycheck-mode)
  (flycheck-select-checker 'python-pylint)
  )
(add-hook 'python-mode-hook #'python-mode-flycheck-checkers-hook)

;; (elpy-enable)
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")
;; (setq elpy-modules '(elpy-module-sane-defaults
;;                      elpy-module-company
;;                      elpy-module-eldoc
;;                      elpy-module-highlight-indentation
;;                      elpy-module-yasnippet))

;;--------------------------------------------------------------------
;; C++

;; Let's disable `cling` until it becomes more stable.
;; (load "cling-mode.el")
;; (require 'cling-mode)

(require 'clang-format)

(setq-default c-basic-offset 2)
(with-eval-after-load 'cc-mode
  (fset 'c-indent-region 'clang-format-region)
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil
	clang-format-executable "/opt/third_party/llvm/bin/clang-format"
	)
  (bind-keys :map c-mode-base-map
             ("C-M-\\" . clang-format-region)
             ))

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++1z")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++1z")))

(defun subprocess--check-output (command-and-args)
  "Run a subprocess call with COMMAND-AND-ARGS and fetch output."
  (replace-regexp-in-string "\n$" ""
                            (shell-command-to-string command-and-args))
  )

(defun setup-flycheck-for-cpp ()
  "Add bazel external paths for linters."
  (interactive)
  (let (
        (bzl-extern (subprocess--check-output "bazel info output_base"))
        )
    (setq flycheck-clang-include-path
          (list
           (concat bzl-extern "/external/com_google_gtest/googletest/include")
           (concat bzl-extern "/external/com_google_gtest/googlemock/include")
           )
          )
  )
)

;;--------------------------------------------------------------------
;; Go
(defconst gopath (expand-file-name "~/CodeBase/sygaldry/golang"))
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(use-package lsp-mode
  :commands (lsp lsp-deferred))

(add-hook 'go-mode-hook #'lsp-deferred)

;; optional - provides fancier overlays
(use-package lsp-ui
  :commands lsp-ui-mode)

;; ;; if you use company-mode for completion 
;; ;; (otherwise, complete-at-point works out of the box):
;; (use-package company-lsp
;;   :commands company-lsp)


;; (require 'gore-shell-mode)
;; (setq gore-bin-path (concat gopath "/bin/gore"))
;; (setq gorepl-command (concat gopath "/bin/gore"))
;; (add-hook 'go-mode-hook
;;           (lambda() "Hook for key-bindings."
;;             (define-key go-mode-map (kbd "C-c C-c") 'gore-shell-eval-region)
;;             (define-key go-mode-map (kbd "C-c C-z") 'run-gore-shell)
;;             ))


;;--------------------------------------------------------------------
;; Scala
(require 'scala-custom)

;;--------------------------------------------------------------------
;; OCaml
(add-to-list 'auto-mode-alist '("\\.re$" . tuareg-mode))
(autoload 'utop "utop" "Toplevel for OCaml" t)
(setq utop-command "opam config exec -- utop -emacs")
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
;; (add-hook 'tuareg-mode-hook
;;           (lambda() "Prettify symbols"
;;             (when (functionp 'prettify-symbols-mode)
;;               (prettify-symbols-mode))))
(add-hook 'tuareg-mode-hook
          (lambda() "Hook for key-bindings of ocam/utop functions."
            (define-key tuareg-mode-map (kbd "C-c C-r") 'utop-eval-region)
            (define-key tuareg-mode-map (kbd "C-c C-z") 'utop)
            (define-key tuareg-mode-map (kbd "C-c C-d") 'utop-eval-buffer)))

;;--------------------------------------------------------------------
;; Coq
;;(load (concat emacs-dev-root "/prfgnrl/generic/proof-site.el"))
;;(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;; (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;;--------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode (site-lisp is the install location)
(load "orgmode-custom.el")

;; Magit
(require 'subr-x)
(require 'magit-popup)

(defun magit-arcanist--do-diff ()
  ;; Perform arcanist operations
  (interactive)
  (let ((repo-root (magit-toplevel))
	(curr-fname (buffer-file-name))
	)
    (message
     (concat "https://github.com/darthsuogles/emacs_dev/" (string-remove-prefix repo-root curr-fname)))
    )
  )

(magit-define-popup magit-arcanist-diff-popup
  "Popup console for Arcanist diff commands."
  :switches '((?l "No lint" "--nolint")
              (?u "No unit tests" "--nounit")
              (?c "No coverage info" "--no-coverage"))
  :actions '((?d "Diff" magit-arcanist--do-diff)))




;;--------------------------------------------------------------------
;; BazelBuild
(require 'bazel-mode)
(add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t)))

;;--------------------------------------------------------------------
;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;--------------------------------------------------------------------
;; HTML/JavaScript/CSS web-mode @ http://web-mode.org
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck http://www.flycheck.org/en/latest/
(package-install 'flycheck)
(global-flycheck-mode)

(package-install 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++1z")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++1z")))

;; Configure include paths
(setq spack-root (expand-file-name "~/CodeBase/spack/opt/spack/darwin-mojave-x86_64/clang-10.0.1-apple"))

(defun spack-pkg-include(pkg_digest &optional include_prefix)
  (let ((inc-path (if include_prefix include_prefix "include")))
    (concat spack-root "/" pkg_digest "/" inc-path)
    )
  )

(defconst spack-cpp-headers
  (list
   (spack-pkg-include
    "boost-1.70.0-kl27accx4oo6wouath344kvgytgp2fkf"
    )
   (spack-pkg-include
    "eigen-3.3.7-bd2r4aqrkox7dpebj2r3gqvgpqzwuh7x"
    "include/eigen3")
   )
  )

(add-hook 'c++mode-hook
	  (lambda () (setq flycheck-clang-include-path spack-cpp-headers)))

;; (package-install 'flycheck-pos-tip)
;; (eval-after-load 'flycheck (flycheck-pos-tip-mode))


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

;; Removing trailing white spaces when saving
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
;; Untabify
(add-hook 'before-save-hook
          'untabify)

;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 sentence-end-double-space nil)

;; modes
(electric-indent-mode 0)

(tool-bar-mode nil) ;; turning off the tool-bar
(transient-mark-mode t)

;; All about the meta keys
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

(setq
 ring-bell-function 'ignore
 visible-bell nil
 global-visual-line-mode t
 word-wrap t
 indent-tabs-mode nil
 ns-pop-up-frames nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set themes at last
;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)

;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)

;; Use less bolding
(setq solarized-use-less-bold t)

;; Use more italics
;; (setq solarized-use-more-italic t)

;; Use less colors for indicators such as git:gutter, flycheck and similar
(setq solarized-emphasize-indicators nil)

;; Don't change size of org-mode headlines (but keep other size-changes)
;; (setq solarized-scale-org-headlines nil)

;; Avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)

(load-theme 'solarized-dark t)
