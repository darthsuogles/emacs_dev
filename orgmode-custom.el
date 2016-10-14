;; org-mode
;;(add-to-list 'load-path (concat elisp_path "/orgmode/current/lisp"))
;;(add-to-list 'load-path (concat elisp_path "/orgmode/current/contrib/lisp"))

(setq org-root "~/org")
(add-to-list 'auto-mode-alist '("\\.\\(org\\  |org_archive\\|txt\\)$" . org-mode))
(require 'org-install)
(require 'org-habit)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(transient-mark-mode 1)

;; org-mode experimental stuffs
;; http://lists.gnu.org/archive/html/emacs-orgmode/2013-10/msg00466.html
(setenv "TMP" ".") ;; a work-around for bibtex2html and inline LaTeX display
;; http://orgmode.org/worg/org-tutorials/org-latex-preview.html
;;(setq org-latex-create-formula-image-program 'imagemagick)
;; http://www.gnu.org/software/auctex/manual/auctex/index.html
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;(require 'ox-bibtex)
(require 'ox-beamer)
(require 'ox-md)

;; Bibliography: org-bibnote-exp
(require 'org-bibnote-exp)
;; On the new Macbook pro with retina display and SSD, this works fast enough
(setq org-latex-create-formula-image-program 'imagemagick)

;; org babel: display source code for different languages
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (sh . t)
         (org . t)
         (latex . t))))

					; Do not prompt to confirm evaluation
					; This may be dangerous - make sure you understand the consequences
					; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE` type source code block in org-mode"
  (interactive
   (let ((src-code-types '("emacs-lisp" "python" "C" "sh" "js" "C++" "css"
			   "dot" "matlab" "sql" "ditaa" "haskell" "latex"
			   "scala" "ocaml" "org" "perl" "ruby")))
     (list (ido-completing-read "Source code type: " src-code-types))))
   (progn
     (newline-and-indent)
     (insert (format "#+BEGIN_SRC %s\n" src-code-type))
     (newline-and-indent)
     (insert "#+END_SRC\n")
     (previous-line 2)
     (org-edit-src-code)))

;; http://wenshanren.org/?p=334
(add-hook 'org-mode-hook '(lambda ()
			    ;; turn on flyspell-mode by default
			    (flyspell-mode 1)
			    ;; key-bindings for inserting code blocks
			    (local-set-key (kbd "C-c s i") 'org-insert-src-block) ))
(setq org-src-fontify-natively t) ;; syntax highlighting in the source block

;;--------------------------------------------------------------------
;; LaTeX options
(require 'ox-latex)
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "xcolor"))
(setq org-latex-minted-options
      '(
	;;("bgcolor" "LightGray")
	("frame" "lines")
	))

(add-to-list 'org-latex-classes
	     '("org-bare-metal"
	       "\\documentclass{article}[letterpaper,11pt]
                [NO-DEFAULT-PACKAGES]
                \\usepackage{qtfd}
                [PACKAGES]
                [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
	     '("llncs"
	       "\\documentclass{llncs}
                [NO-DEFAULT-PACKAGES]
                \\usepackage{computastica}
                [PACKAGES]
                [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; VLDB mode, kinda tricky
(add-to-list 'org-latex-classes
	     '("vldb"
	       "\\documentclass{vldb}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;--------------------------------------------------------------------
(setq org-todo-keywords
      '((sequence "TODO" "SKIMMED" "2e-PASS" "|" "DONE" "SKIP")
	(sequence "|" "CANCELED(c)")))

