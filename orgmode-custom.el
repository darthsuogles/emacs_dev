;; org-mode
;;; Code:
(add-to-list 'load-path (concat emacs-dev-root "/org-mode/lisp"))
(add-to-list 'load-path (concat emacs-dev-root "/org-mode/contrib/lisp"))

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
;;(require 'ox-deck)
(require 'ox-confluence)
(require 'ob-rust)

;; Bibliography: org-bibnote-exp
(require 'org-bibnote-exp)
;; On the new Macbook pro with retina display and SSD, this works fast enough
(setq org-latex-create-formula-image-program 'imagemagick)

;; org babel: display source code for different languages
;; https://github.com/syl20bnr/spacemacs/issues/3314
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (rust . t)
         ;;(scala . t)
         (C . t)
         (shell . t)
         (stan . t)
         (org . t)
         (latex . t))))

                                        ; Do not prompt to confirm evaluation
                                        ; This may be dangerous - make sure you understand the consequences
                                        ; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE` type source code block in org-mode"
  (interactive
   (let ((src-code-types '("emacs-lisp" "python" "C" "sh" "js"  "css"
                           "rust" "C++"
			   "dot" "sql" "haskell" "latex"
			   "scala" "ocaml" "org" "perl")))
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

;;----
;; Export sections to separate files

;; export headlines to separate files
;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files
(defun org-export-subtrees-to-markdown()
  "Export all subtrees that are *not* tagged with :noexport: to separate files."
  (interactive)
  (save-buffer)
  (let ((backup-is-buffer-modified (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((maybe-export-fname-prop (org-entry-get (point) "EXPORT_FILE_NAME"))
               (subtree-title (nth 4 (org-heading-components))))
           (deactivate-mark)
           ;; Execute the actual export command
           (when maybe-export-fname-prop
             (message (format "[AUTO] Export [%s] to [%s]" subtree-title maybe-export-fname-prop))
             (org-md-export-to-markdown nil t nil))
           (set-buffer-modified-p backup-is-buffer-modified)))
       "-noexport"  ;; MATCH
       'region-start-level  ;; SCOPE
       ))))

(defun org-export-all-exportable-subtrees()
  "Export any subtree with the property entry EXPORT_FILE_NAME set."
  (org-map-entries
   (lambda ()
     (let ((maybe-export-fname-prop (org-entry-get (point) "EXPORT_FILE_NAME"))
           (subtree-title (nth 4 (org-heading-components))))
       (deactivate-mark)
       ;; Execute the actual export command
       (when maybe-export-fname-prop
         (message (format "[AUTO] Export [%s] to [%s]" subtree-title maybe-export-fname-prop))
         (org-md-export-to-markdown nil t nil))
       ))
   "-noexport"  ;; MATCH
   'file  ;; SCOPE
   ))


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
      '((sequence "TODO" "SKIMMED" "2e-PASS" "|" "DISTILL" "SALVAGE" "DONE" "SKIP")
	    (sequence "|" "CANCELED(c)")))
