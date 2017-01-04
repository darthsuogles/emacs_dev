;;; inferior-scala-mode.el - Interaction with a Scala interpreter.
;;; $Id: inferior-scala-mode.el 12783 2007-09-04 11:07:23Z michelou $

(require 'comint)

(defgroup amm-shell
  nil
  "Mode to interact with a SBT Ammonite Shell."
  :group 'scala
  :tag "Ammonite Shell")

(defcustom default-amm-shell "./sbt-amm-shell.sh"
  "Name of the interpreter to use by default."
  :type 'string
  :group 'amm-shell)

(defvar amm-shell-cmd default-amm-shell
  "The interpreter that `run-scala' should run.
If you want to change the defaut value, don't change that variable
but customize `scala-default-interpreter' instead.")

(defconst amm-shell-buffer-name "*sbt-amm-shell*")

(define-derived-mode amm-shell-mode comint-mode "SBT Ammonite Shell"
  "Major mode for interacting with a SBT Ammonite Shell interpreter.

\\{amm-shell-mode-map\\}"
  (define-key amm-shell-mode-map [(meta return)] 'comint-accumulate)

  ;; Comint configuration
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'amm-shell-input-sender))

(defun amm-shell-input-sender (proc string)
  (comint-send-string proc string)
  ;; (comint-send-string proc "\nemacs:end\n")) ;; Heineman's contrib (06/03/2007)
  (comint-send-string proc "\n"))

;;;###autoload
(defun amm-shell-running-p-1 ()
  ;; True iff a Scala interpreter is currently running in a buffer.
  (comint-check-proc amm-shell-buffer-name))

(defun amm-shell-check-running ()
  (unless (amm-shell-running-p-1)
    (error "sbt ammonite shell not running")))

;;;###autoload
(defun run-amm-shell (cmd-line)
  "Run a Scala interpreter in an Emacs buffer"
  (interactive (list (if current-prefix-arg
                         (read-string "Scala interpreter: " amm-shell-cmd)
                       amm-shell-cmd)))
  (if (amm-shell-running-p-1) 
      (switch-to-buffer amm-shell-buffer-name)
    (setq amm-shell-cmd cmd-line)
    (let ((cmd/args (split-string cmd-line)))
      (set-buffer
       (apply 'make-comint "sbt-amm-shell" (car cmd/args) nil (cdr cmd/args))))
    (amm-shell-mode)
    (pop-to-buffer amm-shell-buffer-name)))

(defun amm-shell-send-string (str &rest args)
  ;; Send string to interpreter
  (comint-send-string amm-shell-buffer-name (apply 'format str args))
  (comint-send-string amm-shell-buffer-name "\n"))

;;;###autoload
(defun scala-switch-to-amm-shell ()
  "Switch to buffer containing the interpreter"
  (interactive)
  (amm-shell-check-running)
  (switch-to-buffer amm-shell-buffer-name))

(defvar amm-shell-tmp-file nil)

;;;###autoload
(defun amm-shell-eval-region (start end)
  "Send current region to Scala interpreter."
  (interactive "r")
  (amm-shell-check-running)
  (comint-send-region amm-shell-buffer-name start end)
  (comint-send-string amm-shell-buffer-name "\n"))

;;;###autoload
(defun amm-shell-eval-definition ()
  "Send the current 'definition' to the Scala interpreter.
This function's idea of a definition is the block of text ending
in the current line (or the first non-empty line going
backwards), and begins in the first line that is not empty and
does not start with whitespace or '{'.

For example:

println( \"aja\")
println( \"hola\" )

if the cursor is somewhere in the second print statement, the
interpreter should output 'hola'.

In the following case, if the cursor is in the second line, then
the complete function definition will be send to the interpreter:

def foo =
  1 + 2
"
  (interactive)
  (save-excursion
    ;; find the first non-empty line
    (beginning-of-line)
    (while (and (not (= (point) (point-min)))
                (looking-at "\\s-*$"))
      (next-line -1))
    (end-of-line)
    (let ((end (point)))
      ;; now we need to find the start
      (beginning-of-line)
      (while (and (not (= (point) (point-min)))
                  (looking-at (mapconcat '(lambda (x) x)
                                         '("^$"       ; empty lines
                                           "^\\s-+"   ; empty lines or lines that start with whitespace
                                           "^\\s-*}") ; lines that start with a '}'
                                         "\\|")))
        (next-line -1)
        (beginning-of-line))
      (message "region %s %s" (point) end)
      (amm-shell-eval-region (point) end))))

;;;###autoload
(defun amm-shell-eval-buffer ()
  "Send whole buffer to Scala interpreter."
  (interactive)
  (amm-shell-eval-region (point-min) (point-max)))

(defvar amm-shell-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last scala-load-file.
Used for determining the default in the next one.")

;;;###autoload
(defun amm-shell-load-file (file-name)
  "Load a file in the Scala interpreter."
  (interactive (comint-get-source "Load Scala file: " amm-shell-prev-l/c-dir/file
                                  '(scala-mode) t))
  (amm-shell-check-running)
  (comint-check-source file-name)
  (setq amm-shell-prev-l/c-dir/file (cons (file-name-directory file-name)
                                            (file-name-nondirectory file-name)))
  (amm-shell-send-string ":load %s" file-name))

;;;###autoload
(defun scala-quit-amm-shell ()
  "Quit Scala interpreter."
  (interactive)
  (amm-shell-check-running)
  (amm-shell-send-string "\n:quit"))

(provide 'sbt-amm-mode)
