;;; inferior-scala-mode.el - Interaction with a Scala interpreter.
;;; $Id: inferior-scala-mode.el 12783 2007-09-04 11:07:23Z michelou $

(require 'comint)

(defgroup spark-shell
  nil
  "Mode to interact with a Spark Shell."
  :group 'scala
  :tag "Spark Shell")

(defcustom default-spark-shell "./spark-shell.sh"
  "Name of the interpreter to use by default."
  :type 'string
  :group 'spark-shell)

(defvar spark-shell-cmd default-spark-shell
  "The interpreter that `run-scala' should run.
If you want to change the defaut value, don't change that variable
but customize `scala-default-interpreter' instead.")

(defconst spark-shell-buffer-name "*spark-shell*")

(define-derived-mode spark-shell-mode comint-mode "Spark Shell"
  "Major mode for interacting with a Spark shell interpreter.

\\{spark-shell-mode-map\\}"
  (define-key spark-shell-mode-map [(meta return)] 'comint-accumulate)

  ;; Comint configuration
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'spark-shell-input-sender))

(defun spark-shell-input-sender (proc string)
  (comint-send-string proc string)
  ;; (comint-send-string proc "\nemacs:end\n")) ;; Heineman's contrib (06/03/2007)
  (comint-send-string proc "\n"))

;;;###autoload
(defun spark-shell-running-p-1 ()
  ;; True iff a Scala interpreter is currently running in a buffer.
  (comint-check-proc spark-shell-buffer-name))

(defun spark-shell-check-running ()
  (unless (spark-shell-running-p-1)
    (error "spark shell not running")))

;;;###autoload
(defun run-spark-shell (cmd-line)
  "Run a Scala interpreter in an Emacs buffer"
  (interactive (list (if current-prefix-arg
                         (read-string "Scala interpreter: " spark-shell-cmd)
                       spark-shell-cmd)))
  (if (spark-shell-running-p-1) 
      (switch-to-buffer spark-shell-buffer-name)
    (setq spark-shell-cmd cmd-line)
    (let ((cmd/args (split-string cmd-line)))
      (set-buffer
       (apply 'make-comint "spark-shell" (car cmd/args) nil (cdr cmd/args))))
    (spark-shell-mode)
    (pop-to-buffer spark-shell-buffer-name)))

(defun spark-shell-send-string (str &rest args)
  ;; Send string to interpreter
  (comint-send-string spark-shell-buffer-name (apply 'format str args))
  (comint-send-string spark-shell-buffer-name "\n"))

;;;###autoload
(defun spark-switch-to-spark-shell ()
  "Switch to buffer containing the interpreter"
  (interactive)
  (spark-shell-check-running)
  (switch-to-buffer spark-shell-buffer-name))

(defvar spark-shell-tmp-file nil)

;;;###autoload
(defun spark-shell-eval-region (start end)
  "Send current region to Scala interpreter."
  (interactive "r")
  (spark-shell-check-running)
  (comint-send-region spark-shell-buffer-name start end)
  (comint-send-string spark-shell-buffer-name "\n"))

;;;###autoload
(defun spark-shell-eval-definition ()
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
      (spark-shell-eval-region (point) end))))

;;;###autoload
(defun spark-shell-eval-buffer ()
  "Send whole buffer to Scala interpreter."
  (interactive)
  (spark-shell-eval-region (point-min) (point-max)))

(defvar spark-shell-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last scala-load-file.
Used for determining the default in the next one.")

;;;###autoload
(defun spark-shell-load-file (file-name)
  "Load a file in the Scala interpreter."
  (interactive (comint-get-source "Load Scala file: " spark-shell-prev-l/c-dir/file
                                  '(scala-mode) t))
  (spark-shell-check-running)
  (comint-check-source file-name)
  (setq spark-shell-prev-l/c-dir/file (cons (file-name-directory file-name)
                                            (file-name-nondirectory file-name)))
  (spark-shell-send-string ":load %s" file-name))

;;;###autoload
(defun spark-quit-spark-shell ()
  "Quit Scala interpreter."
  (interactive)
  (spark-shell-check-running)
  (spark-shell-send-string "\n:quit"))

(provide 'spark-shell-mode)
