;;; cling-mode  --- Summary
;;; Commentary:
;;;   This mode provides access to the Cling C++ REPL from CERN.
(require 'cc-mode)
(require 'comint)

(defgroup cling-shell
  nil
  "Cling, the Modern C++ REPL from CERN"
  :group 'c++
  :tag "Cling Shell")

(defcustom cling-binary (concat (getenv "HOME") "/CodeBase/.cling/bin/cling")
  "Location of the cling repl binary used by default."
  :type 'string
  :group 'cling-shell)

;; '-std=c++1z' must be the last one, or comint will treat everything after as its params
(defcustom cling-params "-L folly -L eigen -std=c++1z"
  "Command line arguments passed to cling repl binary"
  :type 'string
  :group 'cling-shell)

;; Add key bindings to C++ major mode
(defun cling-c++-mode-hook ()
  "Hook for C++ mode - binding cling functions."
  (define-key c++-mode-map (kbd "C-c C-r") 'cling-send-region)
  (define-key c++-mode-map (kbd "C-c C-z") 'cling-shell)
  (define-key c++-mode-map (kbd "C-c C-d") 'cling-wrap-defun-and-send)
)
(add-hook 'c++-mode-hook 'cling-c++-mode-hook)

(defconst cling-shell-process-name "inferior-cling")
(defconst cling-shell-buffer-name (format "*%s*" cling-shell-process-name))

;;;###autoload
(defun cling-shell-running-p-1 ()
  ;; True iff a cling shell is currently running in a buffer.
  (comint-check-proc cling-shell-buffer-name))

;;;###autoload
(defun cling-shell-check-running ()
  ;; True iff a cling shell is currently running in a buffer.
  (unless (cling-shell-running-p-1)
    (error "cling c++ shell not running")))

;;;###autoload
(defun cling-shell (&optional flags)
  "Move to the buffer containing Cling, or create one if it does not exist.
Defaults to using the argument provided in cling-params."
  (interactive)
  (if (cling-shell-running-p-1)
      (switch-to-buffer cling-shell-buffer-name)
    (let ((flags (or flags cling-params)))
      (make-comint cling-shell-process-name cling-binary nil flags)
      (pop-to-buffer cling-shell-buffer-name))))

(defun cling-send-string (string &optional process)
  "Send a string terminated with a newline to the inferior-cling buffer. Has the effect of executing a command"
  (let ((process (or process (get-process cling-shell-process-name))))
    (comint-send-string process string)
    (comint-send-string process "\n")))

(defun cling-send-region (start end)
  "Sends the region in the current buffer between `start` and `end` to the inferior-cling buffer. Sends the currently selected region when called interactively."
  (interactive "r")
  (cling-send-string (buffer-substring start end)))

(defun cling-send-buffer ()
  "Sends the current buffer to the inferior-cling buffer."
  (interactive)
  (cling-send-region (point-min) (point-max))) ;;do i want to wrap-raw this?

(defun cling-wrap-raw (string)
  "Wraps `string` in \".rawInput\", which tells Cling to accept function definitions"
  (format ".rawInput 1\n%s\n.rawInput 0" string))

(defun cling-wrap-region-and-send (start end)
  "Sends the region between start and end (currently selected when called interactively) to cling in raw input mode "
  (interactive "r")
  (cling-send-string (cling-wrap-raw (buffer-substring start end))))

(defun flatten-function-def ()
  "Flattens a function definition into a single line. This makes it easier to send to the inferior-cling buffer"
  (interactive)
  (replace-regexp "
" "" nil (mark) (point))) ;;;Why did I do this again?

(defun select-defun ()
  "Selects the defun containing the point. Currently only works when point is on the line where the function's name is declared."
  (interactive)
  (move-beginning-of-line nil)
  (push-mark (point))
  (re-search-forward "{")
  (save-excursion
   (flatten-function-def))
  (backward-char)
  (forward-sexp))

(defun cling-wrap-defun-and-send ()
  "Sends the current defun to cling in raw input mode. Currently only works when point is on the first line of function definition."
  (interactive)
  (save-excursion
    (select-defun)
    (cling-wrap-region-and-send (mark) (point))
    (undo)
    (undo)));;;this is a rather leaky way of doing temporary changes. there should be some way to save buffer contents or something
;;;probably uses with-temp-buffer

(define-derived-mode cling-mode comint-mode "Cling C++ Shell"
  "Major mode for interacting with a Cling C++ Shell.

\\{cling-mode-map\\}"
  (define-key cling-mode-map [(meta return)] 'comint-accumulate))

(provide 'cling-mode)
;;; cling-mode ends here
