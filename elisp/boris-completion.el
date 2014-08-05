;;; boris-completion.el --- context-sensitive completion hack for boris repl -*- lexical-binding: t -*-

;; Copyright (C) 2013-2014 Jon Oddie <jonxfield@gmail.com>

;; Author: Jon Oddie <jonxfield@gmail.com>
;; Version: 0.36
;; Keywords: php, repl, boris

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;;; Code:

(require 'php-mode)
(require 'bindat)
(require 'json)
(require 'eldoc)
(require 'format-spec)
(require 'cl-lib)

;;; Silence compilation warnings.
(eval-when-compile
  (require 'company))

;;; Customization options
;;;###autoload
(defgroup boris nil
  "PHP Boris REPL with smart completion."
  :group 'php)

;;;###autoload
(defcustom boris-command "boris"
  "Boris executable"
  :group 'boris
  :type 'string)

;;;###autoload
(defcustom boris-args '("-l")
  "Command-line arguments to use when starting Boris."
  :group 'boris
  :type '(repeat string))

;;;###autoload
(defcustom boris-connect-on-repl-start t
  "When non-nil, connect to Boris for smart completions on starting the REPL."
  :group 'boris
  :type 'boolean)

;;;###autoload
(defcustom boris-port 8015
  "Default port to use when connecting to Boris."
  :group 'boris
  :type 'integer)

;;;###autoload
(defcustom boris-start-timeout 5
  "Seconds to allow for Boris to start up before giving up."
  :group 'boris)

;;;###autoload
(defcustom boris-compilation-regexp-alist
  '(php boris-php-backtrace xdebug-error xdebug-stacktrace)
  "Value for `compilation-error-regexp-alist' in the Boris comint buffer.

Set to nil to disable `compilation-shell-minor-mode' in Boris
comint buffers.")

;;;###autoload
(defface boris-mode-line-run
    '((default :inherit compilation-mode-line-run))
  "Face for Boris's mode line indicator when Comint running but no connection."
  :group 'boris)

;;;###autoload
(defface boris-mode-line-connected
  '((default :inherit compilation-mode-line-exit))
  "Face for Boris's mode line indicator when connected."
  :group 'boris)

;;;###autoload
(defface boris-mode-line-disconnected
  '((default :strike-through t))
  "Face for Boris's mode line indicator when not connected."
  :group 'boris)

;;; Internal variables
(defvar boris-comint-process-name "boris"
  "Name of the Boris comint process")
(defvar boris-comint-process nil
  "Comint process for Boris.")
(defvar boris-comint-process-buffer nil
  "Comint buffer for Boris.")

(defvar boris-process-name "boris-connection"
  "Process name of the Boris side-channel connection")
(defvar boris-process nil
  "Process object representing the side-channel connection to Boris.")
(defvar boris-buffer nil
  "Buffer for processing messages from the side-channel connection to Boris.")

;; Timer object used to wait and connect the side-channel connection
;; after starting the Comint process
(defvar boris-connect-timer nil)

;; Previous value of `eldoc-documentation-function' before turning on
;; Boris mode.  This will be tried first before calling the server,
;; since it may be faster.
(defvar boris-original-eldoc-function nil)

(defvar boris-recent-buffer nil
  "Buffer that was active before last calling `boris-open-or-pop-to-repl'.")

;; The queue of callbacks awaiting a response from the server.
;; Callbacks are added to the end of the list in `boris-call', removed
;; from the front of the list and called in `boris-filter' when a
;; complete response is assembled.
(defvar boris-async-callbacks nil)

(defconst boris-request-format
  ;; Byte packing format for requests to the PHP backend:
  ;; - length of the request data as a long integer (four bytes)
  ;; - request data encoded as JSON
  '((:length long)
    (:data str (:length))))

(defconst boris-response-format
  ;; Byte packing format for responses from the PHP backend:
  ;; - one byte result code (see `boris-response-code-regexp')
  ;; - four bytes (long integer) specifying response length
  ;; - response data encoded as JSON
  '((:code str 1)
    (:length long)
    (:data str (:length))))

(defconst boris-response-code-regexp
  ;; Legal first byte codes in a response from the Boris server
  ;; connection. See EvalWorker.php. Tooling calls (completion, Eldoc,
  ;; etc.) should generally begin with 4 (RESPONSE).
  (rx-to-string
   '(any
     0 ; done, OK
     1 ; exited
     2 ; failed
     3 ; ready
     4 ; response
     )))

;; Timeout for `accept-process-output', in seconds
(defvar boris-timeout 0.5)

;; Hard timeout for synchronous calls, in seconds:
;; give up after this amount of time
(defvar boris-hard-timeout 2)


;;;; Side-channel communication for completions, eldoc, doc lookup

;; t when the Comint process is running
(defun boris-comint-running-p ()
  (and boris-comint-process (process-live-p boris-comint-process)))

;; t when the side channel is connected 
(defun boris-connected-p ()
  (and boris-process (process-live-p boris-process)))

;;;###autoload
(defun boris-connect ()
  (interactive)
  
  (unless (boris-comint-running-p)
    (message "Starting Boris...")
    (save-window-excursion (boris))
    (message "Done.")
    (sleep-for 0.1))

  (message "Connecting to Boris on port %d..." boris-port)
  (when boris-process (delete-process boris-process))
  (when boris-buffer (kill-buffer boris-buffer))
  (setq boris-buffer (get-buffer-create " *boris connection*"))
  (with-current-buffer boris-buffer (set-buffer-multibyte nil))
  (condition-case error-data
      (progn
        (setq boris-process
              (open-network-stream boris-process-name boris-buffer
                                   "127.0.0.1" boris-port))
        (set-process-coding-system boris-process 'binary 'binary)
        (set-process-query-on-exit-flag boris-process nil)
        (set-process-filter boris-process 'boris-filter)
        (setq boris-async-callbacks nil)
        (message "Connecting to Boris on port %d... done." boris-port))
    (file-error
     (let ((details (caddr error-data)))
       (message "Connecting to Boris on port %d failed: %s" boris-port details)
       (kill-buffer boris-buffer)
       (setq boris-process nil
             boris-buffer nil)))))

(defun boris-call (data &optional callback)
  (process-send-string boris-process (boris-pack-request data))
  (cl-flet ((enqueue (callback)
              (setq boris-async-callbacks
                    (append boris-async-callbacks
                            (list callback)))))
    (if callback
        ;; Handle asynchronously -- mostly for ElDoc
        (enqueue callback)
      ;; Wrap with synchronous waiting and timeout
      (let* ((response nil)
             (success nil)
             (timed-out nil)
             (message "Waiting for Boris response..")
             ;; Timeout callback
             (timer
              (run-with-timer boris-hard-timeout nil
                              (lambda () (setq timed-out t)))))
        ;; Success callback
        (enqueue
         (lambda (received)
           (cancel-timer timer)
           (if (not timed-out)
               (setq response received
                     success t)
             (message "Discarded message handled after timeout."))))
        ;; Wait ...
        (while (not (or success timed-out))
          (accept-process-output boris-process boris-timeout)
          (unless success
            (setq message (concat message "."))
            (message "%s" message)))
        (if success
            response
          (message "%s" (concat message "timed out."))
          nil)))))

(defun boris-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))

      ;; Delete any left-over garbage
      (when (> (point-max) (point-min))
        (save-excursion
          (goto-char (point-min))
          (when (not (looking-at boris-response-code-regexp))
            (search-forward-regexp boris-response-code-regexp nil t)
            (let ((garbage
                   (delete-and-extract-region
                    (point-min) (match-beginning 0))))
              (message "Discarded partial message \"%s\" from Boris."
                       garbage)))))

      ;; Handle all responses in buffer in order
      (catch 'done
        (while (> (point-max) (point-min))
          (let ((unpacked
                 (condition-case _
                     (bindat-unpack boris-response-format
                                    (buffer-substring (point-min) (process-mark proc)))
                   (args-out-of-range nil))))
            (if (not unpacked)
                (throw 'done t)
              (let* ((json-object-type 'plist)
                     (json-array-type 'list)
                     (json-false nil)
                     (response
                      (json-read-from-string (bindat-get-field unpacked :data)))
                     (read-chars
                      (bindat-length boris-response-format unpacked)))
                (delete-region (point-min) (+ (point-min) read-chars))
                (when boris-async-callbacks
                  (funcall (pop boris-async-callbacks) response))))))))))

(defun boris-show-connection ()
  (interactive)
  (pop-to-buffer boris-buffer))

(defun boris-pack-request (data)
  (let* ((json (json-encode data))
         (len (length json)))
    (bindat-pack boris-request-format
                 `((:length . ,len)
                   (:data . ,json)))))

;;;###autoload
(defun boris-completion-at-point ()
  (when (boris-connected-p)
    (let ((line (buffer-substring-no-properties (point-at-bol) (point)))
          (evaluate-p (eq major-mode 'boris-mode)))
      (if (string-match-p "\\`[[:space:]]*\\'" line)
          ;; Bail out at beginning of line
          nil
        (cl-destructuring-bind (&key start end completions)
            (boris-call (list :operation :complete
                           :line line
                           :evaluate evaluate-p))
          (when completions
            (list
             (+ (point-at-bol) start) (+ (point-at-bol) end)
             completions)))))))

;;;###autoload
(defun boris-eldoc-function ()
  (or (and (functionp boris-original-eldoc-function)
           (funcall boris-original-eldoc-function))
      (when (boris-connected-p)
        (boris--call-for-info 'hint #'boris-eldoc-callback)
        "")))

(defun boris-eldoc-callback (response)
  (when response
    (message "%s" response)))

;;;###autoload
(defun boris-get-documentation ()
  (interactive)
  (let ((docs (boris--call-for-info 'documentation)))
    (when docs
      (with-help-window "*boris help*"
        (princ docs))
      (with-current-buffer "*boris help*"
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (while (re-search-forward
                  "@@ \\(.*\\) \\([0-9]+\\)[[:space:]]*-[[:space:]]*[0-9]+" nil t)
            (let* ((start (match-beginning 0))
                   (end (match-end 0))
                   (file-name (match-string 1))
                   (line-number (string-to-number (match-string 2))))
              (make-text-button
               start end
               'boris-file-name file-name
               'boris-line-number line-number
               'action 'boris-xref
               'follow-link t))))))))

(defun boris-xref (button)
  (let ((file-name (button-get button 'boris-file-name))
        (line-number (button-get button 'boris-line-number)))
    (pop-to-buffer (find-file file-name))
    (widen)
    (goto-char (point-min))
    (forward-line (1- line-number))))

(defun boris--call-for-info (operation &optional callback)
  ;; Bail out in strings and comments
  (if (boris--in-string-or-comment)
      (if callback (funcall callback nil) nil)
    (let* ((text
            (buffer-substring-no-properties (boris--beginning-of-text)
                                            (point)))
           (evaluate-p (eq major-mode 'boris-mode)))
      (if (and (eq operation :annotate) (equal text ""))
          ;; Hack: Bail out instead of requesting information on an
          ;; empty string (which returns information on all symbols
          ;; and slows things down).  Need a more robust fix for this.
          (if callback (funcall callback nil) nil)
        ;; Otherwise make the request
        (boris-call (list :operation operation
                          :line text
                          :evaluate evaluate-p)
                    callback)))))

(defun boris--in-string-or-comment ()
  (cl-destructuring-bind
        (_ _ _ in-string in-comment &rest ignore)
      (syntax-ppss)
    (or in-string in-comment)))

(defun boris--beginning-of-text ()
  ;; Find the beginning of text to send for documentation purposes.
  ;; This would normally be the beginning of line, but it could be
  ;; several lines up when point is inside a multi-line argument list.
  (or
   ;; Try to move up out of the innermost list of function arguments
   (save-excursion
     (ignore-errors
       (backward-up-list)
       (if (looking-at "(")
           (point-at-bol)
         nil)))
   ;; Either there was an error moving outside of the enclosing list,
   ;; or the enclosing list doesn't start with (, so not an argument
   ;; list.  In this case just use the beginning of line.
   (point-at-bol)))


;;;; Minor mode for source buffers

;; Convenience macros for defining 
(defmacro boris-define-prefix-map (name bindings)
  (declare (indent 1))
  `(progn
     (define-prefix-command ',name)
     (boris-bind-keys ,name ,bindings)
     ,name))

;; Based on slime-bind-keys
(defun boris-bind-keys (keymap bindings)
  (cl-loop for (key command) in bindings do
           (progn
             (define-key keymap `[,key] command)
             (unless (equal key ?h)
               (define-key keymap `[(control ,key)] command)))))

;; These prefix maps are reused for both the minor mode and comint mode
(boris-define-prefix-map boris-who-map
  '((?u boris-who-uses)
    (?i boris-who-implements)
    (?e boris-who-extends)))

(boris-define-prefix-map boris-doc-map
  '((?d boris-get-documentation)
    (?a boris-apropos)))

;; Menu items shared between minor mode and comint buffer
(defvar boris-common-menu
  '(["Load file..." boris-load-file t]
    ["Help on thing at point" boris-get-documentation (boris-connected-p)]
    ["Search symbols..." boris-apropos (boris-connected-p)]
    ["Who implements..." boris-who-implements (boris-connected-p)]
    ["Who extends..." boris-who-extends (boris-connected-p)]
    ["Who uses..." boris-who-uses (boris-connected-p)]
    ))

(defvar boris-minor-mode-menu
  `("Boris"
    ["Open REPL" boris-open-or-pop-to-repl t]
    ["Restart Boris" boris-restart t]
    ["Connect to Boris" boris-connect (not (boris-connected-p))]
    "--"
    ,@boris-common-menu
    ))

(defvar boris-mode-menu
  `("Boris"
    ["" boris-restart-or-pop-back
        :enable t
        :label (if (and (boris-comint-running-p)
                        boris-recent-buffer
                        (buffer-live-p boris-recent-buffer))
                   (format "Back to '%s'" (buffer-name boris-recent-buffer))
                 "Start Boris")]
    ["Restart Boris" boris-restart (boris-comint-running-p)]
    ["Connect to Boris" boris-connect (not (boris-connected-p))]
    "--"
    ,@boris-common-menu
    ))

;;;###autoload
(define-minor-mode boris-minor-mode
    "Minor mode for completion and Eldoc via Boris in PHP buffers."
  :lighter (:eval (boris-mode-line-process))
  :keymap
  `((,(kbd "C-c C-z") . boris-open-or-pop-to-repl)
    (,(kbd "C-c C-l") . boris-load-file)
    (,(kbd "C-c C-d") . boris-doc-map)
    (,(kbd "C-c C-w") . boris-who-map))

  (if boris-minor-mode
      ;; turn on 
      (progn
        (setq boris-original-eldoc-function eldoc-documentation-function)
        (set (make-local-variable 'eldoc-documentation-function)
             #'boris-eldoc-function)
        (eldoc-mode +1)
        (eldoc-add-command 'completion-at-point)
        (add-hook 'completion-at-point-functions #'boris-completion-at-point nil t)

        (when (require 'company nil t)
          (make-local-variable 'company-backends)
          (push #'boris-company company-backends)
          (company-mode 1)))

    ;; turn off
    (setq eldoc-documentation-function boris-original-eldoc-function)
    (remove-hook 'completion-at-point-functions #'boris-completion-at-point t)))

(easy-menu-define nil boris-minor-mode-map
  ""
  boris-minor-mode-menu)

(defun boris-open-or-pop-to-repl ()
  (interactive)
  (setq boris-recent-buffer (current-buffer))
  (if (boris-comint-running-p)
      (pop-to-buffer (process-buffer boris-comint-process))
    ;; call-interactively passes along any prefix argument
    (call-interactively #'boris)))

(defun boris-mode-line-process ()
  (cond ((boris-connected-p)
         (propertize " Boris"
                     'face 'boris-mode-line-connected
                     'help-echo "Connected to running REPL."))
        ((boris-comint-running-p)
         (propertize " Boris" 'face 'boris-mode-line-run
                     'help-echo "REPL running but not connected."))
        (t
         (propertize " Boris" 'face 'boris-mode-line-disconnected
                     'help-echo "No running REPL."))))

(defun boris-load-file (file-name)
  (interactive (list (buffer-file-name)))
  (unless (boris-comint-running-p)
    (save-window-excursion
      (boris)))
  (let ((process (get-process boris-comint-process)))
    (comint-send-string process
                        (format "require '%s';\n"
                                (replace-regexp-in-string "'" "\\'" file-name)))
    (pop-to-buffer (process-buffer process))))



;;;; A simple comint-mode

(defvar boris-command-history nil)
(defvar boris-remote-host-history nil)
(defvar boris-remote-command-history nil)

;;;###autoload
(define-derived-mode boris-mode comint-mode "boris REPL"
  "Major mode for Boris PHP REPL with smart completion."
  :syntax-table boris-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(nil t t))

  ;; delq seems to change global variables if called this phase
  (set (make-local-variable 'comint-dynamic-complete-functions)
       (delete 'comint-dynamic-complete-filename comint-dynamic-complete-functions))

  ;; Completion
  (when (< emacs-major-version 24)
    (add-hook 'comint-dynamic-complete-functions 'completion-at-point
              nil t))
  (set (make-local-variable 'completion-at-point-functions)
       '(boris-completion-at-point))
  
  ;; Eldoc
  (set (make-local-variable 'eldoc-documentation-function)
       'boris-eldoc-function)
  (eldoc-add-command 'completion-at-point)
  (eldoc-add-command 'comint-dynamic-complete)
  (eldoc-mode +1)

  ;; Company-mode
  (when (require 'company nil t)
    (make-local-variable 'company-backends)
    (push #'boris-company company-backends)
    (company-mode 1))

  ;; Compilation errors
  (when boris-compilation-regexp-alist
    (make-local-variable 'compilation-error-regexp-alist)
    (setq compilation-error-regexp-alist
          boris-compilation-regexp-alist)
    (compilation-shell-minor-mode)))

(defvar boris-mode-syntax-table php-mode-syntax-table)

(define-key boris-mode-map (kbd "TAB")
  (if (< emacs-major-version 24)
      'comint-dynamic-complete
    'completion-at-point))
(define-key boris-mode-map (kbd "C-c C-z") 'boris-restart-or-pop-back)
(define-key boris-mode-map (kbd "C-c C-d") 'boris-doc-map)
(define-key boris-mode-map (kbd "C-c C-w") 'boris-who-map)

(easy-menu-define nil boris-mode-map
  ""
  boris-mode-menu)

;;;###autoload
(defun boris (&optional command)
  "Run a Boris PHP REPL in a comint buffer with smart completion and Eldoc.

With prefix argument, prompt for a specific command line to start
Boris.  Otherwise, the command to run is determined by the
variables `boris-command' and `boris-args'.  Boris must be
started with the \"-l\" or \"--listen\" flag to provide smart
completions, eldoc, jump-to-definition and other features.

If there is already a Boris process running, just switch to its
buffer, unless called with a prefix argument to run a specific
command.  In that case, ask before killing the currently running
process and starting a new one."
  (interactive
   (when current-prefix-arg
     (boris-query-kill-if-running)
     (list (read-string "Run command: " nil 'boris-command-history))))
  
  (condition-case error-data
      (let ((new-process-p (not (boris-comint-running-p))))
        (if new-process-p
            (message "Starting new Boris REPL.")
          (message "Boris REPL already running."))

        (if (null command)
            ;; Use the default boris-command & boris-args
            (setq boris-comint-process-buffer
                  (boris--make-comint boris-command boris-args))

          ;; Split custom command and run it
          (let* ((words (split-string command))
                 (program (car words))
                 (arguments (cdr words)))
            (setq boris-comint-process-buffer
                  (boris--make-comint program arguments))))

        ;; Keep a reference to the comint process
        (setq boris-comint-process
              (get-buffer-process boris-comint-process-buffer))

        ;; Pop to comint buffer
        (pop-to-buffer boris-comint-process-buffer)
 
        ;; Connect side-channel
        (when (and new-process-p boris-connect-on-repl-start)
          (setq boris-connect-timer
                (run-at-time boris-start-timeout nil
                             (lambda ()
                               (message "Connection timeout. Try M-x boris-connect.")
                               (set-process-filter boris-comint-process 'comint-output-filter))))
          (set-process-filter boris-comint-process 'boris-wait-and-connect)))

    ;; Clean up on error
    (error
     (setq boris-comint-process-buffer nil)
     (setq boris-comint-process nil)
     (signal (car error-data) (cdr error-data)))))

;;;###autoload
(defun boris-remote ()
  "Run a remote Boris REPL via SSH.

Prompts for host and side-channel port number, then allows
editing the SSH command line before running it in a `boris-mode'
Comint buffer.  Boris must be installed on the remote machine.
The default command line forwards the remote process side-channel
connection over SSH, so that smart completion and other features
work as normal."
  (interactive)
  (boris-query-kill-if-running)
  (let*
      ((default-host
        (if (consp boris-remote-host-history)
            (car boris-remote-host-history)
          nil))
       (host-prompt
        (if default-host
            (format "Host (default '%s'): " default-host)
          "Host: "))
       (host (read-string host-prompt nil 'boris-remote-host-history default-host))
       (port (read-number "Port: " 8015))
       (command
        (read-string "Run command: "
                     (format-spec
                      "ssh -t %h -L %p:localhost:%p boris --listen=%p"
                      `((?h . ,host) (?p . ,port)))
                     'boris-remote-command-history)))
    (boris command)))

(defun boris--make-comint (program arguments)
  (let ((comint-buffer
         (apply 'make-comint boris-comint-process-name
                program nil arguments)))
    (with-current-buffer comint-buffer
      (boris-mode)
      ;; Save the command and arguments as local variables, so that
      ;; restarting the REPL via boris-restart-or-pop-back will re-run
      ;; the same command.
      (setq-local boris-command program)
      (setq-local boris-args arguments)
      ;; Restore input history from Boris's history file if possible
      (let ((comint-input-ring-file-name (expand-file-name "~/.boris_history"))
            (comint-input-history-ignore (regexp-quote "_HiStOrY_V2_")))
        (comint-read-input-ring t)))
    comint-buffer))

(defun boris-wait-and-connect (process string)
  "Wait for Boris to start and connect to the port printed on the first line.

This is installed as a process filter function by `boris'.  It
passes all process output to the standard `comint-output-filter'
function."
  (comint-output-filter process string)
  (when (string-match "Listening on port \\([0-9]+\\)" string)
    (setq boris-port (string-to-number (match-string 1 string)))
    (message "port %d" boris-port)
    (boris-connect)
    (when boris-connect-timer
      (cancel-timer boris-connect-timer)
      (setq boris-connect-timer nil))
    (set-process-filter process 'comint-output-filter)))

(defun boris-restart-or-pop-back ()
  "Switch back to most recent PHP source buffer, or start a new Boris REPL."
  (interactive)
  (if (boris-comint-running-p)
      ;; Pop to recent buffer
      (if (and boris-recent-buffer
               (buffer-live-p boris-recent-buffer))
          (pop-to-buffer boris-recent-buffer))
    
    ;; Not running: try to restart.
    (call-interactively #'boris)))

(defun boris-restart ()
  "Kill any running boris process and restart it."
  (interactive)
  (boris-kill-processes)
  (call-interactively #'boris))

(defun boris-query-kill-if-running ()
  (when (boris-comint-running-p)
    (if (yes-or-no-p "Boris already running. Kill process and start new REPL?")
        (boris-kill-processes)
      (error "Boris already running."))))

(defun boris-kill-processes ()
  (when (boris-comint-running-p)
    ;; Try clean shutdown first
    (process-send-eof boris-comint-process)
    (sit-for 0.1)
    ;; Kill process if clean shutdown failed
    (when (boris-comint-running-p)
      (delete-process boris-comint-process)))
  ;; Disconnect side-channel
  (when (boris-connected-p)
    (delete-process boris-process)))


;;; Company-mode integration

(defvar boris-company-data nil)
(defvar boris-company-last-point nil)
(defvar boris-company-last-tick nil)

(defun boris-company (command &optional arg &rest _ignore)
  (interactive 'interactive)
  (when (boris-connected-p)
    (cl-case command
      (interactive (company-begin-backend 'boris-company))

      (prefix
       (cl-destructuring-bind (&key start end &allow-other-keys)
           (boris--company-data)
         (when (and start end)
           (let ((prefix (buffer-substring start end))
                 (show-all-p (looking-back "\\(\\(->\\|::\\)\\s-*\\)\\|new\\s-*\\(\\sw\\|\\s_\\)*")))
             (cons prefix (or show-all-p (length prefix)))))))
      
      (candidates
       (plist-get (boris--company-data) :completions))

      (meta
       (boris--get-annotation arg :description))

      (annotation
       (boris--get-annotation arg :arguments))

      (location
       (cl-destructuring-bind (&key file line &allow-other-keys)
           (boris--get-annotations arg)
         (cons file line)))

      (doc-buffer
       (company-with-candidate-inserted arg
         (company-doc-buffer (boris--call-for-info :documentation))))

      (post-completion
       (funcall eldoc-documentation-function))

      (require-match nil)

      (t nil))))

(defun boris--company-data ()
  (cond
    ;; Bail in strings and comments
    ((boris--in-string-or-comment) nil)

    ;; Return cached data if possible
    ((and (equal (point) boris-company-last-point)
          (equal (buffer-modified-tick) boris-company-last-tick))
     boris-company-data)

    ;; Otherwise call Boris for info
    (t
     (let ((start-point (boris--beginning-of-text))
           (response (boris--call-for-info :annotate)))
       (setq boris-company-data
             (boris--parse-company-data response start-point))
       (setq boris-company-last-point (point)
             boris-company-last-tick (buffer-modified-tick))
       boris-company-data))))

(defun boris--get-annotation (candidate property)
  (plist-get (boris--get-annotations candidate) property))

(defun boris--get-annotations (candidate)
  (let ((annotations (plist-get (boris--company-data) :annotations)))
    (and annotations
         (gethash candidate annotations))))

(defun boris--parse-company-data (data offset)
  (cl-loop
   for (key value) on data by #'cddr
   append
   (list key
         (cl-case key
           (:annotations (boris--plist-to-hash-table value))
           ((:start :end) (+ value offset))
           (t value)))))

(defun boris--plist-to-hash-table (plist)
  (let ((table (make-hash-table :test 'equal)))
    (cl-loop for (key value) on plist by #'cddr
             do
             (let ((key-string (substring (symbol-name key) 1)))
               (puthash key-string value table)))
    table))


;;;; Apropos

(define-derived-mode boris-apropos-mode special-mode
  "Boris Apropos"
  "Major mode for Boris symbol lookup buffers.")

(defvar boris-apropos-mode-map
  (make-composed-keymap button-buffer-map special-mode-map))

(defvar boris-apropos-history nil)

(defun boris-apropos (regexp)
  (interactive
   (list
    (boris--read-symbol "Search for PHP symbol (PCRE regexp or word list)")))
  (when (string-match-p (rx (syntax whitespace)) regexp)
    (setq regexp (split-string regexp)))
  (let ((tags
         (boris-call (list :operation :apropos
                           :regexp regexp))))
    (boris--show-tags-buffer tags)))

(defun boris-who-implements (interface)
  (interactive
   (list
    (boris--read-symbol "List classes that implement interface" 'interface)))
  (boris--show-tags-buffer
   (boris-call (list :operation :whoimplements
                     :interface interface))))

(defun boris-who-uses (trait)
  (interactive
   (list
    (boris--read-symbol "List classes that use trait" 'trait)))
  (boris--show-tags-buffer
   (boris-call (list :operation :whouses
                     :trait trait))))

(defun boris-who-extends (class)
  (interactive
   (list
    (boris--read-symbol "List classes that extend class" 'class)))
  (boris--show-tags-buffer
   (boris-call (list :operation :whoextends
                     :class class))))

(defun boris--read-symbol (prompt &optional kind)
  (let* ((default (thing-at-point 'symbol))
         (prompt (if default
                     (format "%s (default `%s'): " prompt default)
                   (format "%s: " prompt))))
    (if kind
        (completing-read prompt
         (boris-call (list :operation :completesymbol
                           :kind kind
                           :prefix ""
                           :annotate nil))
         nil nil nil boris-apropos-history default)
      (read-string prompt nil boris-apropos-history default))))

(defun boris--show-tags-buffer (tags)
  (if (zerop (length tags))
      (message "No matching symbols")
    (with-current-buffer (get-buffer-create "*boris apropos*")
      (boris-apropos-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion
          (dolist (tag tags)
            (boris-insert-tag tag)
            (newline)))
        (pop-to-buffer (current-buffer))))))

(defun boris-insert-tag (tag)
  (cl-destructuring-bind (&key kind name file line description arguments defined_in
                            definitions &allow-other-keys)
      tag
    (cl-flet ((insert-xref (label file line)
                (if (and file line)
                    (insert-text-button
                     label
                     'type 'boris-apropos
                     'boris-file file
                     'boris-line line)
                  (insert label))))
      (let ((label (format "%s %s%s" kind name (or arguments ""))))
        (insert-xref label file line)
        (newline))
      (when description
        (insert (format "  %s\n" description)))
      (when defined_in
        (insert (format "  defined in %s\n" defined_in)))
      (when definitions
        (insert (format "  defined in "))
        (dolist (definition definitions)
          (cl-destructuring-bind (&key file line defined_in &allow-other-keys)
              definition
            (insert-xref defined_in file line))
          (insert ", "))
        ;; hack
        (delete-region (- (point) 2) (point))
        (newline)))))

(define-button-type 'boris-apropos
    'help-echo "mouse-2, RET: Visit definition"
    'follow-link t
    'action 'boris-apropos-follow-link)

(defun boris-apropos-follow-link (button)
  (let ((file (button-get button 'boris-file))
        (line (button-get button 'boris-line)))
    (with-current-buffer (find-file file)
      (widen)
      (goto-char (point-min))
      (forward-line (1- line)))))


;;;; Display class hierarchy
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defun boris-class-hierarchy ()
  (interactive)
  (let ((tags
         (boris-call (list :operation :completesymbol
                           :prefix ""
                           :kind 'class
                           :annotate t)))) 
    (boris--display-hierarchy tags)))

(defun boris--display-hierarchy (tags)
  (interactive)
  (switch-to-buffer "*Boris Classes*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (dolist (tag (boris--tags-hierarchy tags))
    (apply 'widget-create
           (boris--tag-to-widget tag)))
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(defun boris--tags-hierarchy (tags)
  (let ((child-index (make-hash-table :test 'equal)))
    (dolist (tag tags)
      (let ((parent-name (plist-get tag :parent)))
        (push tag (gethash parent-name child-index))))
    
    (cl-labels ((tag-with-children (tag)
                  (let* ((tag-name (plist-get tag :name))
                         (children
                          (mapcar #'tag-with-children
                                  (gethash tag-name child-index))))
                    (plist-put tag :children children))))
      (mapcar #'tag-with-children
              (gethash nil child-index)))))

(defun boris--tag-to-widget (tag)
  (cl-destructuring-bind (&key name children description file line &allow-other-keys)
      tag
    `(tree-widget
      :tag ,name
      :open t
      ,@(mapcar #'boris--tag-to-widget children))))



;;;###autoload
(defun boris-setup-compilation-mode ()
  (add-to-list 'compilation-error-regexp-alist-alist
               '(boris-php-backtrace
                 "^PHP.* \\(/[^:]+\\):\\([0-9]+\\)" 1 2 nil nil))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(xdebug-error
                 "^\\(?:Error\\|Warning\\|Notice\\): .* in \\(.*\\) on line \\([0-9]+\\)$" 1 2 nil nil))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(xdebug-stacktrace
                 "\\([^ :]+\\):\\([0-9]+\\)+$" 1 2 nil nil)))

;;;###autoload
(eval-after-load 'php-mode
  '(add-hook 'php-mode-hook 'boris-minor-mode))

;;;###autoload
(eval-after-load 'compile '(boris-setup-compilation-mode))

;;; boris-completion.el ends here
