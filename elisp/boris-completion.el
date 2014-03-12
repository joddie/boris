;;; boris-completion.el --- context-sensitive completion hack for boris repl -*- lexical-binding: t -*-

;; Copyright (C) 2013-2014 joddie <jonxfield@gmail.com>

;; Author: joddie
;; Version: 0.35
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
  (declare-function company-begin-backend "company.el")
  (declare-function company-doc-buffer "company.el")
  (defvar company-backends))

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

(defun boris-prompt-for-connection ()
  (if (boris-connected-p)
      t
    (if (boris-comint-running-p)
        (if (y-or-n-p "Connect to Boris REPL?")
            (boris-connect)
          (message "Use M-x boris-connect to connect."))
      (if (y-or-n-p "Start Boris REPL?")
          (boris)
        (message "Use M-x boris to start.")))
    (boris-connected-p)))

(defun boris-call (data &optional callback)
  (process-send-string boris-process (boris-pack-request data))
  (if callback
      ;; Handle asynchronously -- mostly for ElDoc
      (add-hook 'boris-async-callbacks callback t)
    ;; Wrap with synchronous waiting and timeout
    (let ((response nil) 
          (success nil)
          (timed-out nil))
      ;; Success callback
      (add-hook 'boris-async-callbacks
                (lambda (received)
                  (if (not timed-out)
                      (setq response received
                            success t)
                    (message "Discarded message handled after timeout."))) t)
      ;; Timeout callback
      (let ((timer
             (run-with-timer boris-hard-timeout nil
                             (lambda () (setq timed-out t))))
            (message "Waiting for Boris response.."))
        ;; Wait...
        (while (not (or success timed-out))
          (accept-process-output boris-process boris-timeout)
          (unless success
            (setq message (concat message "."))
            (message "%s" message)))
        (if success
            (progn
              (cancel-timer timer)
              response)
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
  (when (boris-prompt-for-connection)
    (let* ((line (buffer-substring-no-properties (point-at-bol) (point)))
           (evaluate-p (eq major-mode 'boris-mode))
           (response
            (boris-call (list :operation :complete
                              :line line
                              :evaluate evaluate-p))))
      (cl-destructuring-bind (&key start end completions) response
        (when completions
          ;; Display a message only in php-mode buffers, not in the
          ;; boris-repl buffer (which would be redundant)
          (unless (eq major-mode 'boris-mode)
            (message "Completing using Boris REPL"))
          (list
           (+ (point-at-bol) start) (+ (point-at-bol) end)
           completions))))))

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
  (let* ((start
          (save-excursion
            (ignore-errors
              (backward-up-list))
            (point-at-bol)))
         (text
          (buffer-substring-no-properties start (point)))
         (evaluate-p (eq major-mode 'boris-mode)))
    (boris-call (list :operation operation
                      :line text
                      :evaluate evaluate-p)
                callback)))


;;;; Setup php-mode
;; TODO: Make a minor mode.

;;;###autoload
(defun boris-setup-php-mode ()
  (define-key php-mode-map (kbd "C-c C-z") 'boris-open-or-pop-to-repl)
  (define-key php-mode-map (kbd "C-c d")   'boris-get-documentation)
  (define-key php-mode-map (kbd "C-c C-/") 'boris-get-documentation)
  (define-key php-mode-map (kbd "C-c C-k") 'boris-load-file)
  (add-hook 'php-mode-hook 'boris-php-mode-hook))

(defun boris-open-or-pop-to-repl ()
  (interactive)
  (setq boris-recent-buffer (current-buffer))
  (if (boris-comint-running-p)
      (pop-to-buffer (process-buffer boris-comint-process))
    ;; call-interactively passes along any prefix argument
    (call-interactively #'boris)))

(defun boris-php-mode-hook ()
  (setq boris-original-eldoc-function eldoc-documentation-function)
  (set (make-local-variable 'eldoc-documentation-function)
       'boris-eldoc-function)
  (eldoc-mode +1)
  (eldoc-add-command 'completion-at-point)
  (add-hook 'completion-at-point-functions 'boris-completion-at-point nil t)

  (if (boris-connected-p)
      (message "Connected to Boris REPL.")
    (message "Use M-x boris to start Boris REPL."))

  (when (require 'company nil t)
    (make-local-variable 'company-backends)
    (push #'boris-company company-backends)
    (company-mode 1))

  (setq-local mode-line-process '(:eval (boris-mode-line-process))))

(defun boris-mode-line-process ()
  (cond ((boris-connected-p)
         (propertize "+boris"
                     'face 'boris-mode-line-connected
                     'help-echo "Connected to running REPL."))
        ((boris-comint-running-p)
         (propertize "-boris" 'face 'boris-mode-line-run
                     'help-echo "REPL running but not connected."))
        (t
         (propertize " boris" 'face 'boris-mode-line-disconnected
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
    (company-mode 1)))

(defvar boris-mode-syntax-table php-mode-syntax-table)

(define-key boris-mode-map (kbd "TAB")
  (if (< emacs-major-version 24)
      'comint-dynamic-complete
    'completion-at-point))
(define-key boris-mode-map (kbd "C-c C-d") 'boris-get-documentation)
(define-key boris-mode-map (kbd "C-c d")   'boris-get-documentation)
(define-key boris-mode-map (kbd "C-c C-/") 'boris-get-documentation)
(define-key boris-mode-map (kbd "C-c C-z") 'boris-restart-or-pop-back)

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
            (let ((comint-buffer
                   (apply 'make-comint boris-comint-process-name
                          boris-command nil boris-args)))
              (with-current-buffer comint-buffer (boris-mode))
              (setq boris-comint-process-buffer comint-buffer))

          ;; Split custom command and run it
          (let* ((words (split-string command))
                 (program (car words))
                 (arguments (cdr words))
                 (comint-buffer
                  (apply 'make-comint boris-comint-process-name
                         program nil arguments)))
            (with-current-buffer comint-buffer
              (boris-mode)
              ;; Save the command and arguments as local variables, so
              ;; that restarting the REPL via C-c C-z in this buffer
              ;; will re-run the same command
              (setq-local boris-command program)
              (setq-local boris-args arguments))
            (setq boris-comint-process-buffer comint-buffer)))

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
                               (message "Connection timeout. Try M-x boris-connect."))))
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

(defun boris-query-kill-if-running ()
  (when (boris-comint-running-p)
    (if (yes-or-no-p "Boris already running. Kill process and start new REPL?")
        (progn
          (delete-process boris-comint-process)
          (when (boris-connected-p)
            (delete-process boris-process)))
      (error "Boris already running."))))


;;; Company-mode integration

;; FIXME
(defvar boris-company-data nil)
(defvar boris-company-annotations nil)
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
       (cl-destructuring-bind (&key completions &allow-other-keys)
           (boris--company-data)
         completions))

      (meta
       (and boris-company-annotations
            (cl-destructuring-bind (&key description &allow-other-keys)
                (gethash arg boris-company-annotations)
              (or description ""))))

      (annotation
       (and boris-company-annotations
            (cl-destructuring-bind (&key arguments &allow-other-keys)
                (gethash arg boris-company-annotations)
              (or arguments ""))))

      (location
       (and boris-company-annotations
            (cl-destructuring-bind (&key file line &allow-other-keys)
                (gethash arg boris-company-annotations)
              (cons file line))))

      (doc-buffer
       (company-with-candidate-inserted arg
         (company-doc-buffer (boris--call-for-info :documentation))))

      (post-completion
       (funcall eldoc-documentation-function))

      (require-match nil)

      (t nil))))


(defun boris--company-data ()
  (if (and (equal (point) boris-company-last-point)
           (equal (buffer-modified-tick) boris-company-last-tick))
      ;; Return the cached data
      boris-company-data
    ;; TODO: Refactor to use boris--call-for-info
    (let* ((beginning-of-line 
            (save-excursion
              (ignore-errors
                (backward-up-list))
              (point-at-bol)))
           (text (buffer-substring-no-properties beginning-of-line (point)))
           (evaluate-p (eq major-mode 'boris-mode)))
      (if (equal text "")
          ;; Hack: avoid calling with an empty string, which returns a
          ;; huge amount of data.  Need a more robust fix for this...
          (setq boris-company-data nil
                boris-company-annotations nil) 
        (let ((response 
               (boris-call (list :operation :annotate
                                 :line text
                                 :evaluate evaluate-p))))
          (setq boris-company-data
                (boris--parse-company-data response beginning-of-line))
          (setq boris-company-annotations
                (plist-get boris-company-data :annotations)))

        (setq boris-company-last-point (point)
              boris-company-last-tick (buffer-modified-tick))
        boris-company-data))))

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



;;;###autoload
(defun boris-setup-compilation-mode ()
  (add-to-list 'compilation-error-regexp-alist-alist
               '(boris-php-backtrace
                 "^PHP.* \\(/[^:]+\\):\\([0-9]+\\)" 1 2 nil nil))
  (add-to-list 'compilation-error-regexp-alist 'boris-php-backtrace))

;;;###autoload
(eval-after-load 'php-mode '(boris-setup-php-mode))

;;;###autoload
(eval-after-load 'compile '(boris-setup-compilation-mode))

;;; boris-completion.el ends here
