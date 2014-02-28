;;; boris-completion.el --- context-sensitive completion hack for boris repl

;; Copyright (C) 2013-2014 joddie <jonxfield@gmail.com>

;; Author: joddie
;; Version: 0.32
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
(defvar boris-connect-timer nil)

(defvar boris-response nil)
(defvar boris-response-flag nil)
(defvar boris-async-callback nil)

(defvar boris-original-eldoc-function nil)

(defconst boris-request-format
  '((:length long)
    (:data str (:length))))

(defconst boris-response-format
  '((:code str 1)
    (:length long)
    (:data str (:length))))

(defvar boris-timeout 0.5)

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
        (message "Connecting to Boris on port %d... done." boris-port))
    (file-error
     (let ((details (caddr error-data)))
       (message "Connecting to Boris on port %d failed: %s" boris-port details)
       (kill-buffer boris-buffer)
       (setq boris-process nil
             boris-buffer nil)))))

(defun boris-connected-p ()
  (and boris-process (process-live-p boris-process)))

(defun boris-comint-running-p ()
  (and boris-comint-process (process-live-p boris-comint-process)))

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
  (setq boris-response nil
        boris-response-flag nil)
  (process-send-string boris-process (boris-pack-request data))
  (if callback
      (setq boris-async-callback callback)
    (accept-process-output boris-process boris-timeout)
    (if boris-response-flag
        boris-response
      (message "Boris response timeout.")
      nil)))

(defvar boris-response-code-regexp
  (rx-to-string '(any 0 1 2 3 4)))

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

      (let ((unpacked
             (condition-case err
                 (bindat-unpack boris-response-format 
                                (buffer-substring (point-min) (process-mark proc)))
               (args-out-of-range nil))))
        (when unpacked
          (let* ((json-object-type 'hash-table)
                 (json-array-type 'list)
                 (response
                  (json-read-from-string (bindat-get-field unpacked :data)))
                 (read-chars
                  (bindat-length boris-response-format unpacked)))
            (delete-region (point-min) (+ (point-min) read-chars))
            (if boris-async-callback
                (progn
                  (funcall boris-async-callback response)
                  (setq boris-async-callback nil))
              (setq boris-response response
                    boris-response-flag t))))))))

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
           (response (boris-call `((operation . complete)
                                   (line . ,line)
                                   (evaluate . ,evaluate-p)))))
      (when (and response (gethash "completions" response))
        ;; Display a message only in php-mode buffers, not in the
        ;; boris-repl buffer (which would be redundant)
        (unless (eq major-mode 'boris-mode)
          (message "Completing using Boris REPL"))
        (list
         (+ (point-at-bol) (gethash "start" response))
         (+ (point-at-bol) (gethash "end" response))
         (gethash "completions" response))))))

;;;###autoload
(defun boris-eldoc-function ()
  (or (and (functionp boris-original-eldoc-function)
           (funcall boris-original-eldoc-function))
      (when (boris-connected-p)
        (let ((line (buffer-substring-no-properties (point-at-bol) (point)))
              (evaluate-p (eq major-mode 'boris-mode)))
          (boris-call `((operation . hint)
                        (line . ,line)
                        (evaluate . ,evaluate-p))
                      #'boris-eldoc-callback)
          nil))))

(defun boris-eldoc-callback (response)
  (message "%s" response))

;;;###autoload
(defun boris-get-documentation ()
  (interactive)
  (let* ((line (buffer-substring-no-properties (point-at-bol) (point)))
         (evaluate-p (eq major-mode 'boris-mode))
         (docs (boris-call
                `((operation . documentation)
                  (line . ,line)
                  (evaluate . ,evaluate-p)))))
    (when docs
      (with-help-window "*boris help*"
        (princ docs))
      (with-current-buffer "*boris help*"
        (let ((inhibit-read-only t)
              (read-only-mode nil))
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

;; Hacks to php-mode

;;;###autoload
(defun boris-setup-php-mode ()
  (define-key php-mode-map (kbd "C-c C-z") 'boris-open-or-pop-to-repl)
  (define-key php-mode-map (kbd "C-c d")   'boris-get-documentation)
  (define-key php-mode-map (kbd "C-c C-/") 'boris-get-documentation)
  (define-key php-mode-map (kbd "C-c C-k") 'boris-load-file)
  (add-hook 'php-mode-hook 'boris-php-mode-hook))

(defun boris-open-or-pop-to-repl ()
  (interactive)
  (if (boris-comint-running-p)
      (pop-to-buffer (process-buffer boris-comint-process))
    (boris)))

(defun boris-php-mode-hook ()
  (setq boris-original-eldoc-function eldoc-documentation-function)
  (set (make-local-variable 'eldoc-documentation-function)
       'boris-eldoc-function)
  (eldoc-mode +1)
  (eldoc-add-command 'completion-at-point)
  (add-hook 'completion-at-point-functions 'boris-completion-at-point nil t)

  (if (boris-connected-p)
      (message "Connected to Boris REPL.")
    (message "Use M-x boris to start Boris REPL.")))

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



;;;; A very simple comint-mode

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

  (set (make-local-variable 'eldoc-documentation-function)
       'boris-eldoc-function)
  (eldoc-add-command 'completion-at-point)
  (eldoc-add-command 'comint-dynamic-complete)
  (eldoc-mode +1))

(defvar boris-mode-syntax-table php-mode-syntax-table)

(define-key boris-mode-map (kbd "TAB")
  (if (< emacs-major-version 24)
      'comint-dynamic-complete
    'completion-at-point))
(define-key boris-mode-map (kbd "C-c C-d") 'boris-get-documentation)
(define-key boris-mode-map (kbd "C-c d")   'boris-get-documentation)
(define-key boris-mode-map (kbd "C-c C-/") 'boris-get-documentation)
(define-key boris-mode-map (kbd "C-c C-z") 'boris)

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
  "Run Boris REPL with network hacks for completion and Eldoc.

The exact command to run is determined by the variables
`boris-command' and `boris-args'."
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
                      "ssh -t %h -L %p:localhost:%p boris --listen"
                      `((?h . ,host) (?p . ,port)))
                     'boris-remote-command-history)))
    (boris command)))

(defun boris-query-kill-if-running ()
  (when (boris-comint-running-p)
    (if (yes-or-no-p "Boris already running. Kill process and start new REPL?")
        (progn
          (delete-process boris-comint-process)
          (when (boris-connected-p)
            (delete-process boris-process)))
      (error "Boris already running."))))

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
