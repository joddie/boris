;;; boris-completion.el --- context-sensitive completion hack for boris repl

;; Copyright (C) 2013 joddie <jonxfield@gmail.com>

;; Author: joddie
;; Version: 0.29
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

;;;###autoload
(defcustom boris-command "boris"
  "boris program"
  :group 'boris
  :type 'string)

;;;###autoload
(defcustom boris-args '("-l")
  "command-line arguments for boris"
  :group 'boris
  :type '(repeat string))


(defvar boris-process-name "boris")
(defvar boris-process nil)
(defvar boris-buffer nil)

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
  
  (unless (process-live-p boris-process-name)
    (message "Starting Boris...")
    (save-window-excursion (boris))
    (message "Done.")
    (sleep-for 0.1))

  (message "Connecting to Boris on port 8015...")
  (when boris-process (delete-process boris-process))
  (when boris-buffer (kill-buffer boris-buffer))
  (setq boris-buffer (get-buffer-create " *boris connection*"))
  (with-current-buffer boris-buffer (set-buffer-multibyte nil))
  (setq boris-process
        (open-network-stream
         "boris" boris-buffer "127.0.0.1" 8015))
  (set-process-coding-system boris-process 'binary 'binary)
  (set-process-query-on-exit-flag boris-process nil)
  (set-process-filter boris-process 'boris-filter)
  (message "Connecting to Boris on port 8015... done."))

(defun boris-connected-p ()
  (and boris-process (process-live-p boris-process)))

(defun boris-prompt-for-connection ()
  (if (boris-connected-p)
      t
    (let ((comint-process (get-process boris-process-name)))
      (if (and comint-process (process-live-p comint-process))
          (if (y-or-n-p "Connect to Boris REPL?")
              (boris-connect)
            (message "Use M-x boris-connect to connect."))
        (if (y-or-n-p "Start Boris REPL?")
            (boris)
          (message "Use M-x boris to start."))))
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
  (define-key php-mode-map (kbd "C-c C-z")
    'boris-open-or-pop-to-repl)
  (define-key php-mode-map (kbd "C-c d") 'boris-get-documentation)
  (define-key php-mode-map (kbd "C-c C-/") 'boris-get-documentation)
  (define-key php-mode-map (kbd "C-c C-k") 'boris-load-file)
  (add-hook 'php-mode-hook 'boris-php-mode-hook))

(defun boris-open-or-pop-to-repl ()
  (interactive)
  (if (process-live-p boris-process-name)
      (pop-to-buffer (process-buffer
                      (get-process boris-process-name)))
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
  (unless (process-live-p boris-process-name)
    (save-window-excursion
      (boris)))
  (let ((process (get-process boris-process-name)))
    (comint-send-string process
                        (format "require '%s';\n"
                                (replace-regexp-in-string "'" "\\'" file-name)))
    (pop-to-buffer (process-buffer process))))



;;;; A very simple comint-mode

;;;###autoload
(define-derived-mode boris-mode comint-mode "boris REPL"
  "Less-broken major mode for boris php REPL"
  :syntax-table boris-mode-syntax-table
  ;; (set (make-local-variable 'font-lock-defaults) '(nil nil t))

  ;; delq seems to change global variables if called this phase
  (set (make-local-variable 'comint-dynamic-complete-functions)
       (delete 'comint-dynamic-complete-filename comint-dynamic-complete-functions))

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

(defvar boris-mode-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    (modify-syntax-entry ?$ "_" st)
    st))

(defvar boris-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB")     'comint-dynamic-complete)
    (define-key map (kbd "C-c C-d") 'boris-get-documentation)
    (define-key map (kbd "C-c d")   'boris-get-documentation)
    (define-key map (kbd "C-c C-/") 'boris-get-documentation)
    (define-key map (kbd "C-c C-z") 'boris)
    map))

(defun boris ()
  "Run Boris REPL with network hacks for completion and Eldoc.

The exact command to run is determined by the variables
`boris-command' and `boris-args'."
  (interactive)
  (pop-to-buffer
   (apply 'make-comint boris-process-name boris-command nil
          boris-args))
  (boris-mode))

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
