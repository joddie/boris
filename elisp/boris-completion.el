;;; boris-completion.el --- context-sensitive completion hack for boris repl

;; Copyright (C) 2013 joddie <jonxfield@gmail.com>

;; Author: joddie
;; Version: 0.2
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
(require 'php-boris)
(require 'bindat)
(require 'json)

(defvar boris-process nil)
(defvar boris-buffer nil)

(defvar boris-marker nil)
(defvar boris-response nil)

(defconst boris-request-format
  '((:length long)
    (:data str (:length))))

(defconst boris-response-format
  '((:code str 1)
    (:length long)
    (:data str (:length))))

;;;###autoload
(defun boris-connect ()
  (interactive)
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
  (with-current-buffer boris-buffer
    (setq boris-marker (copy-marker (point-min))))
  (message "Connecting to Boris on port 8015... done."))

(defun boris-call (data)
  (unless (and boris-process (process-live-p boris-process))
    (boris-connect))
  (setq boris-response nil)
  (process-send-string boris-process (boris-pack-request data))
  (let ((timeout 2.0) (start-time (float-time)))
    (while (and (not boris-response)
                (< (- (float-time) start-time) timeout))
      (accept-process-output boris-process 0 100 t)))
  boris-response)

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

      (let ((unpacked
             (condition-case err
                 (bindat-unpack boris-response-format 
                                (buffer-substring boris-marker (process-mark proc)))
               (args-out-of-range nil))))
        (when unpacked
          (let* ((json-object-type 'hash-table)
                 (json-array-type 'list)
                 (response
                  (json-read-from-string (bindat-get-field unpacked :data)))
                 (read-chars
                  (bindat-length boris-response-format unpacked)))
            (delete-region boris-marker (+ boris-marker read-chars))
            (setq boris-response response)))))))

(defun boris-pack-request (data)
  (let* ((json (json-encode data))
         (len (length json)))
    (bindat-pack boris-request-format
                 `((:length . ,len)
                   (:data . ,json)))))

;;;###autoload
(defun boris-completion-at-point ()
  (let* ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (response (boris-call `((operation . complete) (line . ,line)))))
    (when (and response (gethash "completions" response))
      (list
       (+ (point-at-bol) (gethash "start" response))
       (+ (point-at-bol) (gethash "end" response))
       (gethash "completions" response)))))

;;;###autoload
(defun boris-eldoc-function ()
  (or (boris-get-eldoc (boris-symbol-at-point))
      (save-excursion
        (search-backward "(" (point-at-bol) t)
        (boris-get-eldoc (boris-symbol-at-point)))))

(defvar boris-namespace-syntax-table
  (let ((table (copy-syntax-table php-mode-syntax-table)))
    (modify-syntax-entry ?\\ "_" table)
    table))

(defun boris-symbol-at-point ()
  (save-excursion
    (skip-syntax-backward "-")
    (with-syntax-table boris-namespace-syntax-table
      (thing-at-point 'symbol))))

(defun boris-get-eldoc (symbol)
  (when symbol
    (let ((response (boris-call `((operation . hint) (what . ,symbol)))))
      (and response (gethash "hint" response)))))

(defun boris-get-documentation ()
  (interactive)
  (let* ((symbol (boris-symbol-at-point))
         (docs (boris-call `((operation . documentation)
                             (what . ,symbol)))))
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
    (goto-line line-number)))

;; hack -- redefine `php-boris' to pass the 'listen' command line flag

;;;###autoload
(eval-after-load 'php-boris 
  '(progn
    (defcustom php-boris-args '("-l")
      "command-line arguments for boris"
      :group 'php-boris
      :type '(repeat string))
    
    (defun php-boris ()
      "Run boris REPL (Hacked version to demo completion code)."
      (interactive)
      (setq php-boris-prompt-re
            (format php-boris-prompt-re-format php-boris-prompt php-boris-prompt))
      (pop-to-buffer
       (apply 'make-comint php-boris-process-name php-boris-command nil
              php-boris-args))
      (php-boris-mode))

    (add-hook 'php-boris-mode-hook
     (lambda ()
       (when (< emacs-major-version 24)
         (setq comint-dynamic-complete-functions '(completion-at-point)))
       (setq completion-at-point-functions '(boris-completion-at-point))
       (setq eldoc-documentation-function 'boris-eldoc-function)
       (eldoc-add-command 'completion-at-point)
       (eldoc-add-command 'comint-dynamic-complete)
       (eldoc-mode +1)))

           (define-key php-boris-mode-map (kbd "C-c C-d") 'boris-get-documentation)
    (define-key php-boris-mode-map (kbd "C-c C-z") 'php-boris)

    (eval-after-load 'php-mode
      '(define-key php-mode-map (kbd "C-c C-z")
        (lambda ()
          (interactive)
          (if (and (buffer-live-p "*boris-repl*")
                   (get-buffer-process "*boris-repl*")
                   (process-live-p (get-buffer-process "*boris-repl*")))
              (pop-to-buffer "*boris-repl*")
            (php-boris)))))))

;;; boris-completion.el ends here
