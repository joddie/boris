;; boris-completion.el -- context-sensitive completion hack for boris repl

;; Copyright (C) 2012-2013 joddie <jonxfield@gmail.com>

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


(require 'php-boris)
(require 'bindat)
(require 'json)


(defvar boris-connection nil)

(defvar boris-point-at-bol nil)

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

(defun boris-connect ()
  (interactive)
  (let ((progress (make-progress-reporter "Connecting to Boris on port 8015...")))
    (when boris-process (delete-process boris-process))
    (when boris-buffer (kill-buffer boris-buffer))
    (setq boris-buffer (get-buffer-create "*boris connection*"))
    (with-current-buffer boris-buffer (set-buffer-multibyte nil))
    (setq boris-process
          (open-network-stream
           "boris" boris-buffer "127.0.0.1" 8015))
    (set-process-coding-system boris-process 'raw-text 'raw-text)
    (set-process-query-on-exit-flag boris-process nil)
    (set-process-filter boris-process 'boris-filter)
    (with-current-buffer boris-buffer
      (setq boris-marker (copy-marker (point-min))))
    (progress-reporter-done progress)))

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

(defun boris-completion-at-point ()
  (let* ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (response (boris-call `((operation . complete) (line . ,line)))))
    (when (and response (gethash "completions" response))
      (list
       (+ (point-at-bol) (gethash "start" response))
       (+ (point-at-bol) (gethash "end" response))
       (gethash "completions" response)))))


;; hack -- redefine `php-boris' to pass the 'listen' command line flag
(eval-after-load 'php-boris 
  '(defun php-boris ()
    "Run boris REPL (Hacked version to demo completion code)."
    (interactive)
    (setq php-boris-prompt-re
          (format php-boris-prompt-re-format php-boris-prompt php-boris-prompt))
    (switch-to-buffer-other-window
     (apply 'make-comint php-boris-process-name php-boris-command nil
            `("-l" "-e" ,(format php-boris-code (window-width) php-boris-prompt))))
    (php-boris-mode)
    (setq completion-at-point-functions '(boris-completion-at-point))))
