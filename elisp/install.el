;; Simple install script for boris Emacs support

(package-initialize)
(when (package-installed-p 'boris-completion)
  (package-delete "boris-completion"
                  (package-version-join
                   (pkg-info-package-version 'boris-completion))))

(with-temp-buffer
  (find-file "boris-completion.el")
  (package-install-from-buffer (package-buffer-info) 'single))



