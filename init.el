;;; init.el --- My Emacs configuration

;;; Commentary:

;; Following lines load an Org file and build the configuration code out of it.

;;; Code:

(let ((gc-cons-threshold most-positive-fixnum))

  ;; Set repositories
  (require 'package)
  (setq-default
   load-prefer-newer t
   package-enable-at-startup nil)

  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  (package-initialize)

  ;; bootstrap use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)

  ;; install missing packages automatically
  (when (not package-archive-contents)
    (package-refresh-contents))

  (when (not (package-installed-p 'use-package))
    (package-install 'use-package))

  ;; Use latest Org
  (use-package org :ensure org-plus-contrib)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
  (garbage-collect))

;;; init.el ends here
