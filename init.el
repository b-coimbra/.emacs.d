;;; init.el --- My Emacs configuration

;;; Commentary:

;; Following lines load an Org file and build the configuration code out of it.

;;; Code:

(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6))

  ;; Silence compiler warnings as they can be pretty disruptive
  (if (boundp 'comp-deferred-compilation)
      (setq comp-deferred-compilation nil)
    (setq native-comp-deferred-compilation nil))
  ;; In noninteractive sessions, prioritize non-byte-compiled source files to
  ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
  ;; to skip the mtime checks on every *.elc file.
  (setq load-prefer-newer noninteractive)

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
