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

  ;; Silence compiler warnings for deprecated packages
  (setq byte-compile-warnings '(cl-functions))

  ;; In noninteractive sessions, prioritize non-byte-compiled source files to
  ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
  ;; to skip the mtime checks on every *.elc file.
  (setq load-prefer-newer noninteractive)

  ;; Set repositories
  (require 'package)
  (setq-default
   load-prefer-newer t
   package-enable-at-startup nil)

  (setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                           ("gnu"    . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("elpa"   . "https://elpa.gnu.org/packages/")))

  ;; Bootstrap straight.el
  (setq straight-base-dir (expand-file-name "var" user-emacs-directory))
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "var/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Make use-package use straight.el instead of package.el
  (straight-use-package 'use-package)

  ;; Prevent package.el loading packages prior to their init-file loading.
  (setq package-enable-at-startup nil)

  ;; Install org-mode
  (straight-use-package 'org)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

  ;; Tangle user configuration
  (let ((userconfig-location (expand-file-name "userconfig.org" user-emacs-directory)))
    (when (file-exists-p userconfig-location)
      (org-babel-load-file userconfig-location)))

  (garbage-collect))

;;; init.el ends here
