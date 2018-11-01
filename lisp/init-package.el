
;;init-package.el
(require 'package)
(setq package-archives
      '( ;;官方
	("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (if (or no-refresh (assoc package package-archive-contents))
          (package-install package)
        (progn
          (package-refresh-contents)
          (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))
(setq package-enable-at-startup nil)
(package-initialize)

;; Using quelpa to Install some packages
(require-package 'quelpa)
(setq quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil)

(provide 'init-package)
