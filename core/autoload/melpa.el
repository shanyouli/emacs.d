;;; core/autoload/melpa.el -*- lexical-binding: t -*-


;;;###autoload
(defun the-fastest-elpa-mirror ()
  (interactive)
  (require 'chart)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https"))
         (urls
          (mapcar
           (lambda (part)
             (concat proto "://" part "archive-contents"))
           '("melpa.org/packages/"
             "www.mirrorservice.org/sites/melpa.org/packages/"
             "emacs-china.org/melpa/"
             "mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"
             "mirrors.163.com/elpa/melpa/"
             "mirrors.cloud.tencent.com/elpa/melpa/"
             )))
         (durations
          (mapcar
           (lambda (url)
             (let ((start (current-time)))
               (message "Fetching %s" url)
               (call-process "curl" nil nil nil "--max-time" "60" url)
               (float-time (time-subtract (current-time) start))))
           urls)))
    (chart-bar-quickie
     'horizontal
     "The fastest elpa mirror"
     (mapcar (lambda (url) (url-host (url-generic-parse-url url))) urls) "Elpa"
     (mapcar (lambda (d) (* 1e3 d)) durations) "ms")
    (message "%s" durations)))
