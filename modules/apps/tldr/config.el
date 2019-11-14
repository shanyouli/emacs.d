;;; tldr/config.el -*- lexical-binding: t -*-
(require 'tldr)
(setq-default tldr-directory-path (expand-file-name "tldr/"
                                                    lye-emacs-cache-dir)
              ;; request storage directory
              request-storage-directory (expand-file-name "request"
                                                          lye-emacs-cache-dir))
