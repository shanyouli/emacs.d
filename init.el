;;; init.el --- Initialize startup  -*- lexical-binding: t -*-

;; Author: shanyouli
;; Maintainer: shanyouli
;; Version: v0.2
;; Homepage: https://github.com/shanyouli/emacs.d
;; Keywords: init


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Initialize startup

;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later with
;; `lye-restore-garbage-collection-h'. Not resetting it will cause
;; stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; In noninteractive sessions,prioritize non-byte-compiled source files to
;; prevent stable, byte-compiled code from running. However, if you're getting
;; recursive load errors, it may help to set this to nil.
(setq load-prefer-newer noninteractive)

(let (file-name-handler-alist)
  ;; Ensure Lye-Emacs is running out of this file's directory
  (when load-file-name
    (setq user-emacs-directory (file-name-directory load-file-name)))

  (cond ((version< emacs-version "27.0")
         (load (concat user-emacs-directory "early-init") nil 'nomessage))
        ((version< emacs-version "25.3")
         (error "Detected Emacs %s. Lye-emacs only supports Emacs 25.3 and higher."
                emacs-version))))

;; Start Time Test
(load (concat user-emacs-directory "core/core-benchmark") nil 'nomessage)
;; Load the heart of Lye-Emacs
(load (concat user-emacs-directory "core/core") nil 'nomessage)

;; Let 'er rip!
(lye-core-initialize)

(lye/modules-require 'md-edit)          ; better edit
(lye/modules-require 'md-shackle)       ; Window rule
(lye/modules-require 'md-dired)         ; Dired
(lye/modules-require 'md-reads)         ; Reader tools
(lye/modules-require 'md-company)       ; company

(run-with-idle-timer! :defer 0.5
  ;; Program language common tool
  (lye/modules-require 'md-lang)
  (lye/modules-require 'md-elisp)
  (lye/modules-require 'md-scheme)
  (lye/modules-require 'md-sh)
  (lye/modules-require 'md-python)
  (lye/modules-require 'md-lua))

;; Org mode
(run-with-idle-timer! :defer 1
  (lye/modules-require 'md-hugo)
  (lye/modules-require 'md-org))
