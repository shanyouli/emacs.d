;;; init-eshell.el --- Initialize Eshell Configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  lye li

;; Author: lye li <shanyouli6@gamil.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package awsehell
  :ensure nil
  :quelpa (awseshell :fetcher github :repo "manateelazycat/aweshell")
  )

(defun lye/eshell ()
  (interactive)
  (require 'aweshell)
  (eshell))

(defalias 'eshell 'lye/eshell)




(provide 'init-eshell)
;;; init-eshell.el ends here

