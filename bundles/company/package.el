;;; bundles/company/package.el -*- lexical-binding: t -*-

(package! company :commands global-company-mode)
(package! company-box :commands company-box-mode)
(package! company-tabnine :if (not IS-WINDOWS))
(package! company-quickhelp :commands company-quickhelp-mode)
