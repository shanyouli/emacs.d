;;; core/lib/lib-doctor.el -*- lexical-binding: t -*-

(require 'cl-macs)

(defvar lye/require-times-list nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun lye/require-times-a (orig feature &rest args)
  "Note in `lye/require-times-list' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (lye/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'lye/require-times-list
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around 'lye/require-times-a)

(defun lye/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))



(define-derived-mode lye/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 lye/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 lye/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'lye/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun lye/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun lye/require-times-sort-by-load-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun lye/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in lye/require-times-list
           with order = 0
           do (incf order)
           collect (list order
                         (vector
                          (format "%.3f" (lye/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun lye-display-require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (lye/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

(defun lye-show-init-time-h ()
  (message "init completed in %.3fms with %d garbage collections"
           (lye/time-subtract-millis after-init-time before-init-time)
           gcs-done))

(add-hook 'emacs-startup-hook 'lye-show-init-time-h 100)

(provide 'lib-doctor)
