;;; org2any.el --- Integration of org2any utility into Emacs -*- coding: utf-8 -*-
;;; Commentary:

;;; Starts watching org file when it's opened in Emacs.

;;; Code:

(defcustom org2any/executable-path "org2any"
  "Path to org2any executable."
  :type '(file :must-match t)
  :group 'org2any)

(defvar org2any/running-processes)
(setq org2any/running-processes '())

(defun org2any/start-org2any ()
  "Start org2any in file watch mode."
  (when (and (string= (file-name-nondirectory (buffer-file-name)) "test.org")
	     (not (assoc (current-buffer) org2any/running-processes)))
    (let ((org2any-process
	   (start-process org2any/executable-path "*org2any log*" "org2any" (buffer-file-name) "-v" "-w")))
      (setq org2any/running-processes (cons `(,(current-buffer) . ,org2any-process) org2any/running-processes))
      (add-hook 'kill-buffer-hook 'org2any/teardown))
    (message "org2any started for %s" (buffer-name))))

(defun org2any/teardown ()
  "Send symbol to end watching Org file."
  (let ((process-opt (cdr (assoc (current-buffer) org2any/running-processes))))
    (when process-opt
      (kill-process process-opt)
      (setq org2any/running-processes (assq-delete-all (current-buffer) org2any/running-processes))
      (when (equal 0 (length org2any/running-processes))
	(remove-hook 'kill-buffer-hook 'org2any/teardown)))))

(provide 'org2any)

;;; org2any.el ends here
