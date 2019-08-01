;;; org2any.el --- Integration of org2any utility into Emacs -*- coding: utf-8 -*-
;;; Commentary:

;;; Starts watching org file when it's opened in Emacs.

;;; Code:

(defvar org2any/running-processes)
(setq org2any/running-processes '())

(defun might-start-org2any ()
  "Start org2any in file watch mode."
  (when (string= (file-name-nondirectory (buffer-file-name)) "test.org")
    (let ((org2any-process
	   (start-process "org2any" "*org2any log*" "org2any" (buffer-file-name) "-v" "-w")))
      (setq org2any/running-processes (cons `(,(current-buffer) . ,org2any-process) org2any/running-processes)))
    (message "org2any started for %s" (buffer-name))))

(provide 'org2any)

;;; org2any.el ends here
