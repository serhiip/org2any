;;; org2any.el --- Integration of org2any utility into Emacs -*- coding: utf-8 -*-

;; Package-Requires: ((dash "2.12.0"))

;;; Commentary:

;;; Starts watching org file when it's opened in Emacs.

;;; Code:

(setq lexical-binding t)

(require 'dash)

(defcustom org2any/executable-path "org2any"
  "Path to org2any executable."
  :type '(file :must-match t)
  :group 'org2any)

(defcustom org2any/autosync-files-regexes '("^.*/?test.org$")
  "Files for which automatic synchronization should be enabled."
  :type '(repeat regexp)
  :group 'org2any)

(defvar org2any/running-processes nil
  "Mapping from org buffers to running org2any processes.")

(defun org2any/start ()
  "Start org2any in file watch mode."
  (when (and
	 (not (assoc (current-buffer) org2any/running-processes))
	 (-any-p
	      (lambda (re) (string-match-p re (buffer-file-name)))
	      org2any/autosync-files-regexes))
    (let ((org2any-process (start-process
			    org2any/executable-path
			    "*org2any log*"
			    "org2any"
			    (buffer-file-name)
			    "-v"
			    "-w")))
      (setq org2any/running-processes
	    (cons `(,(current-buffer) . ,org2any-process) org2any/running-processes))
      (add-hook 'kill-buffer-hook 'org2any/teardown))
    (message "org2any started for %s" (buffer-name))))

(defun org2any/teardown ()
  "Send symbol to end watching Org file."
  (let ((process-opt (cdr (assoc (current-buffer) org2any/running-processes))))
    (when (and process-opt (equal (process-status process-opt) 'run))
      (process-send-string process-opt "stop via org2any.el\n"))
    (setq org2any/running-processes (assq-delete-all (current-buffer) org2any/running-processes))
    (when (equal 0 (length org2any/running-processes))
      (remove-hook 'kill-buffer-hook 'org2any/teardown))))

(provide 'org2any)

;;; org2any.el ends here
