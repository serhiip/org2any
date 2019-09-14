;;; org2any.el --- Integration of org2any utility into Emacs -*- coding: utf-8 -*-

;; Package-Requires: ((dash "2.12.0"))

;;; Commentary:

;;; Starts watching org file when it's opened in Emacs.

;;; Code:

(setq lexical-binding t)

(require 'dash)
(require 'org)
(require 'org-id)

(defcustom org2any/executable-path "org2any"
  "Path to org2any executable."
  :type '(file :must-match t)
  :group 'org2any)

(defcustom org2any/autosync-files-regexes '("^.*/?test.org$")
  "Files for which automatic synchronization should be enabled."
  :type '(repeat regexp)
  :group 'org2any)

(defcustom org2any/verbosity nil
  "If org2any should be started in debug mode."
  :type '(choice (const :tag "Normal" nil)
                 (const :tag "Verbose" "-v")
                 (const :tag "Quiet" "-q"))
  :group 'org2any)

(defvar org2any/running-processes nil
  "Mapping from org buffers to running org2any processes.")

(defun org2any/start ()
  "Start org2any in file watch mode."
  (when (and
         (not (assoc (current-buffer) org2any/running-processes))
         (org2any/file-tracked-p))
    (let* ((args (-non-nil `("-w" ,org2any/verbosity)))
           (org2any-process (apply 'start-process
                                   "org2any-process"
                                   "*org2any-log*"
                                   org2any/executable-path
                                   (buffer-file-name)
                                   "--default-destination"
                                   args)))
      (setq org2any/running-processes
            (cons `(,(current-buffer) . ,org2any-process) org2any/running-processes))
      (add-hook 'kill-buffer-hook 'org2any/teardown)
      (add-hook 'before-save-hook 'org2any/maybe-add-org-ids))
    (message "org2any started for %s" (buffer-name))))

(defun org2any/teardown ()
  "Send symbol to end watching Org file."
  (let ((process-opt (cdr (assoc (current-buffer) org2any/running-processes))))
    (when (and
           process-opt
           (processp process-opt)
           (equal (process-status process-opt) 'run))
      (process-send-string process-opt "stop via org2any.el\n"))
    (setq org2any/running-processes
          (assq-delete-all (current-buffer) org2any/running-processes))
    (when (equal 0 (length org2any/running-processes))
      (remove-hook 'kill-buffer-hook 'org2any/teardown)
      (remove-hook 'before-save-hook 'org2any/maybe-add-org-ids))))

(defun org2any/file-tracked-p ()
  "Check if current file is set up for atutomatic synchronization."
  (-any-p
   (lambda (re) (string-match-p re (buffer-file-name)))
   org2any/autosync-files-regexes))

(defun org2any/maybe-add-org-ids ()
  "Add identifiers to org items if buffer is matched."
  (when (and (eq major-mode 'org-mode)
             (eq buffer-read-only nil)
             (org2any/file-tracked-p))
    (org2any/add-ids-to-items-in-current-file)))

(defun org2any/add-ids-to-items-in-current-file ()
  "Populate ids for org items in current buffer."
  (org-map-entries
   (lambda ()
     (org-with-point-at (point)
       (let* ((id-field-name "ID")
              (id (org-entry-get nil id-field-name)))
         (unless (and id (stringp id) (string-match "\\S-" id))
           (setq id (org-id-new))
           (org-entry-put (point) id-field-name id)
           (org-id-add-location id (buffer-file-name (buffer-base-buffer)))))))))

(provide 'org2any)

;;; org2any.el ends here
