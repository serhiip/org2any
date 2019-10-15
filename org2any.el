;;; org2any.el --- Integration of org2any utility into Emacs -*- coding: utf-8 -*-

;; Package-Requires: ((dash "20190920.1035"))
;; Version: 0.0.1

;;; Commentary:

;;; Starts watching org file when it's opened in Emacs.

;;; Code:

(setq lexical-binding t)

(require 'ert)
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

(defvar org2any/org-hashes nil
  "Mapping from org id to hashed content.")

(defconst org2any/id-field-name "ID"
  "The field that will be used to store org id.")

(defconst org2any/created-at-field-name "CREATED-AT"
  "The field that will be used to store creation time of org records.")

(defconst org2any/updated-at-field-name "UPDATED-AT"
  "Updeted at property name.")

(defun org2any/start-process (file-name args)
  "Start org2any process using for FILE-NAME passing it ARGS."
  (apply 'start-process
         "org2any-process"
         "*org2any-log*"
         org2any/executable-path
         file-name
         "--default-destination"
         args))

(defun org2any/start ()
  "Start org2any in file watch mode."
  (when (and
         (not (assoc (current-buffer) org2any/running-processes))
         (org2any/file-tracked-p))
    (let* ((args (-non-nil `("-w" ,org2any/verbosity)))
           (org2any-process
            (org2any/start-process (buffer-file-name) args)))
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

(add-hook 'org-mode-hook 'org2any/populate-all-hashes)
(add-hook 'org-mode-hook 'org2any/start)

(defun org2any/populate-all-hashes ()
  "Populate missing hashes in current org file."
  (when (org2any/enabled-p)
    (org-map-entries
     (lambda ()
       (org-with-point-at (point)
         (org2any/know-org-hash
          (org-entry-get nil org2any/id-field-name)
          (sxhash (org-get-entry))))))))

(defun org2any/know-org-hash (id hash)
  "Record org item identified by ID HASH."
  (when (and id hash)
    (setq org2any/org-hashes
          (cons `(,id . ,hash) org2any/org-hashes))))

(defun org2any/populate-created-at (created-at)
  "Populate createsd at field of org entry at point by cheking CREATED-AT first."
  (unless (and created-at (-any-p (lambda (a) a) (parse-time-string created-at)))
    (org-entry-put (point) org2any/created-at-field-name (current-time-string))))

(defun org2any/enabled-p ()
  "Is org2any integration is enabled in current buffer."
  (and
   (eq major-mode 'org-mode)
   (eq buffer-read-only nil)
   (org2any/file-tracked-p)
   (file-exists-p (buffer-file-name (current-buffer)))))

(defun org2any/file-tracked-p ()
  "Check if current file is set up for atutomatic synchronization."
  (-any-p
   (lambda (re) (string-match-p re (buffer-file-name)))
   org2any/autosync-files-regexes))

(defun org2any/maybe-add-org-ids ()
  "Add identifiers to org items if buffer is matched."
  (when (org2any/enabled-p)
    (org2any/add-ids-to-items-in-current-file)))

(defun org2any/add-ids-to-items-in-current-file ()
  "Populate ids for org items in current buffer."
  (org-map-entries
   (lambda ()
     (org-with-point-at (point)
       (let* ((id (org-entry-get nil org2any/id-field-name))
              (created-at (org-entry-get nil org2any/created-at-field-name))
              (updated-at (org-entry-get nil org2any/updated-at-field-name))
              (hash (org2any/entry-hash))
              (old-hash (cdr (assoc id org2any/org-hashes))))

         (unless (and id (stringp id) (string-match "\\S-" id))
           (setq id (org-id-new))
           (org-entry-put (point) org2any/id-field-name id)
           (org-id-add-location id (buffer-file-name (buffer-base-buffer))))

         (org2any/populate-created-at created-at)

         (if updated-at
             (unless (eq hash old-hash)
               (org-entry-put
                (point) org2any/updated-at-field-name (current-time-string)))
           (org-entry-put
            (point) org2any/updated-at-field-name (current-time-string)))
         (org2any/know-org-hash id (org2any/entry-hash)))))))

(defun org2any/entry-hash ()
  "Calculate the hash of current org entry."
  (+ (sxhash (org-get-entry)) (sxhash (org-get-heading))))


;;; Tests:

(defvar org2any/--org2any-started 0
  "Count of times org2any process have been started.")

(defun org2any/--setq-fixture (sym value test-body)
  "Temporary set SYM to VALUE and execute TEST-BODY."
  (let ((original-value (symbol-value sym)))
    (unwind-protect
        (progn
          (set sym value)
          (funcall test-body))
      (set sym original-value))))

(defun org2any/--with-file (file-name body)
  "Create file name FILE-NAME and execute BODY deleting FILE-NAME afterwards."
  (cl-letf (((symbol-function 'start-process)
             (lambda (&rest args)
               (setq org2any/--org2any-started (+ org2any/--org2any-started 1)))))
    (org2any/--setq-fixture
     'org2any/autosync-files-regexes
     '("^.*/?test.file.org$")
     (lambda ()
       (unwind-protect
           (progn
             (let ((buff (find-file file-name)))
               (save-buffer)
               (insert "* Test org item")
               (save-buffer)
               (funcall body buff)))
         (progn
           (delete-file file-name)
           (kill-buffer file-name)
           (setq org2any/--org2any-started 0)))))))

(ert-deftest org2any/--org2any-is-started-when-tracked-file-is-open ()
  (org2any/--with-file
   "test.file.org"
   (lambda (buff)
     (switch-to-buffer buff)
     (org-map-entries
      (lambda ()
        (org-with-point-at (point)
          (should (equal
                   (car (cddddr (org-heading-components)))
                   "Test org item"))
          (should (equal org2any/--org2any-started 1))))))))

(ert-deftest org2any/--org2any-is-not-started-when-not-tracked-org-file-is-open ()
  (org2any/--with-file
   "test.not-tracked.org"
   (lambda (buff)
     (should (equal org2any/--org2any-started 0)))))

(ert-deftest org2any/--id-is-populated ()
  (org2any/--with-file
   "test.file.org"
   (lambda (buff)
     (switch-to-buffer buff)
     (org-map-entries
      (lambda ()
        (org-with-point-at (point)
          (should (org-entry-get nil org2any/id-field-name))))))))

(ert-deftest org2any/--test-created-at-is-populated ()
  (org2any/--with-file
   "test.file.org"
   (lambda (buff)
     (switch-to-buffer buff)
     (org-map-entries
      (lambda ()
        (org-with-point-at (point)
          (should (org-entry-get nil org2any/created-at-field-name))))))))

(ert-deftest org2any/--updated-at-is-populated ()
  (org2any/--with-file
   "test.file.org"
   (lambda (buff)
     (switch-to-buffer buff)
     (org-map-entries
      (lambda ()
        (org-with-point-at (point)
          (should (org-entry-get nil org2any/updated-at-field-name))))))))

(ert-deftest org2any/--updated-at-is-changed-when-contents-changed ()
  (org2any/--with-file
   "test.file.org"
   (lambda (buff)
     (switch-to-buffer buff)
     (org-with-point-at (point)
       (let ((old-updated-at (org-entry-get nil org2any/updated-at-field-name)))
         (move-end-of-line nil)
         (insert " modified")
         (move-beginning-of-line nil)
         ;; TODO: figure out better way
         (sleep-for 1)
         (save-buffer)
         (org-with-point-at (point)
           (should-not (equal
                        old-updated-at
                        (org-entry-get nil org2any/updated-at-field-name)))))))))

(ert-deftest org2any/--updated-at-is-changed-only-for-entity-that-is-actually-changed ()
  (org2any/--with-file
   "test.file.org"
   (lambda (buff)
     (switch-to-buffer buff)
     (move-end-of-line nil)
     (newline)
     (insert "* new item")
     (save-buffer)
     (forward-line -1)
     (org-with-point-at (point)
       (let ((old-updated-at (org-entry-get nil org2any/updated-at-field-name)))
         (forward-line 1)
         (end-of-line)
         (insert " modified")
         (sleep-for 1)
         (save-buffer)
         (forward-line -1)
         (org-with-point-at (point)
           (should (equal
                    old-updated-at
                    (org-entry-get nil org2any/updated-at-field-name)))))))))


(provide 'org2any)

;;; org2any.el ends here
