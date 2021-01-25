;;; org-pandoc-import-transient.el --- Convert other formats to Org on-the-fly -*- lexical-binding: t; -*-

;; This file is part of org-pandoc-import.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Utilise the work in `org-pandoc-import' to create a minor mode that enables
;; on-the-fly conversion to and from Org.

;;; Code:

(require 'org-pandoc-import)
(require 'org)
(require 'ox-pandoc nil t)

(defcustom org-pandoc-import-transient-async-export nil
  "Whether or not to use Org's async export mode.
This trades a short blocking period for a long non-blocking period."
  :type 'list
  :group 'org-pandoc-import)

(defcustom org-pandoc-import-transient-associations
  (append
   `(("md" . ,(if (featurep 'ox-gfm) "gfm" "md"))
     ("odt" . "odt")
     ("csv" . (lambda ()
                (unless (org-at-table-p)
                  (user-error "Point is not inside a table"))
                (org-table-export (concat (file-name-base (buffer-file-name)) ".csv")
                                  "orgtbl-to-csv")))
     ("tsv" . (lambda ()
                (unless (org-at-table-p)
                  (user-error "Point is not inside a table"))
                (org-table-export (concat (file-name-base (buffer-file-name)) ".tsv")
                                  "orgtbl-to-tsv"))))
   (when (featurep 'ox-pandoc)
     '(("rst" . org-pandoc-export-to-rst)
       ("docx" . org-pandoc-export-to-docx))))
  "An alist of file extensions, and associated exporters.
Exporters can either be a string correspanding to a particular org exporter,
or a funcion that operates on the current buffer, and requires no arguments.
An importer backend that recognises the given extension must exist.

If this is changed after initialisation, you'll want to call
`org-pandoc-import-transient--register-file-handlers' to ensure that
the handler is called for the right file extensions."
  :type 'list
  :group 'org-pandoc-import)

;;;###autoload
(define-minor-mode org-pandoc-import-transient-mode
  "On-the-fly convert other markups to Org and back."
  :global t
  :init-value nil)

(defun org-pandoc-import-transient--register-file-handlers ()
  "Modify `file-name-handler-alist' to have the transient handler associated with active extensions."
  (let* ((supported-extensions (mapcar #'car org-pandoc-import-transient-associations))
         (extension-regex (concat "\\." (regexp-opt supported-extensions) "\\'"))
         (entry (rassoc 'org-pandoc-import-transient--file-handler file-name-handler-alist))
         (org-entry (rassoc 'org-pandoc-import-transient--maybe-converted-org-file-handler file-name-handler-alist))
         (org-transient-file-regex "\\.org\\+.+\\'")
         (org-alist (assoc org-transient-file-regex auto-mode-alist)))
    (if entry
        (setcar entry extension-regex)
      (nconc file-name-handler-alist (list (cons extension-regex #'org-pandoc-import-transient--file-handler))))
    (unless org-entry
      (nconc file-name-handler-alist (list (cons org-transient-file-regex #'org-pandoc-import-transient--maybe-converted-org-file-handler))))
    (unless org-alist
      (nconc auto-mode-alist (list (cons org-transient-file-regex #'org-mode))))))
(defun org-pandoc-import-transient--deregister-file-handlers ()
  "Remove (the main) org-pandoc-import handler from `file-name-handler-alist'."
  (setq file-name-handler-alist
        (delete (rassoc 'org-pandoc-import-transient--file-handler file-name-handler-alist)
                file-name-handler-alist)))

(defun org-pandoc-import-transient--file-handler (operation &rest args)
  "Expand file names through `org-pandoc-import-transient--file-handler'.
This only occurs when mode is active.  ARGS are passed onto the OPERATION,
and OPERATION is the file operation being performed."
  (let ((inhibit-file-name-handlers
         (cons 'org-pandoc-import-transient--file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))

    (if org-pandoc-import-transient-mode
        (if (eq operation 'expand-file-name)
            (org-pandoc-import-transient--convert-file-name
             (apply operation args))
          (apply operation args))
      (apply operation args))))

(defvar org-pandoc-import-transient--files nil
  "Global alist of special files and their properties.")

(defvar org-pandoc-import-transient--currently-processing nil
  "Indicates that file processing is currently occuring.
Used to prevent undesirable triggering of the file watcher.")

(defun org-pandoc-import-transient--convert-file-name (file)
  "If FILE is being seen for the first time, convert it to Org.
Returns information on the location and state of the converted file."
  (if org-pandoc-import-transient--currently-processing
      file
    (unless (assoc file org-pandoc-import-transient--files)
      (let* ((transient-dir (expand-file-name ".opi-transient" (file-name-directory file)))
             (org-file (expand-file-name (concat (file-name-base file)
                                                 ".org+"
                                                 (file-name-extension file))
                                         transient-dir))
             (symlink-file (expand-file-name (file-name-nondirectory file)
                                             transient-dir))
             (org-pandoc-import-transient--currently-processing t))

        (unless (file-exists-p transient-dir)
          (make-directory transient-dir))

        (push (cons file (list :target org-file)) org-pandoc-import-transient--files)
        (push (cons symlink-file (list :symlink file)) org-pandoc-import-transient--files)
        (push (cons org-file (list :initialised nil :source file :source-symlink symlink-file)) org-pandoc-import-transient--files)

        (write-region "" nil org-file)
        (make-symbolic-link file symlink-file)

        (message "Created org-file stub for %s (%s)" file org-file)))


    (if-let ((org-file
              (plist-get (cdr (assoc file org-pandoc-import-transient--files)) :target)))
        org-file file)))

(defun org-pandoc-import-transient--maybe-converted-org-file-handler (operation &rest args)
  "When an org file is saved, back-propogate the changes if appropriate.
This is done by exporting the org file to the target file type, after checking
that the curret file is indeed a transient conversion.
Argument OPERATION is the file operation to be applied, and ARGS its arguments."
  (let ((inhibit-file-name-handlers
         (cons 'org-pandoc-import-transient--maybe-converted-org-file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))

    (if (and org-pandoc-import-transient-mode (not org-pandoc-import-transient--currently-processing))
        (cond
         ((eq operation 'insert-file-contents)
          (when-let* ((org-file (car args))
                      (file-info (cdr (assoc org-file org-pandoc-import-transient--files)))
                      (source-file (plist-get file-info :source))
                      (start-time (time-to-seconds (current-time)))
                      (org-pandoc-import-transient--currently-processing t))
            (unless (plist-get file-info :initialised)
              (copy-file source-file (concat source-file ".backup") nil t)
              (delete-file org-file) ; to avoid the overwrite prompt
              (message "Initialising...")
              (org-pandoc-import-to-org nil source-file org-file t)
              (plist-put file-info :initialised t)
              (setq-local kill-buffer-hook (append kill-buffer-hook '(org-pandoc-import-transient--killed)))
              (message "Created org file from %s in %2fs." source-file (- (time-to-seconds (current-time)) start-time))))
          (apply operation args))
         ((eq operation 'write-region)
          (let ((org-pandoc-import-transient--currently-processing t)
                (org-export-with-broken-links t)
                (org-export-with-toc nil))
            (ignore org-export-with-broken-links org-export-with-toc) ; Prevent "unused lexical variable" warnings
            (apply operation args)
            (when-let* ((file (nth 2 args))
                        (source-file (plist-get (cdr (assoc file org-pandoc-import-transient--files)) :source))
                        (exporter (cdr (assoc (file-name-extension source-file) org-pandoc-import-transient-associations)))
                        (start-time (time-to-seconds (current-time))))
              (if (functionp exporter)
                  (funcall exporter)
                (org-export-to-file (intern exporter) source-file org-pandoc-import-transient-async-export))
              (message "Updated %s in %2fs." source-file (- (time-to-seconds (current-time)) start-time)))))
         (t (apply operation args)))
      (apply operation args))))

(defun org-pandoc-import-transient--killed ()
  "When this buffer is closed, we assume that the source file may be modified.
Thus, if we re-open the file with `org-pandoc-import-transient-mode' enabled,
we want to re-create the associated org file."
  (plist-put (cdr (assoc (buffer-file-name) org-pandoc-import-transient--files)) :initialised nil))

(defun org-pandoc-import-transient-cleanup (&optional keep-file-handlers)
  "Remove all .opi-transient working dirs to avoid cluttering.
Dirs to remove are found from `org-pandoc-import-transient--files'.

Unless KEEP-FILE-HANDLERS is set, the file handlers will be deregistered.
This KEEP-FILE-HANDLERS is observed when called interactively without
a prefix argument."
  (interactive "p")
  (unless (eq keep-file-handlers 1)
    (org-pandoc-import-transient--deregister-file-handlers))
  (dolist (transient-dir
           (delete-dups
            (mapcar #'file-name-directory
                    (delq nil
                          (mapcar (lambda (item) (plist-get (cdr item) :target))
                                  org-pandoc-import-transient--files)))))
    (delete-directory transient-dir t))
  (setq org-pandoc-import-transient--files nil))

(org-pandoc-import-transient--register-file-handlers)
(add-hook 'kill-emacs-hook #'org-pandoc-import-transient-cleanup)

(provide 'org-pandoc-import-transient)

;;; org-pandoc-import-transient.el ends here
