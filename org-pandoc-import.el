;;; org-pandoc-import.el --- Stay in org-mode as much as possible -*- lexical-binding: t; -*-

;; Copyright (C) 2020 TEC

;; Author: TEC <https://github/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: 16 Aug 2020
;; Modified: August 29, 2020
;; Version: 1.0
;; Keywords: org-mode, pandoc, convenience, files
;; Homepage: https://github.com/tecosaur/org-pandoc-import
;; Package-Requires: ((emacs "26.3"))

;;; License:

;; This file is part of org-pandoc-import, which is not part of GNU Emacs.
;;
;; org-pandoc-import is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; org-pandoc-import is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with org-pandoc-import.  If not, see <https://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Leverage Pandoc to make convert non-org formats to org trivial.
;; Also supplies a minor mode to do on-the-fly conversion transparently
;; and automatically, exporting to the original format on save.

;;; Code:

(require 'subr-x)

(defgroup org-pandoc-import nil
  "Provides methods to convert other markup files to Org."
  :tag "Org Pandoc Import"
  :group 'org)

(defcustom org-pandoc-import-executable "pandoc"
  "Location of the pandoc binary."
  :type 'string
  :group 'org-pandoc-import)

(defcustom org-pandoc-import-buffer-name "*org-pandoc-import*"
  "Name of the buffer to be created for the results of pandoc's conversion.
If a function, it is called to provide a string with the input file name
as the argument."
  :type '(choice string function)
  :group 'org-pandoc-import)

(defcustom org-pandoc-import-setup-defaults t
  "If non-nil, set up a number of default import backends."
  :type 'boolean
  :group 'org-pandoc-import)

(defcustom org-pandoc-import-setup-folder
  (file-name-directory
   (eval-when-compile
     (or (bound-and-true-p byte-compile-current-file)
         load-file-name)))
  "The default folder to look for filters and preprocessor files in.
Affects `org-pandoc-import-filters-folder' and `org-pandoc-import-preprocessor-folder'."
  :type 'string
  :group 'org-pandoc-import)

(defcustom org-pandoc-import-filters-folder
  (expand-file-name
   "filters" org-pandoc-import-setup-folder)
  "Location of Lua filters for use with pandoc.
If FITERS/backend.lua exists, it will automatically be used when backend is registered."
  :type 'string
  :group 'org-pandoc-import)

(defcustom org-pandoc-import-preprocessor-folder
  (expand-file-name
   "preprocessors" org-pandoc-import-setup-folder)
  "A file to but pre-processors in.
When a new backend is defined, if PREPROCESSORS/backend.el exists it will be
loaded, and if org-pandoc-import-backend-preprocessor exists (where backend a
placeholder for the actual backend name), it will be called with the input file
as the argument, and the result used as the new input file."
  :type 'string
  :group 'org-pandoc-import)

(defcustom org-pandoc-import-global-args nil
  "List of arguments to apply to all backends.
Accepts three types of atoms:
- strings, which are passed as-is to `call-process'
- plist keywords, which have the : replaced with single dash if the word
  is one charachter, else two dashes
- functions, which are evaluated and have the result passed as one
  of the arguments"
  :type 'list
  :group 'org-pandoc-import)

(defcustom org-pandoc-import-global-filters
  (directory-files org-pandoc-import-filters-folder nil "^_")
  "List of filters to apply to all backends.
Either the name of a file contained in `org-pandoc-import-filters-folder', or
an absolute path to a filter.

Filters ending in '.lua' will be called with '--lua-filter', and all other
filters with '--filter'.

By default, all files starting with '_' in `org-pandoc-import-filters-folder'
are used."
  :type 'list
  :group 'org-pandoc-import)

(defvar org-pandoc-import-backends nil
  "List of registerd org-pandoc-import backends.")

;;;###autoload
(defmacro org-pandoc-import-backend (name &optional recognised-extensions pandoc-type filters pandoc-args)
  "Create an export backend named NAME.
The backend is applied by default on files which end in a RECOGNISED-EXTENSIONS.
This calls pandoc, specifying the input format to be PANDOC-TYPE.
PANDOC-ARGS is a list of args passed to the pandoc command in the same manner
as `org-pandoc-import-global-args'.
FILTERS can be either absolute paths to pandoc filters, or names of files
within `org-pandoc-import-filters-folder'.

RECOGNISED-EXTENSIONS defaults to '(\"NAME\"), and PANDOC-TYPE to \"NAME\"."
  (let* ((s-name (symbol-name name))
         (recognised-extensions (or recognised-extensions `'(,s-name)))
         (pandoc-type (or pandoc-type s-name))
         (format-args (intern (format "org-pandoc-import-%s-args" s-name)))
         (format-extensions (intern (format "org-pandoc-import-%s-extensions" s-name)))
         (format-filters (intern (format "org-pandoc-import-%s-filters" s-name)))
         (preprocessor (let ((preprocessor-file
                              (expand-file-name (concat s-name ".el") org-pandoc-import-preprocessor-folder))
                             (preprocerror-func (intern (format "org-pandoc-import-%s-preprocessor" s-name))))
                         (when (file-exists-p preprocessor-file)
                           (load preprocessor-file nil t)
                           (when (fboundp preprocerror-func)
                             preprocerror-func))))
         (common-args (list pandoc-type 'in-file format-extensions format-args format-filters
                            'syncronous-p (when preprocessor `(function ,preprocessor))))
         (filters (or filters
                      (let ((filter (expand-file-name (concat s-name ".lua")
                                                      org-pandoc-import-filters-folder)))
                        (when (file-exists-p filter) (list filter))))))
    `(progn
       (defvar ,format-args ,pandoc-args
         ,(format "Arguments to be passed to pandoc when processing a %s file.
This is treated the same as `org-pandoc-import-global-args'" s-name))

       (defvar ,format-extensions ,recognised-extensions
         ,(format "File extensions to recognise as associated with %s." s-name))

       (defvar ,format-filters ',filters
         ,(format "Either an absolute path to, or the name of a filter within `org-pandoc-import-filters-folder'
to be applied when converting from %s.
This is treated the same as `org-pandoc-import-global-filters'" s-name))

       (defun ,(intern (format "org-pandoc-import-%s-as-org" s-name)) (prompty &optional in-file syncronous-p)
         ,(format "Parse the provided %s IN-FILE to org-mode and open in a new buffer.
Recognises files with extensions which are a member of `org-pandoc-import-%s-extensions'.
Calls pandoc with arguments listed in `org-pandoc-import-%s-args', and filters `org-pandoc-import-%s-filters'." s-name s-name s-name s-name)
         (interactive "P")
         (org-pandoc-import-convert prompty nil ,@common-args))

       (defun ,(intern (format "org-pandoc-import-%s-to-org" s-name)) (prompty &optional in-file out-file syncronous-p)
         ,(format "Parse the provided %s IN-FILE to org-mode and save to OUT-FILE - defulting to (file-name-base IN-FILE).org.
Recognises files with extensions which are a member of `org-pandoc-import-%s-extensions'.
Calls pandoc with arguments listed in `org-pandoc-import-%s-args', and filters `org-pandoc-import-%s-filters'." s-name s-name s-name s-name)
         (interactive "P")
         (org-pandoc-import-convert prompty (or out-file t) ,@common-args))

       (add-to-list 'org-pandoc-import-backends ',name))))

(defun org-pandoc-import-convert (prompty out-file pandoc-type &optional in-file expected-extensions args filters syncronous-p preprocessor)
  "Call pandoc on an IN-FILE.
Determines the relevant paramaters to convert IN-FILE of type PANDOC-TYPE to
either OUT-FILE, or a buffer (when OUT-FILE is nil).

If PROMPTY is non-nill, then the value of IN-FILE and (if applicable) OUT-FILE
will be always prompted for.  A prompt for IN-FILE is also triggered when
IN-FILE is nil, or its extension is not a member of EXPECTED-EXTENSIONS.
A prompt for OUT-FILE is triggered when OUT-FILE is t, or the name of a
pre-existing file.  Pandoc is then called with arguments from the list ARGS,
as described in `org-pandoc-import-global-args', and filters named in the list
FILTERS --- which can be either absolute paths to pandoc filters, or names of
files within `org-pandoc-import-filters-folder'.

If preprocessor is given, and a function, it is run with the value of IN-FILE.
The value returned is used as the new IN-FILE."

  (let* ((in-file (or in-file
                      (if (and (not prompty)
                               (buffer-file-name)
                               (if expected-extensions
                                   (member (file-name-extension (buffer-file-name))
                                           expected-extensions))
                               t)
                          (buffer-file-name)
                        (read-file-name "File to convert: " nil nil t
                                        (when expected-extensions
                                          (concat "." (car expected-extensions)))))))
         (in-file-processed (if (and preprocessor (functionp preprocessor))
                                (funcall preprocessor in-file)
                              in-file))
         (in-file-org (concat (file-name-sans-extension in-file) ".org"))
         (out-file (if (eq t out-file)
                       (if prompty
                           (read-file-name "File to write: "
                                           (file-name-directory in-file)
                                           nil nil
                                           (concat (file-name-base in-file) ".org"))
                         in-file-org)
                     out-file))
         (filter-args nil))

    (if (and out-file (file-exists-p out-file))
        (unless (yes-or-no-p (format "Overwrite file %s? "
                                     (file-relative-name
                                      out-file
                                      (file-name-directory in-file))))
          (setq out-file
                (read-file-name "File to write: "
                                (file-name-directory in-file)))))

    (dolist (filter (append filters org-pandoc-import-global-filters))
      (setq filter-args
            (append filter-args
                    (list (pcase (file-name-extension filter)
                            ("lua" "--lua-filter")
                            (_ "--filter"))
                          (if (= ?/ (aref filter 0)) filter
                            (expand-file-name filter org-pandoc-import-filters-folder))))))

    (org-pandoc-import-run-convert
     (org-pandoc-import-generate-convert-arguments
      in-file-processed pandoc-type out-file (append args filter-args))
     in-file-processed out-file syncronous-p)))


(defun org-pandoc-import-run-convert (arguments in-file &optional out-file syncronous-p)
  "Call pandoc on IN-FILE with ARGUMENTS, creating OUT-FILE if given.
`call-process' is used instead of `start-process' if SYNCRONOUS-P is non-nil."
  (let* ((pandoc-buffer (generate-new-buffer
                         (if (functionp org-pandoc-import-buffer-name)
                             (funcall org-pandoc-import-buffer-name in-file)
                           org-pandoc-import-buffer-name)))
         (process
          (if syncronous-p
              (apply #'call-process
                     org-pandoc-import-executable
                     nil
                     pandoc-buffer
                     nil
                     arguments)
            (apply #'start-process
                   "org-pandoc-import"
                   pandoc-buffer
                   org-pandoc-import-executable
                   arguments))))
    (unless syncronous-p
      (set-process-sentinel process (org-pandoc-import-process-sentinel pandoc-buffer out-file (time-to-seconds (current-time)))))))

(defun org-pandoc-import-process-sentinel (process-buffer &optional out-file start-time-seconds)
  "Creats a lambda sentinel for a pandoc process, outputing to PROCESS-BUFFER.
If OUT-FILE is given, kill the PROCESS-BUFFER and use the file in its place.
When START-TIME-SECONDS is given, a messege is generated indicating the total
time elapsed."
  (lambda (process _signal)
    (pcase (process-status process)
      ('exit (if out-file
                 (progn (find-file out-file)
                        (kill-buffer process-buffer))
               (switch-to-buffer process-buffer)
               (goto-char (point-min)))
             (when start-time-seconds
               (message "Converted docunent in %3fs" (- (time-to-seconds (current-time)) start-time-seconds)))
             (org-mode))
      ((or 'stop 'signal 'failed)
       (user-error "The pandoc process to create %s has exited unexpectedly" out-file)
       (switch-to-buffer process-buffer)))))

(defun org-pandoc-import-generate-convert-arguments (in-file target-format &optional out-file arguments-list)
  "Format the provided arguments to be passed to pandoc.
Have pandoc convert IN-FILE of pandoc type TARGET-FORMAT to the org file
OUT-FILE (if given), with arguments given by ARGUMENTS-LIST."
  (let (arguments)
    (dolist (element (reverse (append arguments-list org-pandoc-import-global-args)))
      (push
       (cond
        ((stringp element) element)
        ((keywordp element) (let ((keyword (substring (symbol-name element) 1)))
                              (pp keyword)
                              (concat (if (= 1 (length keyword)) "-" "--")
                                      keyword)))
        ((functionp element) (funcall element in-file target-format)))
       arguments))
    (append (list "-f" target-format
                  "-t" "org")
            (when out-file
              (list "-o" out-file))
            arguments
            (list in-file))))


;;;###autoload
(defun org-pandoc-import-as-org (prompty &optional in-file syncronous-p)
  "Parse the provided file to `org-mode', and open in a new buffer.
With PROMPTY (given by the universal argument), always prompt for the IN-FILE
to act on.

This only works so long as these is backend registered in
`org-pandoc-import-backends' associated with the extension of the selected file.
See org-pandoc-import-{backend}-as-org for information on a particular backend.

When SYNCRONOUS-P is set, the pandoc process is run in a blocking manner."
  (interactive "P")
  (if-let ((backend (org-pandoc-import-find-associated-backend (or in-file (buffer-file-name)))))
      (funcall (intern (format "org-pandoc-import-%s-as-org" (symbol-name backend)))
               prompty in-file syncronous-p)
    (funcall #'org-pandoc-import-as-org prompty (read-file-name "File to convert: " nil nil t) syncronous-p)))

;;;###autoload
(defun org-pandoc-import-to-org (prompty &optional in-file out-file syncronous-p)
  "Parse the provided file to an `org-mode' file, and open.
With PROMPTY (given by the universal argument), always prompt for the IN-FILE to
act on, and the where to save the new Org file.
The result is saved to OUT-FILE, which defaults to IN-FILE but with the .org
extension.

This only works so long as these is backend registered in
`org-pandoc-import-backends' associated with the extension of the selected file.
See org-pandoc-import-{backend}-as-org for information on a particular backend.

When SYNCRONOUS-P is set, the pandoc process is run in a blocking manner."
  (interactive "P")
  (if-let ((backend (org-pandoc-import-find-associated-backend (or in-file (buffer-file-name)))))
      (funcall (intern (format "org-pandoc-import-%s-to-org" (symbol-name backend)))
               prompty in-file out-file syncronous-p)
    (funcall #'org-pandoc-import-to-org prompty (read-file-name "File to convert: " nil nil t) syncronous-p)))


(defun org-pandoc-import-find-associated-backend (file)
  "Find the backend symbol from associated with FILE's extension.
The backend is found from searching `org-pandoc-import-backends', and is nil
if no such match could be found."
  (when file
    (let ((ext (file-name-extension file))
          the-backend)
      (dolist (backend org-pandoc-import-backends)
        (when (member ext (symbol-value
                           (intern
                            (format "org-pandoc-import-%s-extensions"
                                    (symbol-name backend)))))
          (setq the-backend backend)))
      the-backend)))

(dont-compile
  (when org-pandoc-import-setup-defaults
    (org-pandoc-import-backend markdown '("md" "markdown"))
    (org-pandoc-import-backend latex '("tex" "latex"))
    (org-pandoc-import-backend rst)
    (org-pandoc-import-backend odt)
    (org-pandoc-import-backend docx)
    (org-pandoc-import-backend rmarkdown '("rmd" "Rmd") "markdown")
    (org-pandoc-import-backend ipynb)
    (org-pandoc-import-backend csv)
    (org-pandoc-import-backend tsv '("tsv") "csv")))

(provide 'org-pandoc-import)

;;; org-pandoc-import.el ends here
