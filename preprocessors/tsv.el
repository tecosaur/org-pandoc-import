;;; org-pandoc-import/preprocessors/tsv.el --- Pre-process tsv files before importing -*- lexical-binding: t; -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later

(defun org-pandoc-import-tsv-preprocessor (in-file)
  (let ((processed-file (make-temp-file "opif" nil ".csv")))
    (call-process "sed" nil (list :file processed-file) nil "-E" "s/\\t+/,/g" in-file)
    (message processed-file)
  processed-file))
