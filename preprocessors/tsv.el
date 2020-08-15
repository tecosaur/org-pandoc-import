;;; org-pandoc-import/preprocessors/tsv.el -*- lexical-binding: t; -*-

(defun org-pandoc-import-tsv-preprocessor (in-file)
  (let ((processed-file (make-temp-file "opif" nil ".csv")))
    (call-process "sed" nil (list :file processed-file) nil "-E" "s/\\t+/,/g" in-file)
    (message processed-file)
  processed-file))
