;;; org-pandoc-import/preprocessors/asciidoc.el --- Pre-process AsciiDoc files before importing -*- lexical-binding: t; -*-
;;; SPDX-License-Identifier: GPL-3.0-or-later

(defun org-pandoc-import-asciidoc-preprocessor (in-file)
  (let ((processed-file (make-temp-file "opif" nil ".dbk"))
        (stderr-file (make-temp-file "opif" nil ".err")))
    (call-process (executable-find "asciidoctor") nil
                  (list `(:file ,processed-file) stderr-file) nil
                  "--backend" "docbook" "--out-file" "-" in-file)
    ;; Dispatch stderr to *Warnings* buffer if there is any
    (if (< 0 (file-attribute-size (file-attributes stderr-file)))
        (with-temp-buffer
          (insert-file-contents stderr-file)
          (warn (buffer-string))))
    (message processed-file)
  processed-file))
