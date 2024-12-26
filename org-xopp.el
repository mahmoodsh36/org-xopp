;; -*- lexical-binding: t -*-
;;; org-xopp.el --- embed xournalpp figures/pages in org-mode.

;; Author: Mahmood Sheikh <mahmod.m2015@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; Copyright (C) 2024  Mahmood Sheikh

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package is for embedding xournalpp figures/pages in org-mode.

;;; Code:
(require 'org-compat)
(require 'ol)
(require 'cl-lib)
(require 'org-element)

;; we can only `load-file-name' at load time, so we do it here to be able to invoke
;; the script.
(defvar org-xopp-figure-generation-script
  (format "%sgenerate_xopp_figure.sh" (file-name-directory load-file-name))
  "path to generate_xopp_figure.sh")

(defcustom org-xopp-generate-image-async t
  "non-nil means to generate images asynchronously so as to not delay
the startup of org-mode.")

(defcustom org-xopp-regenerate-only-on-change t
  "non-nil means to generate images every time regardless of whether
there have been changes to the .xopp files or not.")

(defun org-xopp-setup ()
  "initial setup for org-xopp."
  (org-link-set-parameters "xopp-figure"
                           :follow #'org-xopp-link-open
                           :export #'org-xopp-export-figure)
  (org-link-set-parameters "xopp-pages"
                           :follow #'org-xopp-link-open
                           :export #'org-xopp-export-pages)
  (add-hook 'org-mode-hook 'org-xopp-place-figures))

(defun org-xopp-new-figure ()
  "insert a link to a new xournalpp file meant for a figure, open the file."
  (interactive)
  (when-let* ((filepath (read-file-name "New xournalpp file: "))
              (full-filepath (expand-file-name filepath)))
    (org-xopp-open-xournalpp full-filepath)
    (insert (format "[[xopp-figure:%s]]" full-filepath))))

(defun org-xopp-new-pages ()
  "insert a link to a new xournalpp file meant for a document, open the file."
  (interactive)
  (when-let* ((filepath (read-file-name "New xournalpp file: "))
              (full-filepath (expand-file-name filepath)))
    (org-xopp-open-xournalpp full-filepath)
    (insert (format "[[xopp-pages:%s]]" full-filepath))))

(defun org-xopp-export-figure (path desc backend)
  "handles figures on org exports."
  (let* ((image-filepath (org-xopp-generate-figure path)))
    ;; perhaps allow the user to specify how the figures are handled on export?
    ;; this behavior is somewhat of a "placeholder".
    (if (string= backend "html")
        (format "<img src='%s' />" image-filepath)
      (when (string= backend "latex")
        (format "\\begin{center}\\includegraphics[max width=0.5\\linewidth]{%s}\\end{center}" image-filepath)))))

(defun org-xopp-export-pages (path desc backend)
  "handles xournalpp documents on org exports."
  (let ((pdf-filepath (org-xopp-temp-file path "pdf")))
    (call-process "xournalpp"
                  nil nil nil
                  "--create-pdf"
                  pdf-filepath
                  path)
    (when (string= backend "latex")
      (format "\\includepdf[pages=-]{%s}" pdf-filepath))))

(defun org-xopp-open-xournalpp (xopp-filepath)
  "open xournalpp with the given XOPP-FILEPATH."
  (start-process "xournalpp" "xournalpp" "xournalpp" xopp-filepath))

(defun org-xopp-link-open (path _)
  "handles org's :follow function (just opens the xournalpp file)."
  (org-xopp-open-xournalpp path))

(cl-defun org-xopp-temp-file (xopp-filepath &optional (extension "png"))
  "returns a filepath of the image to be generated from the given XOPP-FILEPATH."
  (format "%sorg-xopp-%s.%s"
          temporary-file-directory
          (file-name-base xopp-filepath)
          extension))

(defun org-xopp-generate-figure (xopp-filepath)
  "synchronously generate a figure for the given file XOPP-FILEPATH."
  (let* ((image-filepath (org-xopp-temp-file xopp-filepath)))
    (if (or (not org-xopp-regenerate-only-on-change)
            (file-newer-than-file-p xopp-filepath image-filepath))
        (call-process org-xopp-figure-generation-script
                      nil
                      nil
                      nil
                      xopp-filepath
                      image-filepath)
      image-filepath)))

(defun org-xopp-place-figures ()
  "overlay .xopp file links in the current org buffer with the corresponding sketches."
  (interactive)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (let* ((type (org-element-property :type link))
             (buffer (current-buffer))
             (path (org-element-property :path link))
             (absolute-path (expand-file-name path))
             (output-path (org-xopp-temp-file absolute-path)))
        (when (string-equal type "xopp-figure")
          ;; check if the .xopp file exists
          (if (not (file-exists-p absolute-path))
              (message "file not found: %s" absolute-path)
            ;; export the .xopp file to an image if not already done
            (progn
              (if (or (not org-xopp-regenerate-only-on-change)
                      (file-newer-than-file-p absolute-path output-path))
                  (progn
                    (message "generating image %s" output-path)
                    ;; generate asynchronously
                    (if org-xopp-generate-image-async
                        (make-process
                         :name "xopp-preview"
                         ;; not a good idea to keep generating new buffers
                         :buffer (generate-new-buffer " *xopp-preview*")
                         :command (list org-xopp-figure-generation-script
                                        absolute-path
                                        output-path)
                         :sentinel
                         (lambda (proc event)
                           (let ((out (with-current-buffer
                                          (process-buffer proc)
                                        (string-trim (buffer-string)))))
                             (if (string= event "finished\n")
                                 (org-xopp-place-image buffer output-path link)
                               (message "Error generating image: %s, %s" event out)))))
                      ;; generate synchronously
                      (org-xopp-generate-figure absolute-path)
                      (org-xopp-place-image buffer output-path link)
                      ))
                ;; display without generation (file already present)
                (org-xopp-place-image buffer output-path link)))))))))

(defun org-xopp-place-image (buffer image-path link)
  "replace LINK with an overlay displaying the image in IMAGE-PATH."
  (let* ((begin (org-element-property :begin link))
         (end (org-element-property :end link))
         (ov (make-overlay begin end)))
    ;; if org-link-preview-file (introduced in org-9.7) is available, make use of it,
    ;; otherwise we place the image ourselves.
    (if (fboundp 'org-link-preview-file)
        (org-link-preview-file ov image-path link)
      (when-let* ((width (or (org-display-inline-image--width link) 400))
                  (img (org--create-inline-image image-path width))
                  (buffer-live-p buffer)
                  (file-exists-p image-path))
        (let ((ov (make-overlay begin end)))
          (overlay-put ov 'display img)
          (overlay-put ov 'modification-hooks
                       (list (lambda (ov &rest _) (delete-overlay ov)))))))))

(provide 'org-xopp)
;;; org-xopp.el ends here