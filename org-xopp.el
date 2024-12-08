;;; org-xopp.el --- Rapidly create and follow links across text files  -*- lexical-binding: t; -*-

;; Author: Mahmood Sheikh <mahmod.m2015@gmail.com>
;; Keywords: lisp
;; Version: 0.0.2

;; Copyright (C) 2024  Mahmood Sheikh and Bob Weiner

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
;; This package is for making arbitrary links across text files.

;;; Code:
(require 'org-compat)
(require 'ol)

;; we can only `load-file-name' at load time, so we do it here to be able to invoke
;; the script.
(defvar org-xopp-figure-generation-script
  (format "%sgenerate_xopp_figure.sh" (file-name-directory load-file-name)))

(defun org-xopp-setup ()
  "Initial setup for org-xopp."
  (org-link-set-parameters "xopp-figure"
                           :follow #'org-xopp-link-open
                           :export #'org-xopp-export-figure)
  (org-link-set-parameters "xopp-pages"
                           :follow #'org-xopp-link-open
                           :export #'org-xopp-export-pages)
  (add-hook 'org-mode-hook 'org-xopp-place-figures))

(defun new-xournalpp-figure ()
  "Insert a link to a new xournalpp file meant for a figure, open the file."
  (interactive)
  (let* ((filepath (read-file-name "New xournalpp file: ")))
    (start-process "xournalpp" "xournalpp" "xournalpp" filepath)
    (insert (format "[[xopp-figure:%s]]" filepath))))

(defun new-xournalpp-pages ()
  "Insert a link to a new xournalpp file meant for a document, open the file."
  (interactive)
  (let* ((filepath (read-file-name "New xournalpp file: ")))
    (start-process "xournalpp" "xournalpp" "xournalpp" filepath)
    (insert (format "[[xopp-pages:%s]]" filepath))))

(defun org-xopp-shell-command-to-string-no-stderr (cmd)
  (with-output-to-string
    (with-current-buffer
        standard-output
      (process-file shell-file-name nil '(t nil) nil shell-command-switch cmd))))

(defun org-xopp-export-figure (path desc backend)
  (let* ((image-filepath (org-xopp-temp-image-file path)))
    (call-process org-xopp-figure-generation-script
                  nil
                  nil
                  nil
                  path
                  image-filepath)
    ;; perhaps allow the user to specify how the figures are handled on export?
    ;; this behavior is somewhat of a "placeholder".
    (when (string= backend "html")
      (format "<img src='%s' />" image-filepath))
    (when (string= backend "latex")
      (format "\\begin{center}\\includegraphics[max width=0.5\\linewidth]{%s}\\end{center}" image-filepath))))

(defun org-xopp-export-pages (path desc backend)
  (shell-command
   (format "xournalpp --create-pdf '%s' '%s'"
           pdf-filepath
           path))
  (when (equal backend "html")
    (format "<img src='%s' />" image-filepath))
  (when (equal backend "latex")
    (format "\\includepdf[pages=-]{%s}" pdf-filepath)))

(defun org-xopp-open-xournalpp (xopp-filepath)
  (start-process "xournalpp" "xournalpp" "xournalpp" xopp-filepath))

(defun org-xopp-link-open (link desc backend)
  "Handles org's :follow function (just opens the xournalpp file)."
  (let ((link-path (org-element-property :path link)))
    (org-xopp-open-xournalpp link-path)))

(defun org-xopp-temp-image-file (xopp-filepath)
  "returns a filepath of the image to be generated from the given XOPP-FILEPATH."
  (format "%s%s.png" temporary-file-directory (file-name-base xopp-filepath)))

(defun org-xopp-place-figures ()
  "overlay .xopp file links in the current org buffer with the corresponding sketches."
  (interactive)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (let* ((type (org-element-property :type link))
             (path (org-element-property :path link))
             (begin (org-element-property :begin link))
             (end (org-element-property :end link))
             (absolute-path (expand-file-name path))
             (width (org-display-inline-image--width link))
             (output-path (org-xopp-temp-image-file absolute-path))
             (ov (make-overlay begin end)))
        (when (string-equal type "xopp-figure")
          ;; check if the .xopp file exists
          (if (not (file-exists-p absolute-path))
              (message "file not found: %s" absolute-path)
            ;; export the .xopp file to an image if not already done
            (lexical-let ((begin begin)
                          (end end)
                          (width width)
                          (output-path output-path)
                          (ov ov))
              (prog1 t
                (make-process
                 :name "xopp-preview"
                 :buffer (generate-new-buffer " *xopp-preview*")
                 :command (list
                           "sh"
                           "-c"
                           (format "%s '%s' '%s' 2>/dev/null"
                                   org-xopp-figure-generation-script
                                   absolute-path
                                   output-path))
                 :sentinel
                 (lambda (_proc _status)
                   ;; its not necessary to grab it output-path from the output buffer
                   (when-let* ((output-path
                                (with-current-buffer
                                    (process-buffer _proc)
                                  (string-trim (buffer-string))))
                               (img (org--create-inline-image output-path width))
                               (org-buf (overlay-buffer ov))
                               (buffer-live-p org-buf)
                               (file-exists-p output-path))
                     (with-current-buffer org-buf
                       (save-excursion
                         (goto-char begin)
                         (let ((ov (make-overlay begin end)))
                           (overlay-put ov 'display img)
                           (overlay-put ov 'modification-hooks
                                        (list (lambda (ov &rest _) (delete-overlay ov))))))))))))))))))

(provide 'org-xopp)
;;; org-xopp.el ends here