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

(defcustom org-xopp-generate-image-async t
  "non-nil means to generate images asynchronously so as to not delay
the startup of org-mode.")

(defcustom org-xopp-regenerate-only-on-change t
  "non-nil means to generate images every time regardless of whether
there have been changes to the .xopp files or not.")

(defcustom org-xopp-imagemagick-command "mogrify"
  "the name of the imagemagick `mogrify' binary (`magick' is the modern alternative).")

(defcustom org-xopp-imagemagick-default-args
  (list "-transparent" 'org-xopp-background-color
        "-fuzz" "5%"
        "-trim")
  "default args to pass to imagemagick when invoking `mogrify'.

the default arguments remove the background and trim the extra padding around
the sketch.
if a string, it will be included as-is, otherwise `symbol-value' will be used.")

(defcustom org-xopp-imagemagick-extra-args (list "-resize" "50%")
  "extra args to pass to imagemagick when invoking `mogrify'.

by default -resize 50% is used to reduce the size of images so that displaying
them doesnt make emacs too laggy.")

(defcustom org-xopp-xournalpp-command "xournalpp"
  "the command used to invoke the xournalpp binary.")

(defcustom org-xopp-background-color "#ffffff"
  "the color to consider when removing the background from an image.")

(defcustom org-xopp-image-format "png"
  "the image format to use when generating figures.")

(defcustom org-xopp-gzip-command "gzip"
  "the command used to invoke the gzip binary.")

(defcustom org-xopp-gunzip-command "gunzip"
  "the command used to invoke the gunzip binary.")

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
  (let* ((image-filepath (car (org-xopp-generate-figures path))))
    ;; perhaps allow the user to specify how the figures are handled on export?
    ;; this behavior is somewhat of a "placeholder".
    (if (string= backend "html")
        (format "<img src='%s' />" image-filepath)
      (when (string= backend "latex")
        (format "\\begin{center}\\includegraphics[max width=0.5\\linewidth]{%s}\\end{center}" image-filepath)))))

(defun org-xopp-export-pages (path desc backend)
  "handles xournalpp documents on org exports."
  (cond ((equal backend 'latex)
         (let ((pdf-filepath (org-xopp-temp-file path "pdf")))
           (call-process org-xopp-xournalpp-command
                         nil nil nil
                         "--create-pdf"
                         pdf-filepath
                         path)
           (when (string= backend "latex")
             (format "\\includepdf[pages=-]{%s}" pdf-filepath))))
        ((equal backend 'html)
         (format
          "<div class='xopp-pages'>%s</div>"
          (apply
           #'concat
           (mapcar
            (lambda (image-path)
              (format "<img src='%s' />" image-path))
            (org-xopp-generate-figures path)))))))

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

(defun org-xopp-place-figures ()
  "overlay .xopp file links in the current org buffer with the corresponding sketches."
  (interactive)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (let* ((type (org-element-property :type link))
             (buffer (current-buffer))
             (path (org-element-property :path link))
             (absolute-path (expand-file-name path))
             (output-path (org-xopp-temp-file absolute-path org-xopp-image-format)))
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
                    (org-xopp-extract-figures
                     absolute-path
                     output-path
                     (lambda (final-output-paths)
                       (org-xopp-place-image buffer (car final-output-paths) link))
                     org-xopp-generate-image-async))
                ;; display without generation (file already present)
                (org-xopp-place-image buffer output-path link)))))))))

(defun org-xopp-place-image (buffer image-path link)
  "replace LINK with an overlay displaying the image in IMAGE-PATH."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((begin (org-element-property :begin link))
             (end (org-element-property :end link))
             (ov (make-overlay begin end)))
        ;; if org-link-preview-file (introduced in org-9.7) is available, make use of it,
        ;; otherwise we place the image ourselves.
        (if (fboundp 'org-link-preview-file)
            (progn
              (overlay-put ov 'modification-hooks
                           (list (lambda (ov &rest _) (delete-overlay ov))))
              (org-link-preview-file ov image-path link))
          (let* ((width (or (org-display-inline-image--width link) 400))
                 (img (org--create-inline-image image-path width))
                 (file-exists-p image-path))
            (let ((ov (make-overlay begin end)))
              (overlay-put ov 'display img)
              (overlay-put ov 'modification-hooks
                           (list (lambda (ov &rest _) (delete-overlay ov)))))))))))

(cl-defun org-xopp-command (command success-cb failure-cb &optional (async t))
  "takes COMMAND as a list, on successful run calls SUCCESS-CB with stdout,
otherwise calls FAILURE-CB with 'event' and output."
  ;; if we cant find the executable, return an error already, otherwise
  ;; call-process might trigger an exception in emacs
  (if (not (executable-find (car command)))
      (funcall failure-cb nil (format "couldnt find executable %s" (car command)))
    (let ((out-buffer (generate-new-buffer " *xopp-preview*")))
      (if async
          ;; use sentinels for async calls
          (make-process
           :name "xopp-preview"
           ;; not a good idea to keep generating new buffers
           :buffer out-buffer
           :command command
           :sentinel
           (lambda (proc event)
             (let ((out (with-current-buffer
                            (process-buffer proc)
                          (string-trim (buffer-string)))))
               (if (string= event "finished\n")
                   (funcall success-cb out)
                 (funcall failure-cb event out)))))
        ;; synchronous call
        (let ((out-code
               (apply #'call-process
                      (append (list (car command) nil out-buffer nil)
                              (rest command)))))
          (let ((out (with-current-buffer
                         out-buffer
                       (string-trim (buffer-string)))))
            (if (equal out-code 0)
                (funcall success-cb out)
              (funcall failure-cb nil out))))))))

(defun org-xopp-numbered-filepath (filepath num)
  (format "%s-%s.%s"
          (file-name-sans-extension filepath)
          num
          (file-name-extension filepath)))

;; if the document is multi-page, xournalpp generates files
;; with names of the form myfilename-i.png (e.g. myfilename-1.png)
;; we need to check if thats the case and act accordingly
(defun org-xopp-find-generated-images (out-path)
  (let ((images))
    (if (file-exists-p out-path)
        (setq images (list out-path)) ;; one file only
      (if (file-exists-p (org-xopp-numbered-filepath out-path 1))
          (let ((num 1))
            (while (file-exists-p
                    (org-xopp-numbered-filepath out-path num))
              (push (org-xopp-numbered-filepath out-path num) images)
              (setq num (1+ num))))))
    images))

(cl-defun org-xopp-extract-figures (in-path out-path cb &optional (async t))
  "extract the figure(s) from the given .xopp file IN-PATH to the destination image file OUT-PATH."
  (org-xopp-command
   (list org-xopp-gunzip-command "-c" in-path)
   (lambda (raw-data)
     ;; at this point we have grabbed the uncompressed xml file from gunzip output
     (let* ((raw-file (make-temp-file "org-xopp-"))
            (new-xopp-file (make-temp-file "org-xopp-")))
       ;; create a new file and insert the ungzipped data from the original .xopp
       ;; then properly set the background color in the new .xopp file so that
       ;; it can then be removed by imagemagick form the image generated by xournalpp
       (with-temp-file raw-file
         (insert raw-data)
         (goto-char (point-min))
         (let ((inhibit-message t)
               (message-log-max nil))
           (replace-regexp
            "<background[^>]*>"
            (format "<background type=\"solid\" color=\"%sff\" style=\"plain\"/>"
                    org-xopp-background-color))))
       (org-xopp-command
        (list org-xopp-gzip-command "-c" raw-file)
        (lambda (new-xopp-data)
          ;; at this point we have the new gzipped .xopp file with the modifications
          (let ((coding-system-for-write 'no-conversion)) ;; since we're working with raw binary data
            (with-temp-buffer
              (set-buffer-file-coding-system 'raw-text)
              (insert new-xopp-data)
              (write-region nil nil new-xopp-file)))
          ;; export the image using xournalpp's commandline by running it on the new
          ;; .xopp file we created
          (org-xopp-command
           (list org-xopp-xournalpp-command
                 "--create-img"
                 out-path
                 new-xopp-file)
           (lambda (stdout)
             ;; at this point we have the image generated by xournalpp
             ;; we run imagemagick to apply the last modifications such as bg removal
             (let ((images (org-xopp-find-generated-images out-path)))
               (mapcar
                (lambda (image)
                  (org-xopp-command
                   (append
                    (list org-xopp-imagemagick-command)
                    (mapcar
                     (lambda (arg)
                       (if (stringp arg)
                           arg
                         (symbol-value arg)))
                     org-xopp-imagemagick-default-args)
                    org-xopp-imagemagick-extra-args
                    (list image))
                   (lambda (stdout)
                     (when (and cb (string= (car (last images)) image))
                       (funcall cb images)))
                   (lambda (event out)
                     (message "error running imagemagick, %s, %s" event out))
                   async))
                images)))
           (lambda (event out)
             (message "error running xournalpp, %s, %s" event out))
           async)
          out-path)
        (lambda (event out)
          (message "error running gzip: %s, %s" event out))
        async)))
   (lambda (event out)
     (message "error running gunzip: %s, %s" event out))
   async))

(defun org-xopp-generate-figures (xopp-filepath)
  "synchronously generate a figure for the given file XOPP-FILEPATH.

CB will be called with the filepath to the image."
  (let* ((image-filepath (org-xopp-temp-file xopp-filepath org-xopp-image-format)))
    (when (or (not org-xopp-regenerate-only-on-change)
              (or (file-newer-than-file-p
                   xopp-filepath
                   (if (file-exists-p image-filepath)
                       image-filepath
                     (org-xopp-numbered-filepath image-filepath 1)))))
      (org-xopp-extract-figures xopp-filepath
                                image-filepath
                                nil
                                nil))
    (org-xopp-find-generated-images image-filepath)))

(provide 'org-xopp)
;;; org-xopp.el ends here