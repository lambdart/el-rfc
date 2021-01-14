;;; rfc-docs.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/rfc-docs.el
;; Version: 0.0.1 Alpha
;; Keywords:
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; This library provides ways to quickly: search, fetch and
;; open RFC text documents inside Emacs.
;;
;;; Code:

;; GROUP DEFINITION

(defgroup rfc-docs nil
  "Search, download and read RFC text documents."
  :prefix "rfc-docs-"
  :group 'external)

;; CUSTOM OPTIONS

(defcustom rfc-docs-dir (expand-file-name "rfc/" user-emacs-directory)
  "The directory where RFC documents are stored."
  :type 'directory)

(defcustom rfc-docs-archive-url "https://www.rfc-editor.org/rfc/"
  "The URL format for fetching arbitrary RFC documents.
Assume RFC documents."
  :type 'string
  :safe nil)

(defcustom rfc-docs-title-width 32
  "The width of the column containing RFC titles in the browser."
  :type 'integer
  :safe t)

(defcustom rfc-docs-index-file "rfc-index.txt"
  "RFC index file name."
  :type 'string
  :safe t)

(defcustom rfc-docs-prefix "[RFC-DOCS]: "
  "The prefix used in debug messages."
  :type 'string
  :safe t)

(defcustom rfc-docs-debug-message-flag t
  "Non-nil means echo `rfc-docs' debug messages."
  :type 'boolean
  :safe t)

;;; GLOBAL VARIABLES

(defvar rfc-docs-file-fmt "rfc%s.txt"
  "RFC file name format.")

(defvar rfc-docs-buffer-name-fmt "*RFC-%s*"
  "RFC buffer name format.")

(defvar rfc-docs-cache-file
  (concat rfc-docs-dir "rfc-cache.txt")
  "Entries cache file.")

(defvar rfc-docs-entries '()
  "List of RFC entries.")

;;; MACROS

(defmacro rfc-docs--message (fmt &rest args)
  "Display a internal message at the bottom of the screen.
See `message' for more information about FMT and ARGS arguments."
  `(when rfc-docs-debug-message-flag
     (message (concat rfc-docs-prefix ,fmt) ,@args)))

;;; FETCH FUNCTIONS

(defun rfc-docs--dest-dir-p ()
  "Try to create `rfc-docs-dir' if it not exists."
  (or (file-exists-p rfc-docs-dir)
      (and (y-or-n-p (format "Create directory %s? " rfc-docs-dir))
           (not (make-directory rfc-docs-dir t)))))

;; TODO: replace this by curl or fetch call using start-process
(defun rfc-docs--fetch-doc (index &optional dest)
  "Fetch RFC INDEX document.
If DEST is non-nil use it as the document
directory destination."
  (let* ((file-name (format rfc-docs-file-fmt index))
         (file-path (expand-file-name file-name (or dest rfc-docs-dir))))
    ;; unless file already exists, copy from the internet
    (or (file-exists-p file-path)
        (url-copy-file (concat rfc-docs-archive-url file-name) file-path))
    ;; return file full path
    file-path))

(defvar rfc-docs-entry-regex
  "\\(^[0-9]+\\) *\\(.*?\\)\\.\\(?: \\|$\\)"
  "RFC regex related to the index file.")

;;; ENTRIES FUNCTIONS

(defun rfc-docs--parse-entry (string)
  "Parse entry STRING to the format `\"NUMBER TITLE\"."
  (when (string-match rfc-docs-entry-regex string)
    (let ((number (match-string 1 string))
          (title  (match-string 2 string)))
      (if (string= title "Not Issued") nil
        (format "%s %s" number title)))))

(defun rfc-docs--parse-entries ()
  "Return a list of RFC index entries."
  (with-temp-buffer
    ;; insert file contents
    (insert-file-contents
     (expand-file-name rfc-docs-index-file
                       rfc-docs-dir))
    ;; go to the beginning of the buffer
    (goto-char (point-min))
    ;; get entries
    (let ((entries '()))
      (while (search-forward-regexp "^[0-9]+ " nil t)
        (let ((start (match-beginning 0)))
          (search-forward-regexp " $")
          (let* ((end (match-beginning 0))
                 (entry (rfc-docs--parse-entry
                         (buffer-substring-no-properties start end))))
            ;; save entry after parsing it
            (and entry (push entry entries)))))
      ;; return reversed entries
      (nreverse entries))))

(defun rfc-docs--write-cache-file (entries)
  "Cache RFC ENTRIES."
  (with-temp-file rfc-docs-cache-file
    ;; insert file contents in the file buffer (implicit)
    (prin1 entries (current-buffer))))

(defun rfc-docs--read-cache-file ()
  "Return FILE contents."
  (let ((file rfc-docs-cache-file))
    (when (file-exists-p file)
      (read (with-temp-buffer
              (insert-file-contents file)
              (buffer-substring-no-properties (point-min)
                                              (point-max)))))))

(defun rfc-docs--set-entries ()
  "Set RFC entries list (read/write from cache)."
  (let ((cached-entries (rfc-docs--read-cache-file)))
    ;; save it
    (setq rfc-docs-entries
          (or cached-entries
              (rfc-docs--write-cache-file (rfc-docs--parse-entries))))
    ;; return the entries
    rfc-docs-entries))

;;; BUFFER FUNCTIONS

(defun rfc-docs--create-buffer (index file)
  "Create buffer using INDEX to determinate its name.
Insert FILE contents to it."
  (let ((buffer (get-buffer-create file)))
    (and (bufferp buffer)
         (with-current-buffer buffer
           ;; insert file RFC file contents
           (insert-file-contents file)
           ;; made it read only
           (setq buffer-read-only t)
           ;; move cursor to the start of the file
           (goto-char (point-min))
           ;; rename buffer
           (rename-buffer
            (format rfc-docs-buffer-name-fmt index))
           ;; return current buffer
           (current-buffer)))))

(defun rfc-docs--display-buffer (index file)
  "Display RFC INDEX/FILE buffer."
  (display-buffer (rfc-docs--create-buffer index file)))

;;; COMMAND INTERFACE

(defun rfc-docs-read-index ()
  "Read number string using the `minibuffer'completion system."
  (completing-read "RFC: "
                   (or rfc-docs-entries
                       (rfc-docs--set-entries))
                   nil t))

(defun rfc-docs-find-file (index)
  "Open or fetch RFC INDEX text document file."
  ;; parse the necessary index argument
  (interactive (list (car (split-string (rfc-docs-read-index)))))
  ;; find file after the fetching process: from the internet or the disk
  (rfc-docs--display-buffer index (rfc-docs--fetch-doc index)))

(provide 'rfc-docs)

;;; rfc-docs.el ends here
