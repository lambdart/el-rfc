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

(require 'seq)

(eval-when-compile
  (require 'cl-macs))

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

(defcustom rfc-docs-entry-title-width 64
  "The width of the column containing RFC titles in the browser."
  :type 'integer
  :safe t)

(defcustom rfc-docs-index-file "rfc-index.txt"
  "RFC index file name."
  :type 'string
  :safe t)

(defcustom rfc-docs-file-format "rfc%s.txt"
  "RFC file name format."
  :type 'string
  :safe t)

(defcustom rfc-docs-buffer-name-format "*rfc%s*"
  "RFC buffer name format."
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

(defvar rfc-docs-entries-cache
  (concat rfc-docs-dir "rfc-cache.txt")
  "The entries cache file.")

(defvar rfc-docs--entries '()
  "The list of entries in the RFC index.")

;;; MACROS

(defmacro rfc-docs--message (fmt &rest args)
  "Display a internal message at the bottom of the screen.
See `message' for more information about FMT and ARGS arguments."
  `(when rfc-docs-debug-message-flag
     (message (concat rfc-docs-prefix ,fmt) ,@args)))

;;; INTERNAL FUNCTIONS

(defun rfc-docs--buffer-name (number)
  "Return the buffer name for the RFC document NUMBER."
  (format rfc-docs-buffer-name-format
          (number-to-string number)))

(defun rfc-docs--file-path (number)
  "Return the absolute path of the RFC file NUMBER."
  (expand-file-name (format rfc-docs-file-format number) rfc-docs-dir))

(defun rfc-docs--ensure-dest-dir ()
  "Create `rfc-docs-dir' if does not exists."
  (or (file-exists-p rfc-docs-dir)
      (and (y-or-n-p (format "Create directory %s? " rfc-docs-dir))
           (not (make-directory rfc-docs-dir t)))))

;; TODO: replace this by curl or fetch call using start-process
(defun rfc-docs--fetch-file (number)
  "Fetch (if necessary) RFC document using its NUMBER (string)."
  (unless (rfc-docs--ensure-dest-dir)
    (rfc-docs--message "Missing destination directory"))
  ;; set file name and its destination path
  (let* ((file-name (format rfc-docs-file-format number))
         (file-path (expand-file-name file-name rfc-docs-dir)))
    ;; unless file already exists, download it
    (or (file-exists-p file-path)
        (let ((file-url (concat rfc-docs-archive-url file-name)))
          (url-copy-file file-url file-path)))))

;; (defun rfc-docs--create-buffer (number)
;;   " a buffer visiting the RFC document NUMBER.
;; The buffer is created if it does not exist."
;;   (let ((file-path (rfc-docs--file-path number))
;;         (buffer-name (rfc-docs--buffer-name number)))
;;     (if (not (rfc-docs--fetch-file number file-path))
;;         (rfc-docs--message "Fetching the document fails")
;;       (find-file file-path)
;;       (rename-buffer buffer-name)
;;       ;; not your responsibility
;;       (read-only-mode 1)
;;       (current-buffer))))

;; (defun rfc-docs--parse-index-entry (string)
;;   "Parse the RFC document index entry STRING and return it as a plist."
;;   (unless (string-match "\\(^[0-9]+\\) *\\(.*?\\)\\.\\(?: \\|$\\)" string)
;;     (error "Invalid index entry format: %S" string))
;;   (let* ((number-string (match-string 1 string))
;;          (number (string-to-number number-string))
;;          (title (match-string 2 string))
;;          (entry nil))
;;     (cond
;;      ;; verify error
;;      ((not number)
;;       (error "Invalid index entry number: %S" number-string))
;;      ;; default, set entry
;;      (t (setq entry (list :number number :title title))))
;;     entry))

;; (defun rfc-docs--parse-ref (string)
;;   "Parse a reference to a RFC document from STRING.
;; For example: \"RFC 2822\"."
;;   (when (string-match "^RFC *\\([0-9]+\\)" string)
;;     (string-to-number (match-string 1 string))))

;; (defun rfc-docs--parse-refs (string)
;;   "Parse a list of references to RFC documents from STRING.
;; For example: \"RFC3401, RFC3402 ,RFC 3403\"."
;;   (seq-remove #'null (mapcar #'rfc-docs--parse-ref
;;                              (split-string string "," t " +"))))

;; (defun rfc-docs--parse-cadidate (entry)
;;   "Parse ENTRY to candidate string that will be used latter."
;;   (let* ((number (format "%s" (plist-get entry :number)))
;;          (title (truncate-string-to-width
;;                  (plist-get entry :title)
;;                  rfc-docs-entry-title-width))
;;          (candidate (format "%s|%s" number title)))
;;     candidate))

;; (defun rfc-docs-candidates ()
;;   "Return parsed candidates (string list) from `rfc-docs-index-entries'."
;;   (rfc-docs--fetch-file "-index" (rfc-docs-index-path))
;;   (unless rfc-docs-index-entries
;;     (setq rfc-docs-index-entries
;;           (rfc-docs-read-index-file (rfc-docs-index-path))))
;;   (if rfc-docs-index-entries
;;       (let ((candidates (mapcar
;;                          #'rfc-docs--parse-cadidate
;;                          rfc-docs-index-entries)))
;;         candidates)
;;     nil))

;; (defun rfc-docs-index-path ()
;;   "Return he path of the file containing the index of all RFC documents."
;;   (expand-file-name rfc-docs-index rfc-docs-dir ))

;; (defun rfc-docs-read-index (buffer)
;;   "Read an RFC index file from BUFFER and return a list of entries."
;;   (with-current-buffer buffer
;;     (goto-char (point-min))
;;     (let ((entries nil))
;;       (while (search-forward-regexp "^[0-9]+ " nil t)
;;         (let ((start (match-beginning 0)))
;;           (search-forward-regexp " $")
;;           (let* ((end (match-beginning 0))
;;                  (lines (buffer-substring start end))
;;                  (entry-string (replace-regexp-in-string "[ \n]+" " " lines))
;;                  (entry (rfc-docs--parse-index-entry entry-string)))
;;             (unless (string= (plist-get entry :title) "Not Issued")
;;               (push entry entries)))))
;;       (nreverse entries))))

;; (defun rfc-docs-read-index-file (path)
;;   "Read an RFC index file at PATH and return a list of entries."
;;   (with-temp-buffer
;;     (insert-file-contents path)
;;     (rfc-docs-read-index (current-buffer))))

;; (defun rfc-docs-switch-to-buffer (number)
;;   "Switch to RFC buffer: document NUMBER."
;;   (let ((buffer (rfc-docs--create-buffer number)))
;;     (if (not buffer)
;;         (rfc--docs-message "Error, buffer does not exists")
;;       (switch-to-buffer buffer))))

;; ;;;###autoload
;; (defun rfc-docs-reload-index ()
;;   "Reload the RFC document index from its original file."
;;   (interactive)
;;   (setq rfc-docs-index-entries
;;         (rfc-docs-read-index-file (rfc-docs-index-path))))

;; ;;;###autoload
;; (defun rfc-docs-find-file ()
;;   "Browse through all RFC documents referenced in the index."
;;   (interactive)
;;   (let ((candidates (rfc-docs-candidates)))
;;     (if (not candidates)
;;         (rfc-docs--message "No candidates available")
;;       (rfc-docs-switch-to-buffer
;;        (string-to-number
;;         (completing-read "RFC: " candidates nil t))))))

;; ;;;###autoload
;; (define-minor-mode rfc-docs
;;   "Define a new minor mode `rfc-docs'.

;; This defines the toggle command `rfc-docs' and (by default)
;; a control variable `rfc-docs'.

;; Interactively with no prefix argument, it toggles the mode.
;; A prefix argument enables the mode if the argument is positive,
;; and disables it otherwise."

;;   :group rfc-docs
;;   :lighter rfc-docs-minor-mode-string
;;   (cond
;;    (rfc-docs
;;     ;; create docs directory (if necessary)
;;     (rfc-docs--create-dir))
;;    (t (setq rfc-docs nil))))

(provide 'rfc-docs)

;;; rfc-docs.el ends here
