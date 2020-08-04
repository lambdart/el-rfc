;;; rfc-docs.el --- RFC document browser and viewer
;;
;; -*- lexical-binding: t -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Version: 0.1
;; Package-Type:
;; URL: https://github.com/esac-io/rfc-docs
;; Compatibility: GNU Emacs 25.x
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 esac
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
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
;; This package makes it easy to browse/search/read RFC documents,
;; helm dependency from its original code was removed,
;; the minibuffer with the standards completions mechanism
;; are more then sufficient for me.
;;
;; PS: icomplete rules!
;;
;;; Code:

(require 'seq)
(require 'text-mode)

(eval-when-compile
  (require 'cl-macs))

(defgroup rfc-docs nil
  "Tools to browse and read RFC documents."
  :prefix "rfc-docs-"
  :link '(url-link :tag "GitHub" "https://github.com/esac-io/rfc-docs")
  :group 'external)

(defcustom rfc-docs-directory
  (expand-file-name "rfc/" user-emacs-directory)
  "The directory where RFC documents are stored."
  :type 'directory)

(defcustom rfc-docs-url
  "https://www.rfc-editor.org/rfc/rfc%s.txt"
  "A `format'able URL for fetching arbitrary RFC documents.
Assume RFC documents are named as e.g. rfc21.txt, rfc-index.txt."
  :type 'string)

(defcustom rfc-docs-orig-buffer-name-flag nil
  "Non-nil means RFC document buffers should keep their original name."
  :type 'boolean)

(defcustom rfc-docs-entry-title-width 60
  "The width of the column containing RFC titles in the browser."
  :type 'integer)

(defface rfc-docs-header-face
  '((t :inherit font-lock-comment-face))
  "Face used for RFC document page headers.")

(defface rfc-docs-section-title-face
  '((t :inherit font-lock-keyword-face))
  "Face used for RFC document section titles.")

(defvar rfc-docs-index-entries nil
  "The list of entries in the RFC index.")

(defun rfc-docs--document-buffer-name (number)
  "Return the buffer name for the RFC document NUMBER."
  (concat "*rfc" (number-to-string number) "*"))

(defun rfc-docs--document-path (number)
  "Return the absolute path of the RFC document NUMBER."
  (expand-file-name
   (format "rfc%s.txt" number) rfc-docs-directory))

(defun rfc-docs--ensure-directory-exists ()
  "Create `rfc-docs-directory' if does not exists."
  (when (and (not (file-exists-p rfc-docs-directory))
             (y-or-n-p (format "Create directory %s? " rfc-docs-directory)))
    (make-directory rfc-docs-directory t)))

(defun rfc-docs--fetch-document (suffix document-path)
  "Ensure an RFC document with SUFFIX exists at DOCUMENT-PATH.
If no such file exists, fetch it from `rfc-document-url'."
  (rfc-docs--ensure-directory-exists)
  (unless (file-exists-p document-path)
    (url-copy-file (format rfc-docs-url suffix)
                   document-path)))

(defun rfc-docs--document-buffer (number)
  "Return a buffer visiting the RFC document NUMBER.
The buffer is created if it does not exist."
  (let* ((buffer-name (rfc-docs--document-buffer-name number))
         (document-path (rfc-docs--document-path number)))
    (rfc-docs--fetch-document number document-path)
    (find-file document-path)
    (unless rfc-docs-orig-buffer-name-flag
      (rename-buffer buffer-name))
    (read-only-mode 1)
    (rfc-docs)
    (current-buffer)))

(defun rfc-docs--parse-index-entry (string)
  "Parse the RFC document index entry STRING and return it as a plist."
  (unless (string-match "\\(^[0-9]+\\) *\\(.*?\\)\\.\\(?: \\|$\\)" string)
    (error "Invalid index entry format: %S" string))
  (let* ((number-string (match-string 1 string))
         (number (string-to-number number-string))
         (title (match-string 2 string))
         (entry nil))
    (cond
     ;; verify error
     ((not number)
      (error "Invalid index entry number: %S" number-string))
     ;; default, set entry
     (t (setq entry (list :number number :title title))))
    entry))

(defun rfc-docs--parse-ref (string)
  "Parse a reference to a RFC document from STRING.
For example: \"RFC 2822\"."
  (when (string-match "^RFC *\\([0-9]+\\)" string)
    (string-to-number (match-string 1 string))))

(defun rfc-docs--parse-refs (string)
  "Parse a list of references to RFC documents from STRING.
For example: \"RFC3401, RFC3402 ,RFC 3403\"."
  (seq-remove #'null (mapcar #'rfc-docs--parse-ref
                             (split-string string "," t " +"))))

(defun rfc-docs--parse-cadidate (entry)
  "Parse ENTRY to candidate string that will be used latter."
  (let* ((number (format "%s" (plist-get entry :number)))
         (title (truncate-string-to-width
                 (plist-get entry :title)
                 rfc-docs-entry-title-width))
         (candidate (format "%s|%s" number title)))
    candidate))

(defun rfc-docs-candidates ()
  "Return parsed candidates (string list) from `rfc-docs-index-entries'."
  (rfc-docs--fetch-document "-index" (rfc-docs-index-path))
  (unless rfc-docs-index-entries
    (setq rfc-docs-index-entries
          (rfc-docs-read-index-file (rfc-docs-index-path))))
  (if rfc-docs-index-entries
      (let ((candidates (mapcar
                         #'rfc-docs--parse-cadidate
                         rfc-docs-index-entries)))
        candidates)
    nil))

(defun rfc-docs-highlight-headers ()
  "Highlight RFC headers."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     (let* ((end (rfc-docs-next-header))
            (start (point)))
       (unless end (cl-return))
       (put-text-property
        start end 'face 'rfc-docs-header-face)
       (goto-char end)))))

(defun rfc-docs-highlight-titles ()
  "Highlight RFC titles."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp
            "^\\(?:[0-9]+\\.\\)+\\(?:[0-9]+\\)? .*$" nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (put-text-property
         start end 'face 'rfc-docs-section-title-face)
        (goto-char end)))))

(defun rfc-docs-highlight-references ()
  "Highlight RFC references."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "RFC *\\([0-9]+\\)" nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (number (string-to-number (match-string 1))))
        (make-text-button start end
                          'action `(lambda (button)
                                     (rfc-docs-read ,number))
                          'help-echo (format "Read RFC %d" number))
        (goto-char end)))))

(defun rfc-docs-highlight ()
  "Highlight the current buffer."
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (rfc-docs-highlight-headers)
      (rfc-docs-highlight-titles)
      (rfc-docs-highlight-references))))

(defun rfc-docs-index-path ()
  "Return he path of the file containing the index of all RFC documents."
  (concat rfc-docs-directory "rfc-index.txt"))

(defun rfc-docs-read-index-file (path)
  "Read an RFC index file at PATH and return a list of entries."
  (with-temp-buffer
    (insert-file-contents path)
    (rfc-docs-read-index (current-buffer))))

(defun rfc-docs-read-index (buffer)
  "Read an RFC index file from BUFFER and return a list of entries."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((entries nil))
      (while (search-forward-regexp "^[0-9]+ " nil t)
        (let ((start (match-beginning 0)))
          (search-forward-regexp " $")
          (let* ((end (match-beginning 0))
                 (lines (buffer-substring start end))
                 (entry-string (replace-regexp-in-string "[ \n]+" " " lines))
                 (entry (rfc-docs--parse-index-entry entry-string)))
            (unless (string= (plist-get entry :title) "Not Issued")
              (push entry entries)))))
      (nreverse entries))))

(defun rfc-docs-switch-to-buffer (number)
  "Switch to RFC buffer: document NUMBER."
  (switch-to-buffer (rfc-docs--document-buffer number)))

(defun rfc-docs-header-start ()
  "Move to the start of the current header.
When the point is on a linebreak character, move it to the start
of the current page header and return the position of the end of
the header."
  (when (looking-at "")
    (forward-line 1)
    (move-end-of-line 1)
    (let ((end (point)))
      (forward-line -2)
      (move-beginning-of-line 1)
      end)))

(defun rfc-docs-previous-header ()
  "Move the the start of the previous header.
Return the position of the end of the previous header or NIL if
no previous header is found."
  (when (search-backward "" nil t)
    (goto-char (match-beginning 0))
    (rfc-docs-header-start)))

(defun rfc-docs-next-header ()
  "Move the end of the previous header.
Return the position of the end of the next header or NIL if
no next header is found."
  (interactive)
  (when (search-forward "" nil t)
    (goto-char (match-beginning 0))
    (rfc-docs-header-start)))

(defun rfc-docs-quit ()
  "Quit the current window and bury its buffer."
  (interactive)
  (quit-window))

(defun rfc-docs-backward-page ()
  "Scroll to the previous page of the current buffer."
  (interactive)
  (backward-page)
  (rfc-docs-previous-header)
  (recenter 0))

(defun rfc-docs-forward-page ()
  "Scroll to the next page of the current buffer."
  (interactive)
  (forward-page)
  (rfc-docs-previous-header)
  (recenter 0))

(defun rfc-docs-reload-index ()
  "Reload the RFC document index from its original file."
  (interactive)
  (setq rfc-docs-index-entries
        (rfc-docs-read-index-file (rfc-docs-index-path))))

;;;###autoload
(defvar rfc-docs-keymap
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'rfc-docs-quit)
    (define-key map (kbd "SPC") 'rfc-docs-forward-page)
    (define-key map (kbd "<prior>") 'rfc-docs-backward-page)
    (define-key map (kbd "<next>") 'rfc-docs-forward-page)
    map)
  "The keymap for `rfc-docs'.")

;;;###autoload
(defun rfc-docs-open ()
  "Browse through all RFC documents referenced in the index."
  (interactive)
  (let ((candidates (rfc-docs-candidates)))
    (if candidates
        (rfc-docs-switch-to-buffer
         (string-to-number
          (completing-read "RFC: " candidates nil t)))
      (message "No candidates available"))))

;;;###autoload
(define-derived-mode rfc-docs text-mode "rfc-docs"
  "Major mode to browse and read RFC documents."
  (setq-local buffer-read-only t)
  (setq-local page-delimiter "^.*?\n")
  (rfc-docs-highlight))

;;;###autoload
(add-to-list 'auto-mode-alist '("/rfc[0-9]+\\.txt\\'" . rfc-docs))

(provide 'rfc-docs)
;;; rfc-docs.el ends here
