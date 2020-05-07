;;; rfc-mode.el --- RFC document browser and viewer
;;
;; -*- lexical-binding: t -*-
;;
;; Author: Nicolas Martyanoff <khaelin@gmail.com>
;; URL: https://github.com/galdor/rfc-mode
;; Version: 1.2.0
;;
;; Modify by: esac <esac-io@tutanota.com>
;; URL: https://github.com/esac-io/rfc-mode
;; Package-Version: 1.0
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
;; IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;
;;; Commentary:
;;
;; This package makes it easy to browse to search/read RFC documents,
;; helm dependency from its original code was removed,
;; the minibuffer with the standards completions mechanism
;; are more then sufficient for me.
;;
;; PS: icomplete rules!
;;
;;; Code:

(require 'seq)

(defgroup rfc-mode-group nil
  "Tools to browse and read RFC documents."
  :prefix "rfc-mode-"
  :link '(url-link :tag "GitHub" "https://github.com/esac-io/rfc-mode")
  :group 'external)

(defface rfc-mode-document-header-face
  '((t :inherit font-lock-comment-face))
  "Face used for RFC document page headers.")

(defface rfc-mode-document-section-title-face
  '((t :inherit font-lock-keyword-face))
  "Face used for RFC document section titles.")

(defcustom rfc-mode-directory
  (expand-file-name "rfc/" user-emacs-directory)
  "The directory where RFC documents are stored."
  :type 'directory)

(defcustom rfc-mode-document-url
  "https://www.rfc-editor.org/rfc/rfc%s.txt"
  "A `format'able URL for fetching arbitrary RFC documents.
Assume RFC documents are named as e.g. rfc21.txt, rfc-index.txt."
  :type 'string)

(defcustom rfc-mode-use-original-buffer-names nil
  "Whether RFC document buffers should keep their original name or not."
  :type 'boolean)

(defcustom rfc-mode-browser-entry-title-width 60
  "The width of the column containing RFC titles in the browser."
  :type 'integer)

(defvar rfc-mode-index-entries nil
  "The list of entries in the RFC index.")

(defvar rfc-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'rfc-mode-quit)
    (define-key map (kbd "SPC") 'rfc-mode-forward-page)
    (define-key map (kbd "<prior>") 'rfc-mode-backward-page)
    (define-key map (kbd "<next>") 'rfc-mode-forward-page)
    map)
  "The keymap for `rfc-mode'.")

(defun rfc-mode-init ()
  "Initialize the current buffer for `rfc-mode'."
  (setq-local buffer-read-only t)
  (setq-local page-delimiter "^.*?\n")
  (rfc-mode-highlight))

(defun rfc-mode-quit ()
  "Quit the current window and bury its buffer."
  (interactive)
  (quit-window))

(defun rfc-mode-backward-page ()
  "Scroll to the previous page of the current buffer."
  (interactive)
  (backward-page)
  (rfc-mode-previous-header)
  (recenter 0))

(defun rfc-mode-forward-page ()
  "Scroll to the next page of the current buffer."
  (interactive)
  (forward-page)
  (rfc-mode-previous-header)
  (recenter 0))

(defun rfc-mode-switch-to-buffer (number)
  "Switch to RFC buffer: document NUMBER."
  (switch-to-buffer (rfc-mode--document-buffer number)))

;;;###autoload
(defun rfc-mode-read (number)
  "Open RFC document NUMBER."
  (interactive "nRFC number: ")
  (rfc-mode-switch-to-buffer number))

;;;###autoload
(defun rfc-mode-browse ()
  "Browse through all RFC documents referenced in the index."
  (interactive)
  (rfc-mode--fetch-document "-index" (rfc-mode-index-path))
  (unless rfc-mode-index-entries
    (setq rfc-mode-index-entries
      (rfc-mode-read-index-file (rfc-mode-index-path))))
  (let ((candidates (rfc-mode--get-candidates)))
    (if candidates
      (rfc-mode-switch-to-buffer
        (string-to-number
          (completing-read "RFC: " candidates nil t)))
      (message "No candidates available"))))

;;;###autoload
(define-derived-mode rfc-mode text-mode "rfc-mode"
  "Major mode to browse and read RFC documents."
  (rfc-mode-init))

;;;###autoload
(add-to-list 'auto-mode-alist '("/rfc[0-9]+\\.txt\\'" . rfc-mode))

(defun rfc-mode-reload-index ()
  "Reload the RFC document index from its original file."
  (interactive)
  (setq rfc-mode-index-entries
    (rfc-mode-read-index-file (rfc-mode-index-path))))

(defun rfc-mode-highlight ()
  "Highlight the current buffer."
  (with-silent-modifications
    (let ((inhibit-read-only t))
      ;; Headers
      (save-excursion
        (goto-char (point-min))
        (cl-loop
          (let* ((end (rfc-mode-next-header))
                  (start (point)))
            (unless end
              (cl-return))
            (put-text-property start end
              'face 'rfc-mode-document-header-face)
            (goto-char end))))
      ;; Section titles
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp "^\\(?:[0-9]+\\.\\)+\\(?:[0-9]+\\)? .*$" nil t)
          (let ((start (match-beginning 0))
                 (end (match-end 0)))
            (put-text-property start end
              'face 'rfc-mode-document-section-title-face)
            (goto-char end))))
      ;; RFC references
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp "RFC *\\([0-9]+\\)" nil t)
          (let ((start (match-beginning 0))
                 (end (match-end 0))
                 (number (string-to-number (match-string 1))))
            (make-text-button start end
              'action `(lambda (button)
                         (rfc-mode-read ,number))
              'help-echo (format "Read RFC %d" number))
            (goto-char end)))))))

(defun rfc-mode-header-start ()
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

(defun rfc-mode-previous-header ()
  "Move the the start of the previous header.
Return the position of the end of the previous header or NIL if
no previous header is found."
  (when (search-backward "" nil t)
    (goto-char (match-beginning 0))
    (rfc-mode-header-start)))

(defun rfc-mode-next-header ()
  "Move the end of the previous header.
Return the position of the end of the next header or NIL if
no next header is found."
  (interactive)
  (when (search-forward "" nil t)
    (goto-char (match-beginning 0))
    (rfc-mode-header-start)))

(defun rfc-mode--get-candidates ()
  "Construct candidates string list from `rfc-mode-index-entries'."
  (if rfc-mode-index-entries
    (let ((candidates
            (mapcar
              #'rfc-mode--parse-cadidate
              rfc-mode-index-entries)))
      candidates)
    nil))

(defun rfc-mode--parse-cadidate (entry)
  "Parse ENTRY to candidate string that will be used latter."
  (let* ((number (format "%s" (plist-get entry :number)))
          (title (truncate-string-to-width
                   (plist-get entry :title)
                   rfc-mode-browser-entry-title-width))
          (candidate (format "%s|%s" number title)))
    candidate))

(defun rfc-mode-index-path ()
  "Return he path of the file containing the index of all RFC documents."
  (concat rfc-mode-directory "rfc-index.txt"))

(defun rfc-mode-read-index-file (path)
  "Read an RFC index file at PATH and return a list of entries."
  (with-temp-buffer
    (insert-file-contents path)
    (rfc-mode-read-index (current-buffer))))

(defun rfc-mode-read-index (buffer)
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
                  (entry (rfc-mode-parse-index-entry entry-string)))
            (unless (string= (plist-get entry :title) "Not Issued")
              (push entry entries)))))
      (nreverse entries))))

(defun rfc-mode-parse-index-entry (string)
  "Parse the RFC document index entry STRING and return it as a plist."
  (unless (string-match "\\(^[0-9]+\\) *\\(.*?\\)\\.\\(?: \\|$\\)" string)
    (error "Invalid index entry format: %S" string))
  (let* ((number-string (match-string 1 string))
          (number (string-to-number number-string))
          (title (match-string 2 string)))
    (unless number
      (error "Invalid index entry number: %S" number-string))
    (let ((entry (list :number number :title title)))
      (when (string-match "(Status: \\([^)]+\\))" string)
        (plist-put entry :status (downcase (match-string 1 string))))
      (when (string-match "(Obsoletes \\([^)]+\\))" string)
        (plist-put entry :obsoletes
          (rfc-mode--parse-rfc-refs (match-string 1 string))))
      (when (string-match "(Obsoleted by \\([^)]+\\))" string)
        (plist-put entry :obsoleted-by
          (rfc-mode--parse-rfc-refs (match-string 1 string))))
      (when (string-match "(Updates \\([^)]+\\))" string)
        (plist-put entry :updates
          (rfc-mode--parse-rfc-refs (match-string 1 string))))
      (when (string-match "(Updated by \\([^)]+\\))" string)
        (plist-put entry :updated-by
          (rfc-mode--parse-rfc-refs (match-string 1 string))))
      entry)))

(defun rfc-mode--document-buffer-name (number)
  "Return the buffer name for the RFC document NUMBER."
  (concat "*rfc" (number-to-string number) "*"))

(defun rfc-mode--document-path (number)
  "Return the absolute path of the RFC document NUMBER."
  (expand-file-name (format "rfc%s.txt" number) rfc-mode-directory))

(defun rfc-mode--document-buffer (number)
  "Return a buffer visiting the RFC document NUMBER.
The buffer is created if it does not exist."
  (let* ((buffer-name (rfc-mode--document-buffer-name number))
          (document-path (rfc-mode--document-path number)))
    (rfc-mode--fetch-document number document-path)
    (find-file document-path)
    (unless rfc-mode-use-original-buffer-names
      (rename-buffer buffer-name))
    (read-only-mode 1)
    (rfc-mode)
    (current-buffer)))

(defun rfc-mode--fetch-document (suffix document-path)
  "Ensure an RFC document with SUFFIX exists at DOCUMENT-PATH.
If no such file exists, fetch it from `rfc-document-url'."
  (rfc-mode--ensure-directory-exists)
  (unless (file-exists-p document-path)
    (url-copy-file (format rfc-mode-document-url suffix) document-path)))

(defun rfc-mode--ensure-directory-exists ()
  "Create `rfc-mode-directory' if does not exists."
  (when (and (not (file-exists-p rfc-mode-directory))
          (y-or-n-p (format "Create directory %s? " rfc-mode-directory)))
    (make-directory rfc-mode-directory t)))

(defun rfc-mode--parse-rfc-ref (string)
  "Parse a reference to a RFC document from STRING.
For example: \"RFC 2822\"."
  (when (string-match "^RFC *\\([0-9]+\\)" string)
    (string-to-number (match-string 1 string))))

(defun rfc-mode--parse-rfc-refs (string)
  "Parse a list of references to RFC documents from STRING.
For example: \"RFC3401, RFC3402 ,RFC 3403\"."
  (seq-remove #'null (mapcar #'rfc-mode--parse-rfc-ref
                       (split-string string "," t " +"))))

(provide 'rfc-mode)
;;; rfc-mode.el ends here
