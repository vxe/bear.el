;;; bear.el --- x-callback-url wrapper for bear on OSX

;; Copyright (C) 2018 Vijay Edwin

;; Author: Vijay Edwin <vedwin@vijays-mbp.localhost>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; x-callback-url wrapper for bear on OSX

;;; Code:

(defgroup bear nil
  "Interact with bear through emacs."
  :group 'tools
  :group 'convenience)

(defcustom bear:notes-dir "~/"
  "Where to put newly created notes."
  :group 'bear
  :type 'string)

(defun bear:convert-to-markdown ()
  "Convert the current buffer's content from orgmode to markdown format and save it with the current buffer's file name but with .md extension."
    (interactive)
    (progn (shell-command-on-region (point-min) (point-max)
                                    (format "pandoc -f org -t markdown -o %s"
                                            (concat (file-name-sans-extension (buffer-file-name)) ".md")))
           (find-file (concat (file-name-sans-extension (buffer-file-name)) ".md"))
           (goto-char (point-min))
           (end-of-line)
           (delete-backward-char 1)
           (goto-char (point-max))
           (bear-mode)))

(defun bear:convert-to-markdown-and-push ()
  "Convert the current buffer's content from orgmode to markdown format and save it with the current buffer's file name but with .md extension."
    (interactive)
    (progn (shell-command-on-region (point-min) (point-max)
                                    (format "pandoc -f org -t markdown -o %s"
                                            (concat (file-name-sans-extension (buffer-file-name)) ".md")))
           (find-file (concat (file-name-sans-extension (buffer-file-name)) ".md"))
           (goto-char (point-min))
           (end-of-line)
           (delete-backward-char 1)
           (bear:push)))

(defun bear:get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (interactive)
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun bear:create-note (title tag-string)
  "Create a note with TITLE and TAG-STRING in bear:notes-dir."
  (interactive "stitle: \nstag(s): ")
  (with-temp-buffer
    (cd bear:notes-dir)
    (find-file (s-replace-regexp " " "-" (concat bear:notes-dir title ".md")))
    (insert (concat title))
    (insert "\n")
    (insert tag-string)
    (bear-mode)))

(defun bear:create-note-org (title tag-string)
  "Create a note with TITLE and TAG-STRING in bear:notes-dir."
  (interactive "stitle: \nstag(s): ")
  (with-temp-buffer
    (cd bear:notes-dir)
    (find-file (s-replace-regexp " " "-" (concat bear:notes-dir title ".org")))
    (insert (concat title "\\\\"))
    (insert "\n")
    (insert tag-string)
    (insert "\n")
    (bear-mode)))

(defun bear:encode-note (file)
  "Encode the note of name FILE."
  (let ((note (base64-encode-string (bear:get-string-from-file
                                     (concat bear:notes-dir file)))))))

(defun bear:push ()
  "Upload the curent buffer to Bear."
  (interactive)
  (save-buffer)
  (let* ((file-name (buffer-name))
         (note-title (s-chomp (shell-command-to-string (concat "cat"
                                                               " "
                                                               bear:notes-dir
                                                               file-name
                                                               "|"
                                                               "head -1"
                                                               ))))
         (tag-string (s-chomp (shell-command-to-string (concat "cat"
                                                               " "
                                                               bear:notes-dir
                                                               file-name
                                                               "|"
                                                               "head -2"
                                                               "| "
                                                               "tail -1"
                                                               ))))
         (contents (s-chomp (shell-command-to-string (concat "cat"
                                                             " "
                                                             bear:notes-dir
                                                             file-name
                                                             "|"
                                                             "tail -n +3")))))
    (async-shell-command (concat "open"
                                 " "
                                 "'"
                                 "bear://x-callback-url/create?title="
                                 note-title
                                 "&"
                                 "tags="
                                 (if (not (string= "" tag-string))
                                     (url-encode-url tag-string)
                                   "emacs")
                                 "&text="
                                 (s-replace-regexp "[']" "\"" contents)
                                 "'"))))

(defun bear:push-note ()
  "Select a note from bear notes directory."
  (interactive)
  (with-temp-buffer
    (cd bear:notes-dir)
    (let* ((file-name (completing-read "note: " (s-split "\n" (s-chomp (shell-command-to-string (concat "find"
                                                                                                        " "
                                                                                                        bear:notes-dir
                                                                                                        " "
                                                                                                        "-maxdepth 1"
                                                                                                        " "
                                                                                                        "-name '*.md'"
                                                                                                        " "
                                                                                                        "-type f"
                                                                                                        " "
                                                                                                        "-exec basename {} \\;"))))))
           (note-title (s-chomp (shell-command-to-string (concat "cat"
                                                                 " "
                                                                 file-name
                                                                 "|"
                                                                 "head -1"
                                                                 ))))
           (tag-string (s-chomp (shell-command-to-string (concat "cat"
                                                                 " "
                                                                 file-name
                                                                 "|"
                                                                 "head -2"
                                                                 "| "
                                                                 "tail -1"
                                                                 ))))
           (contents (s-chomp (shell-command-to-string (concat "cat"
                                                               " "
                                                               file-name
                                                               "|"
                                                               "tail -n +3")))))
      (async-shell-command (concat "open"
                                   " "
                                   "'"
                                   "bear://x-callback-url/create?title="
                                   note-title
                                   "&"
                                   "tags="
                                   (if (not (string= "" tag-string))
                                       (url-encode-url tag-string)
                                     "emacs")
                                   "&text="
                                   (s-replace-regexp "[']" "\"" contents)
                                   "'")))))


(defun bear:quick-note (title tags)
  "Create a new note with a TITLE and TAGS string only."
  (interactive "stitle: \nstags-string: ")
  (async-shell-command (concat "open "
                               "'"
                               "bear://x-callback-url/create?title="
                               (url-encode-url title)
                               "&"
                               "tags="
                               (if (not (string= "" tags))
                                   (url-encode-url tags)
                                 "emacs")
                               "'")))

(define-minor-mode bear-mode
  "Make editing bear notes smoother."
  :lighter " bear"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'bear:push)
            (define-key map (kbd "C-c C") 'bear:convert-to-markdown)
            map)
  (make-local-variable 'foo-count))

(provide 'bear)

;;; bear.el ends here
