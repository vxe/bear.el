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


(defun bear:get-string-from-file (filePath)
  "Return filePath's file content. http://ergoemacs.org/emacs/elisp_read_file_content.html"
  (interactive)
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun bear:create-note (title tags)
  "Start a new markdown note with TITLE."
  (interactive "stitle:\n stags: ")
  (find-file (concat bear:notes-dir title ".md")))

(defun bear:encode-note (file)
  "Encode the note of name FILE."
  (let ((note (base64-encode-string (bear:get-string-from-file
                                     (concat bear:notes-dir file)))))))

(defun bear:push-note (title tags)
  (interactive "stitle: \nstags-string: ")
  (find-file (concat bear:notes-dir title ".md"))
  (let ((completed-yet (read-string "completed? "))) ;; so this kind of works, but what we really need is a minor mode for bear, and then for the on save hook, get the tags and title somehow??
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
           (encoded-file (bear:encode-note file-name)))
      (with-temp-buffer
        (cd bear:notes-dir)
        (async-shell-command (concat "open"
                                     " "
                                     "'"
                                     "bear://x-callback-url/create?title="
                                     (url-encode-url title)
                                     "&"
                                     "tags="
                                     (if (not (string= "" tags))
                                         (url-encode-url tags)
                                       "emacs")
                                     "&file="
                                     encoded-file
                                     "&filename="
                                     file-name
                                     "'"))))))


(defun bear:new-note (title tags)
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

(provide 'bear)

;;; bear.el ends here
