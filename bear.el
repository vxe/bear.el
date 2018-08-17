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
  "Where to put newl created notes."
  :group 'bear
  :type 'string)


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
