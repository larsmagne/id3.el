;;; id3.el --- functions for querying and setting id3 data in mp3 files

;; Copyright (C) 2015 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; id3.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; id3.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defun id3-get-data (file)
  "Return the id3 data.
Elements will typically include :track, :artist, :album, :year, :comment,
:track-number and :genre."
  (with-temp-buffer
    (let ((coding-system-for-read 'binary))
      (set-buffer-multibyte nil)
      (insert-file-contents file)
      (goto-char (- (buffer-size) 127))
      (when (looking-at "TAG")
	;; This is an id3v1 file.
	(id3-parse-id3v1 (buffer-substring (point) (point-max)))))))

(defun id3-parse-id3v1 (id3)
  (setq b id3)
  (let ((types '((:track 30)
		 (:artist 30)
		 (:album 30)
		 (:year 4)
		 (:comment 29)
		 (:track-number 1 :binary)
		 (:genre 1 :binary)))
	(start 3)
	(data nil))
    (dolist (type types)
      (let ((length (cadr type))
	    (format (caddr type)))
      (setq data (nconc data
			(list (car type)
			      (id3-chunk id3 start length format))))
      (cl-incf start length)))
    data))

(defun id3-chunk (id3 start length format)
  (let ((chunk (substring id3 start (+ start length))))
    (if (eq format :binary)
	;; Return the numeric value.
	(aref chunk 0)
      (replace-regexp-in-string "\0+\\'" "" chunk))))

(defun id3-set-sata (file data)
  )
	    
(provide 'id3)

;;; id3.el ends here
