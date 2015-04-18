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
  "Return the id3 data as reported by the id3tool program.
Elements will typically include \"song title\", \"artist\",
\"album\" and \"track\"."
  (with-temp-buffer
    (call-process "id3tool" nil (current-buffer) nil file)
    (let ((data nil))
      (goto-char (point-min))
      (while (looking-at "\\([^:]+\\):[ \t]+\\(.*\\)")
	(push (cons (downcase (match-string 1))
		    (string-trim (match-string 2)))
	      data)
	(forward-line 1))
      (nreverse data))))

(defun id3-set-sata (file data)
  )
	    
(provide 'id3)

;;; id3.el ends here
