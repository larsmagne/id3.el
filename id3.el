;; NOTE!  This has been barely tested and is liable to destroy your
;; precious mp3 files if you actually use it.

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

;; This package supports both id3v1 and id3v2 versions up to id3v2.4.

;; NOTE!  This has been barely tested and is liable to destroy your
;; precious mp3 files if you actually use it.

;; (setq auto-mode-alist (cons '("\\.mp3" . id3-mode) auto-mode-alist))

;;; Code:

(defvar id3-v1-format
  '(("TIT2" 30)
    ("TPE1" 30)
    ("TALB" 30)
    ("TYER" 4)
    ("COMM" 29)
    ("TRCK" 1 :binary)
    ("TCON" 1 :binary)))

(defun id3-get-data (file)
  "Return the id3 data.
Elements will typically include :track, :artist, :album, :year, :comment,
:track-number and :genre."
  (with-temp-buffer
    (let ((coding-system-for-read 'binary))
      (set-buffer-multibyte nil)
      (insert-file-contents file)
      (cond
       ((and (> (buffer-size) 10)
	     (goto-char (point-min))
	     (looking-at "ID3"))
	(id3-parse-id3v2))
       ((and (> (buffer-size) 128)
	     (goto-char (- (buffer-size) 127))
	     (looking-at "TAG"))
	;; This is an id3v1 file.
	(id3-parse-id3v1 (buffer-substring (point) (point-max))))))))

(defun id3-parse-id3v1 (id3)
  (let ((types id3-v1-format)
	(start 3)
	(data nil))
    (dolist (type types)
      (let ((name (pop type))
	    (length (pop type))
	    (format (pop type)))
	(push (list :frame-id name
		    :data (id3-v1-chunk id3 start length format))
	      data)
	(cl-incf start length)))
    (list :frames (nreverse data))))

(defun id3-v1-chunk (id3 start length format)
  (let ((chunk (substring id3 start (+ start length))))
    (if (eq format :binary)
	;; Return the numeric value.
	(aref chunk 0)
      (let ((string (replace-regexp-in-string "\0+\\'" "" chunk)))
	(decode-coding-string string (car (detect-coding-string string)))))))

(defun id3-parse-id3v2 ()
  (let ((header (id3-parse-header))
	frames)
    (when (plist-get (plist-get header :flags) :extended-header)
      (setq header (nconc header :extended-header (id3-parse-extended-header))))
    (let ((size (plist-get header :size)))
      (while (and (> size 0)
		  (not (= (id3-v2-chunk 4 :binary 8) 0)))
	(let ((frame (id3-parse-frame)))
	  (setq size (- size 10 (plist-get frame :size)))
	  (push frame frames))))
    (list :header header
	  :frames (nreverse frames))))

(defun id3-parse-frame ()
  (let ((header (id3-parse-chunk
		 '((:frame-id 4)
		   (:size 4 :binary)
		   (:flags 2 :bit-field (:tag-alter-preservation
					 :file-alter-preservation
					 :read-only
					 :reserved
					 :reserved
					 :reserved
					 :reserved
					 :reserved
					 :compression
					 :encryption
					 :grouping-identity))))))
    (when (id3-flag header :compression)
      (setq header (nconc header
			  (id3-parse-chunk '(:compression-length 4 :binary)))))
    (when (id3-flag header :encryption)
      (setq header (nconc header
			  (id3-parse-chunk '(:encryption-method 1 :binary)))))
    (when (id3-flag header :grouping-identity)
      (setq header (nconc header
			  (id3-parse-chunk '(:group-identity 1 :binary)))))
    (cond
     ((string-match "\\`T" (plist-get header :frame-id))
      (let ((data
	     (id3-parse-chunk
	      `((:text-encoding 1 :binary)
		(:data ,(1- (plist-get header :size)))))))
	(plist-put data :data
		   (decode-coding-string
		    (plist-get data :data)
		    (case (plist-get data :text-encoding)
		      (0 'iso-8859-1)
		      (1 'utf-16)
		      (2 'utf-16)
		      (3 'utf-8))))
	(nconc header data)))
     ((equal (plist-get header :frame-id) "APIC")
      (let ((data (plist-get
		   (id3-parse-chunk
		    `((:data ,(plist-get header :size))))
		   :data)))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (insert data)
	  (goto-char (point-min))
	  (let ((encoding (following-char)))
	    (forward-char 1)
	    (let ((mime-type (buffer-substring (point)
					       (re-search-forward "\0")))
		  (picture-type (prog1
				    (following-char)
				  (forward-char 1)))
		  (description (buffer-substring (point)
						 (re-search-forward "\0")))
		  (image (buffer-substring (point) (point-max))))
	      (nconc header
		     `(:image ,image
			      :description ,description
			      :picture-type ,picture-type
			      :mime-type ,mime-type
			      :encoding ,encoding)))))))
     (t
      (nconc header
	     (id3-parse-chunk
	      `((:data ,(plist-get header :size)))))))))

(defun id3-flag (header name)
  (plist-get (plist-get header :flags) name))

(defun id3-parse-extended-header ()
  (let ((data (id3-parse-chunk
	       '((:extended-header-size 4 :binary)
		 (:extended-flags 2 :bit-field '(:crc-data-present))
		 (:padding-size 4 :binary)))))
    (when (plist-get (plist-get data :extended-flags) :crc-data-present)
      (setq data (nconc data (id3-parse-chunk
			      '((:total-frame-crc 4 :binary))))))
    data))

(defun id3-parse-header ()
  (id3-parse-chunk
   '((:identifier 3)
     (:version-major 1 :binary)
     (:version-minor 1 :binary)
     (:flags 1 :bit-field (:unsynchronisation
			   :extended-header
			   :experimental))
     (:size 4 :binary 7))))

(defun id3-parse-chunk (types)
  (let ((data nil))
    (dolist (type types)
      (let ((name (pop type))
	    (length (pop type))
	    (format (or (pop type) :text))
	    (extra (pop type)))
	(setq data (nconc data
			  (list name (id3-v2-chunk length format extra))))
	(forward-char length)))
    data))

(defun id3-v2-chunk (length format extra)
  (cond
   ((eq format :text)
    (buffer-substring (point) (+ (point) length)))
   ((eq format :binary)
    (id3-read-number length extra))
   ((eq format :bit-field)
    (let ((number (id3-read-number length 8))
	  (i (1- (* length 8)))
	  fields)
      (dolist (field extra)
	(setq fields (nconc fields
			    (list field (not (zerop
					      (logand number (expt 2 i)))))))
	(decf i))
      fields))
   (t
    (error "Unknown format %s" format))))    

(defun id3-read-number (length bits)
  (let ((number 0)
	(i 0))
    (while (< i length)
      (setq number (+ (* number (expt 2 (or bits 8)))
		      (char-after (+ (point) i))))
      (incf i))
    number))

(defun id3-insert-v1-tags (data)
  (save-excursion
    (goto-char (point-max))
    (insert "TAG")
    (dolist (type id3-v1-format)
      (let* ((name (pop type))
	     (length (pop type))
	     (format (or (pop type) :text))
	     (value (encode-coding-string
		     (or (cdr (assoc name data)) "")
		     'iso-8859-1)))
	(id3-insert-data length format value)))))

(defun id3-insert-v2-tags (data charset)
  (save-excursion
    (goto-char (point-min))
    (let (tags)
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(dolist (elem data)
	  (let ((text
		 (with-temp-buffer
		   (set-buffer-multibyte nil)
		   (id3-insert-data nil :text (cdr elem) charset)
		   (buffer-string))))
	    (id3-insert-data 4 :text (car elem))
	    (id3-insert-data 4 :binary (length text))
	    (id3-insert-data 2 :binary 0)
	    (insert text)))
	(setq tags (buffer-string)))
      (insert "ID3")
      (id3-insert-data 1 :binary 3)
      (id3-insert-data 1 :binary 0)
      (id3-insert-data 1 :binary 0)
      (id3-insert-data 4 :binary (length tags) 7)
      (insert tags))))

(defun id3-insert-data (length format data &optional extra)
  (cond
   ((eq format :text)
    (cond
     ((null length)
      ;; This is a v2 string, so we need to encode the charset.
      (let ((coding-system
	     (cond
	      ((string-match "\\`[[:ascii:]]*\\'" data)
	       (id3-insert-data 1 :binary 0)
	       nil)
	      ((eq extra 'iso-8859-1)
	       (id3-insert-data 1 :binary 0)
	       'iso-8859-1)
	      ((eq extra 'utf-16)
	       (id3-insert-data 1 :binary 1)
	       'utf-16)
	      (t
	       ;; utf-8
	       (id3-insert-data 1 :binary 3)
	       'utf-8))))
	(insert (encode-coding-string data coding-system))))
     ((> (length data) length)
      (insert (substring data 0 length)))
     (t
      (insert data)
      (insert (make-string (- length (length data)) ?\0)))))
   ((eq format :binary)
    (let ((number (if (stringp data)
		      (string-to-number data)
		    data)))
      (id3-insert-binary number length (or extra 8))))
   (t
    (error "No such format: %s" format))))

(defun id3-insert-binary (number length bits)
  (dotimes (i length)
    (insert
     (logand (lsh number (- (* bits (- length i 1))))
	     (1- (expt 2 bits))))))

(defun id3-delete-tags ()
  ;; First remove id3v2 tags from the beginning of the buffer.
  (when (and (> (buffer-size) 10)
	     (goto-char (point-min))
	     (looking-at "ID3"))
    (let ((data (id3-parse-id3v2)))
      (goto-char (point-min))
      (delete-region (point) (+ (point) 10 (plist-get (plist-get data :header)
						      :size)))))
  ;; Then id3v1 tags from the end.
  (when (and (> (buffer-size) 128)
	     (goto-char (- (buffer-size) 127))
	     (looking-at "TAG"))
    (delete-region (point) (point-max))))

(defvar id3-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'id3-save)
    (easy-menu-define nil map ""
      '("id3"
	["Save" id3-save t]))))

(defcustom id3-charset 'utf-8
  "Charset to use on non-ASCII text parts."
  :type '(choice (const :tag 'utf-8)
		 (const :tag 'iso-8859-1)
		 (const :tag 'utf-16)))

(define-derived-mode id3-mode text-mode "id3"
  "Mode for editing id3 tags in mp3 files."
  (setq-local id3-charset id3-charset)
  (setq-local write-file-functions 'id3-save)
  (id3-display-data)
  (set-buffer-modified-p nil)
  (setq-local backup-inhibited nil))

(defun id3-display-data ()
  (let ((data (id3-get-data buffer-file-name)))
    (buffer-disable-undo)
    (erase-buffer)
    (dolist (frame (plist-get data :frames))
      (insert
       (format
	"%s %s\n"
	(propertize
	 (format "%s:" (plist-get frame :frame-id))
	 'face '(:foreground "red"))
	(if (equal (plist-get frame :frame-id) "APIC")
	    (propertize " "
			'display
			(create-image (plist-get frame :image)
				      'imagemagick t))
	  (plist-get frame :data)))))
    (buffer-enable-undo)
    (goto-char (point-min))))

(defun id3-save ()
  "Update the id3 data of the mp3 file."
  (interactive)
  (let ((file buffer-file-name)
	(data (id3-parse-mode-data)))
    (backup-buffer)
    (with-temp-buffer
      (let ((coding-system-for-read 'binary))
	(set-buffer-multibyte nil)
	(insert-file-contents file)
	(id3-delete-tags)
	(id3-insert-v1-tags data)
	(id3-insert-v2-tags data id3-charset)
	(write-region (point-min) (point-max) file nil)))
    (set-buffer-modified-p nil))
  t)

(defun id3-parse-mode-data ()
  (let ((data nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([A-Z0-9]+\\): *\\(.*\\)" nil t)
	(push (cons (match-string 1) (match-string 2)) data)))
    (nreverse data)))

(provide 'id3)

;;; id3.el ends here
