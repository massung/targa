;;;; Targa Image Loading for Common Lisp
;;;;
;;;; Copyright (c) 2015 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :targa
  (:use :cl)
  (:nicknames :tga)
  (:export

   ;; reading and loading a targa
   #:tga-read
   #:tga-load

   ;; pixel access functions
   #:tga-get-pixel

   ;; accessor functions
   #:tga-header
   #:tga-image-id
   #:tga-color-map
   #:tga-pixels
   #:tga-extension
   #:tga-developer-tags

   ;; header reader functions
   #:tga-header-id-length
   #:tga-header-color-map-type
   #:tga-header-image-type
   #:tga-header-color-map-first-entry
   #:tga-header-color-map-length
   #:tga-header-color-map-entry-size
   #:tga-header-x-origin
   #:tga-header-y-origin
   #:tga-header-width
   #:tga-header-height
   #:tga-header-pixel-size
   #:tga-header-image-descriptor
   #:tga-header-color-map-p
   #:tga-header-rle-compressed-p
   #:tga-header-alpha-size
   #:tga-header-screen-x-origin
   #:tga-header-screen-y-origin

   ;; extension reader functions
   #:tga-extension-size
   #:tga-extension-author
   #:tga-extension-comments
   #:tga-extension-date
   #:tga-extension-time
   #:tga-extension-job-id
   #:tga-extension-job-time
   #:tga-extension-software-id
   #:tga-extension-software-version
   #:tga-extension-key-color
   #:tga-extension-aspect-ratio
   #:tga-extension-gamma
   #:tga-extension-color-correction-offset
   #:tga-extension-postage-stamp-offset
   #:tga-extension-scan-line-offset
   #:tga-extension-attributes-type
   #:tga-extension-scan-line-table
   #:tga-extension-postage-stamp-image
   #:tga-extension-color-correction-table

   ;; developer tag reader functions
   #:tga-developer-tag-id
   #:tga-developer-tag-offset
   #:tga-developer-tag-size
   #:tga-developer-tag-bytes))

(in-package :targa)

;;; -----------------------------------------------------

(defstruct tga
  "A parsed targa image file."
  (header         nil :read-only t)
  (image-id       nil :read-only t)
  (color-map      nil :read-only t)
  (pixels         nil :read-only t)
  (extension      nil :read-only t)
  (developer-tags nil :read-only t))

(defmethod print-object ((tga tga) stream)
  "Output a parsed targa to a stream."
  (print-unreadable-object (tga stream :type t)
    (with-slots (image-type (w width) (h height) (bpp pixel-size))
        (tga-header tga)
      (format stream "~a bit, ~ax~a, ~a" bpp w h (case image-type
                                                   (#b0000 :no-image)
                                                   (#b0001 :color-mapped-image)
                                                   (#b0010 :true-color-image)
                                                   (#b0011 :black-and-white-image)
                                                   (#b1001 :rle-color-mapped-image)
                                                   (#b1010 :rle-true-color-image)
                                                   (#b1011 :rle-black-and-white-image))))))

(defun tga-get-pixel (tga x y)
  "Lookup a pixel in the image based on the screen origin of the targa."
  (with-slots (header pixels)
      tga
    (let ((w (tga-header-width header))
          (h (tga-header-height header)))

      ;; ensure the pixel is within the bounds of the image
      (when (and (<= 0 x (1- w))
                 (<= 0 y (1- h)))

        ;; the origin of the image may not be the top-right
        (when (eq (tga-header-screen-x-origin header) :right)
          (setf x (- w x 1)))
        (when (eq (tga-header-screen-y-origin header) :bottom)
          (setf y (- h y 1)))

        ;; the pixels are stored row-major
        (aref (aref pixels y) x)))))

;;; -----------------------------------------------------

(defstruct tga-header
  "The parsed header of a Targa file."
  (id-length             nil :read-only t)
  (color-map-type        nil :read-only t)
  (color-map-first-entry nil :read-only t)
  (color-map-length      nil :read-only t)
  (color-map-entry-size  nil :read-only t)
  (image-type            nil :read-only t)
  (x-origin              nil :read-only t)
  (y-origin              nil :read-only t)
  (width                 nil :read-only t)
  (height                nil :read-only t)
  (pixel-size            nil :read-only t)
  (image-descriptor      nil :read-only t))

(defun read-tga-header (stream)
  "Read a targa header from an input stream."
  (make-tga-header
   :id-length (read-byte stream)
   :color-map-type (read-byte stream)
   :image-type (read-byte stream)
   :color-map-first-entry (read-short stream)
   :color-map-length (read-short stream)
   :color-map-entry-size (read-byte stream)
   :x-origin (read-short stream)
   :y-origin (read-short stream)
   :width (read-short stream)
   :height (read-short stream)
   :pixel-size (read-byte stream)
   :image-descriptor (read-byte stream)))

(defun tga-header-color-map-p (h)
  "T if the image has a color map."
  (plusp (tga-header-color-map-type h)))

(defun tga-header-rle-compressed-p (h)
  "T if the image is run-length-encoded."
  (plusp (logand (tga-header-image-type h) #b1000)))

(defun tga-header-alpha-size (h)
  "Return the number of alpha bits per pixel."
  (logand (tga-header-image-descriptor h) #b1111))

(defun tga-header-screen-x-origin (h)
  "Return the origin of each scanline - :left or :right."
  (if (zerop (logand (tga-header-image-descriptor h) #b010000)) :left :right))

(defun tga-header-screen-y-origin (h)
  "Return the origin of the image - :bottom or :top."
  (if (zerop (logand (tga-header-image-descriptor h) #b100000)) :bottom :top))

;;; -----------------------------------------------------

(defstruct tga-extension
  "A parse extension in a targa stream."
  (size                    nil :read-only t)
  (author                  nil :read-only t)
  (comments                nil :read-only t)
  (date                    nil :read-only t)
  (time                    nil :read-only t)
  (job-id                  nil :read-only t)
  (job-time                nil :read-only t)
  (software-id             nil :read-only t)
  (software-version        nil :read-only t)
  (key-color               nil :read-only t)
  (aspect-ratio            nil :read-only t)
  (gamma                   nil :read-only t)
  (color-correction-offset nil :read-only t)
  (postage-stamp-offset    nil :read-only t)
  (scan-line-offset        nil :read-only t)
  (attributes-type         nil :read-only t)
  (scan-line-table         nil :read-only t)
  (postage-stamp-image     nil :read-only t)
  (color-correction-table  nil :read-only t))

(defun read-tga-extension (stream offset)
  "Parse an extension from an input stream at a given offset."
  (when (file-position stream offset)
    (make-tga-extension
     :size (read-short stream)
     :author (read-string stream 41)
     :comments (read-string stream 324)
     :date (read-date/time stream)
     :time (read-date/time stream)
     :job-id (read-string stream 41)
     :job-time (read-date/time stream)
     :software-id (read-string stream 41)
     :software-version (read-version stream)
     :key-color (read-long stream)
     :aspect-ratio (let ((n (read-short stream))
                         (d (read-short stream)))
                     (if (zerop d) nil (/ n d)))
     :gamma (read-long stream)
     :color-correction-offset (read-long stream)
     :postage-stamp-offset (read-long stream)
     :scan-line-offset (read-long stream)
     :attributes-type (read-byte stream))))

;;; -----------------------------------------------------

(defstruct tga-developer-tag
  "A developer tag parsed from a targa stream."
  (id     nil :read-only t)
  (offset nil :read-only t)
  (size   nil :read-only t)
  (bytes  nil :read-only t))

(defmethod print-object ((tag tga-developer-tag) stream)
  "Output a developer tag to a stream."
  (print-unreadable-object (tag stream :type t)
    (format stream "ID ~a" (tga-developer-tag-id tag))))

(defun read-tga-developer-tags (stream offset)
  "Read all the developer tags from an input targa stream."
  (when (file-position stream offset)
    (loop for i below (read-short stream)

          ;; read the initial values for each tag
          for id = (read-short stream)
          for offset = (read-long stream)
          for size = (read-long stream)
          
          ;; read each tag
          collect (make-tga-developer-tag :id id :offset offset :size size)
          into tags
          
          ;; read the byte data for each tag and return all the tags
          finally (dolist (tag tags tags)
                    (with-slots (offset size bytes)
                        tag
                      (when (file-position stream offset)
                        (setf bytes (read-bytes stream size))))))))

;;; -----------------------------------------------------

(defun read-short (s)
  "Read an unsigned, little-endian, 16-bit short."
  (logior (read-byte s) (ash (read-byte s) 8)))

(defun read-long (s)
  "Read an unsigned, little-endian, 32-bit long."
  (logior (read-short s) (ash (read-short s) 16)))

(defun read-bytes (s len)
  "Read a sequence of bytes from the stream."
  (let ((bytes (make-array len :element-type '(unsigned-byte 8))))
    (prog1 bytes
      (read-sequence bytes s))))

(defun read-string (s len)
  "Read a null-terminated string of a given length."
  (let ((bytes (read-bytes s len)))
    (let ((i (find 0 bytes)))
      (map 'string #'code-char (if (null i)
                                   bytes
                                 (subseq bytes 0 i))))))

(defun read-date/time (s)
  "Reads three (3) shorts from the stream."
  (list (read-short s)
        (read-short s)
        (read-short s)))

(defun read-version (s)
  "Reads a 2-byte version and 1-byte character from the stream."
  (list (read-short s) (code-char (read-byte s))))

(defun read-image-id (s h)
  "Read the tga image id as a string from the stream."
  (read-string s (tga-header-id-length h)))

(defun read-color (s pixel-size alpha-size)
  "Read a true color (or black and white) entry from either the color map or the image data."
  (loop with color-size = (min (truncate pixel-size 3) 8)

        ;; calculate the maximum value for each color component and the alpha
        with pixel-max = (1- (ash 1 pixel-size))
        with color-max = (1- (ash 1 color-size))
        with alpha-max = (1- (ash 1 alpha-size))

        ;; read each component channel into the final color
        for channel below pixel-size by 8
        sum (ash (read-byte s) channel) into c

        ;; convert the final color into a list of floats
        finally (return (if (<= pixel-size 8)
                            (let ((g (float (/ (ldb (byte pixel-size 0) c) pixel-max))))
                              (list g g g (if (zerop alpha-max)
                                              1.0
                                            (float (/ (ldb (byte alpha-size pixel-size) c) alpha-max)))))
                          (let ((b (ldb (byte color-size (* color-size 0)) c))
                                (g (ldb (byte color-size (* color-size 1)) c))
                                (r (ldb (byte color-size (* color-size 2)) c))
                                (a (ldb (byte alpha-size (* color-size 3)) c)))
                            (list (float (/ r color-max))
                                  (float (/ g color-max))
                                  (float (/ b color-max))
                                  (if (zerop alpha-max)
                                      1.0
                                    (float (/ a alpha-max)))))))))

(defun read-color-map (s h)
  "Read all the color map entries from the stream."
  (when (tga-header-color-map-p h)
    (loop with length = (tga-header-color-map-length h)
          with color-map = (make-array length :fill-pointer 0)

          ;; get the size of each color entry
          with color-size = (tga-header-color-map-entry-size h)
          with alpha-size = (tga-header-alpha-size h)

          ;; loop over the entire color map
          for n below length

          ;; read each color into the color map
          do (vector-push (read-color s color-size alpha-size) color-map)

          ;; done
          finally (return color-map))))

(defun read-pixel (s h color-map)
  "Reads a pixel value from the image data in a stream."
  (let ((pixel-size (tga-header-pixel-size h)))
    (if color-map
        (loop for i below pixel-size by 8
              sum (ash (read-byte s) i) into pixel
              finally (return (aref color-map pixel)))
      (read-color s pixel-size (tga-header-alpha-size h)))))

(defun read-rle-pixels (s h color-map)
  "Reads a run-length encoded list of pixels."
  (let ((run (read-byte s)))
    (if (zerop (logand run #x80))
        (loop for i to (logand run #x7f) collect (read-pixel s h color-map))
      (let ((pixel (read-pixel s h color-map)))
        (make-list (1+ (logand run #x7f)) :initial-element pixel)))))

(defun read-scanline (s h color-map)
  "Read a y-axis scanline of pixels."
  (let ((scanline (make-array (tga-header-width h) :fill-pointer 0)))
    (do ()
        ((= (length scanline) (tga-header-width h)) scanline)
      (if (tga-header-rle-compressed-p h)
          (dolist (pixel (read-rle-pixels s h color-map))
            (vector-push pixel scanline))
        (vector-push (read-pixel s h color-map) scanline)))))

(defun read-image-data (s h color-map)
  "Read all the pixels for the image."
  (when (plusp (tga-header-image-type h))
    (let ((scanlines (make-array (tga-header-height h) :fill-pointer 0)))
      (flet ((read-scanlines (color-map)
               (dotimes (y (tga-header-height h) scanlines)
                 (vector-push (read-scanline s h color-map) scanlines))))
        (read-scanlines (and (tga-header-color-map-p h) color-map))))))

(defun read-ext-and-tags (s)
  "Determine if this is a truevision file with an extension and a developer tags."
  (when (file-position s (- (file-length s) 26))
    (let ((ext-offset (read-long s))
          (dev-offset (read-long s)))

      ;; read the signature at the end of the file and the last byte (null)
      (when (and (string= (read-string s 17) "TRUEVISION-XFILE.")
                 (zerop (read-byte s)))

        ;; return both the extension and the developer tags
        (values (when (plusp ext-offset)
                  (read-tga-extension s ext-offset))
                (when (plusp dev-offset)
                  (read-tga-developer-tags s dev-offset)))))))

(defun tga-read (s &key (read-ext-and-tags t))
  "Read a tga from an input-bit-stream."
  (let* ((header (read-tga-header s))
         (image-id (read-image-id s header))
         (color-map (read-color-map s header))
         (pixels (read-image-data s header color-map)))

    ;; optionally read the extension and developer tags
    (multiple-value-bind (ext tags)
        (when read-ext-and-tags
          (read-ext-and-tags s))

      ;; create the tga
      (make-tga
       :header header
       :image-id image-id
       :color-map color-map
       :pixels pixels
       :extension ext
       :developer-tags tags))))

(defun tga-load (pathname &key (read-ext-and-tags t))
  "Open a file and read the targa image inside."
  (with-open-file (s pathname :direction :input :element-type '(unsigned-byte 8))
    (tga-read s :read-ext-and-tags read-ext-and-tags)))
