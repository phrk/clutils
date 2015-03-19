
;;; Message-Digest Algorithm - MD5
;;; ver: 20131106

;;; (md5:digest arg)
;;; Calculate the MD5 digest of the data given in a string or a vector. 
;;; Returns a vector of bytes with md5 hash
;;; 
;;; (md5:digest-hex arg)
;;; The same as (digest arg) but returns a hexadecimal encoded version
;;; of the digest as a string.
;;; 
;;; (md5:hmac key message)
;;; Calculate an HMAC digest using the MD5 digest algorithm. Key and
;;; message are strings or vectors.
;;; 
;;; (md5:hmac-hex key message)
;;; The same as (hmac key message) but but returns a hexadecimal encoded
;;; version of hmac the digest as a string.


;;; Copyright (c) 2013, Art Obrezan
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Use in source and binary forms are prohibited in projects under
;;;    GNU General Public Licenses and its derivatives.
;;;
;;; THIS SOFTWARE IS PROVIDED BY ART OBREZAN ''AS IS'' AND ANY
;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL ART OBREZAN BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package "CL-USER")

(defpackage "MD5"
  (:export digest
           digest-hex
           hmac
           hmac-hex))

(in-package "MD5")


;;-----------------------------------------------------------------------------
;; 1: MAIN
;;-----------------------------------------------------------------------------

(defun digest (arg)
  (let ((message (cond ((stringp arg) (string-to-utf8vector arg))
                       ((vectorp arg) arg)
                       (t (error "MD5: wrong type")))))
    (let ((list nil))
      (flet ((to-bytes (x)
               (push (logand #xFF (ash x -24)) list)
               (push (logand #xFF (ash x -16)) list)
               (push (logand #xFF (ash x -8)) list)
               (push (logand #xFF x) list)))
        (mapcar #'to-bytes (md5-digest message)))
      (coerce (nreverse list) 'vector))))

(defun digest-hex (arg)
  (format nil "~{~2,'0x~}" (coerce (digest arg) 'list)))
  
(defun hmac (key-arg msg-arg)
  (let ((key (cond ((stringp key-arg) (string-to-utf8vector key-arg))
                   ((vectorp key-arg) key-arg)
                   (t (error "MD5 HMAC: wrong type of key"))))
        (msg (cond ((stringp msg-arg) (string-to-utf8vector msg-arg))
                   ((vectorp msg-arg) msg-arg)
                   (t (error "MD5 HMAC: wrong type of message")))))
    (hmac-md5 key msg)))

(defun hmac-hex (key-arg msg-arg)
  (format nil "~{~2,'0x~}" (coerce (hmac key-arg msg-arg) 'list)))


;;-----------------------------------------------------------------------------
;; 2: MD5 HASH DIGEST
;;-----------------------------------------------------------------------------

(declaim (inline to-32bit-word))
(defun to-32bit-word (n)
  (logand #xFFFFFFFF n))

(declaim (inline leftrotate))
(defun leftrotate (n shift)
  (let ((n32 (to-32bit-word n)))
    (logior (to-32bit-word (ash n32 shift))
            (ash n32 (- shift 32)))))


(defconstant +k+
  #(#xD76AA478 #xE8C7B756 #x242070DB #xC1BDCEEE
    #xF57C0FAF #x4787C62A #xA8304613 #xFD469501
    #x698098D8 #x8B44F7AF #xFFFF5BB1 #x895CD7BE
    #x6B901122 #xFD987193 #xA679438E #x49B40821
    #xF61E2562 #xC040B340 #x265E5A51 #xE9B6C7AA
    #xD62F105D #x02441453 #xD8A1E681 #xE7D3FBC8
    #x21E1CDE6 #xC33707D6 #xF4D50D87 #x455A14ED
    #xA9E3E905 #xFCEFA3F8 #x676F02D9 #x8D2A4C8A
    #xFFFA3942 #x8771F681 #x6D9D6122 #xFDE5380C
    #xA4BEEA44 #x4BDECFA9 #xF6BB4B60 #xBEBFBC70
    #x289B7EC6 #xEAA127FA #xD4EF3085 #x04881D05
    #xD9D4D039 #xE6DB99E5 #x1FA27CF8 #xC4AC5665
    #xF4292244 #x432AFF97 #xAB9423A7 #xFC93A039
    #x655B59C3 #x8F0CCC92 #xFFEFF47D #x85845DD1
    #x6FA87E4F #xFE2CE6E0 #xA3014314 #x4E0811A1
    #xF7537E82 #xBD3AF235 #x2AD7D2BB #xEB86D391))

(defconstant +g+
  #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
    1 6 11 0 5 10 15 4 9 14 3 8 13 2 7 12
    5 8 11 14 1 4 7 10 13 0 3 6 9 12 15 2
    0 7 14 5 12 3 10 1 8 15 6 13 4 11 2 9))

(defconstant +shifts+
  #(7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22
    5  9 14 20 5  9 14 20 5  9 14 20 5  9 14 20
    4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23
    6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21))


(defun md5-digest (message)
  (let* ((padded-message (pad-message message))
         (n (/ (length padded-message) 64))
         (h0 #x67452301)
         (h1 #xEFCDAB89)
         (h2 #x98BADCFE)
         (h3 #x10325476)
         (f nil))
    (dotimes (block-num n)
      (let ((a h0) (b h1) (c h2) (d h3) (temp 0)
            (message-block (prepare-message-block block-num padded-message)))
        (dotimes (i 64)
          (cond ((<= 0 i 15)
                 (setq f (logxor d (logand b (logxor c d)))))
                ((<= 16 i 31)
                 (setq f (logxor c (logand d (logxor b c)))))
                ((<= 32 i 47)
                 (setq f (logxor b c d)))
                ((<= 48 i 63)
                 (setq f (logxor c (logior b (lognot d))))))
          (setq temp d)
          (setq d c)
          (setq c b)
          (setq b (let ((k (svref +k+ i))
                        (w (svref message-block (svref +g+ i)))
                        (shift (svref +shifts+ i)))
                    (to-32bit-word
                     (+ b (leftrotate (+ a f k w) shift)))))
          (setq a temp))
        (setq h0 (to-32bit-word (+ h0 a)))
        (setq h1 (to-32bit-word (+ h1 b)))
        (setq h2 (to-32bit-word (+ h2 c)))
        (setq h3 (to-32bit-word (+ h3 d)))))
    (list (encode-as-little-endian h0)
          (encode-as-little-endian h1)
          (encode-as-little-endian h2)
          (encode-as-little-endian h3))))     

(defun encode-as-little-endian (n)
  (+ (ash (logand #xFF n) 24)
     (ash (logand #xFF00 n) 8)
     (logand #xFF00 (ash n -8))
     (logand #xFF (ash n -24))))


(defun pad-message (message)
  (let* ((message-len (length message))
         (message-len-in-bits (* message-len 8))
         (buffer-len (padded-buffer-len message-len))
         (buffer (make-array buffer-len :initial-element 0)))
    (dotimes (i message-len)
      (setf (aref buffer i) (aref message i)))
    (setf (aref buffer message-len) #b10000000)
    (dotimes (i 8)
      (setf (aref buffer (+ (- buffer-len 8) i))
            (logand #xFF (ash message-len-in-bits (* i -8)))))
    buffer))

(defun padded-buffer-len (message-len)
  (let ((x (rem (+ message-len 1 8) 64)))
    (+ message-len 1 8 (- 64 (if (zerop x) 64 x)))))

(defun prepare-message-block (block-num message)
  (let ((message-block (make-array 16))
        (offset (* block-num 64)))
    (dotimes (i 16)
      (setf (aref message-block i)
            (+ (aref message (+ offset (* i 4)))
               (ash (aref message (+ offset (* i 4) 1)) 8)
               (ash (aref message (+ offset (* i 4) 2)) 16)
               (ash (aref message (+ offset (* i 4) 3)) 24))))
    message-block))


;;-----------------------------------------------------------------------------
;; 3: HMAC
;;-----------------------------------------------------------------------------

(defconstant +hmac-blocksize+ 64) ;; 64 for sha-1, md5

(defun hmac-md5 (key-vector msg-vector)
  (flet ((vec+ (x y) (concatenate 'vector x y)))
    (let* ((key (make-key key-vector +hmac-blocksize+))
           (opad-key (make-pad-key key #x5c))
           (ipad-key (make-pad-key key #x36)))
      (digest (vec+ opad-key (digest (vec+ ipad-key msg-vector)))))))

(defun make-key (key-vector block-size)
  (let ((key (if (> (length key-vector) block-size)
                 (digest key-vector)
               key-vector)))
    (concatenate 'vector
       key (make-array (- block-size (length key)) :initial-element 0))))

(defun make-pad-key (key xor-value)
  (let ((buffer (make-array (length key))))
    (dotimes (i (length key))
      (setf (aref buffer i) (logxor xor-value (aref key i))))
    buffer))


;;-----------------------------------------------------------------------------
;; 4: LW STRING TO UTF8 OCTET VECTOR CONVERTIONS
;;-----------------------------------------------------------------------------

(defun string-to-utf8vector (str)
  (let ((buf (make-array (length str)
                         :element-type '(unsigned-byte 8)
                         :adjustable t
                         :fill-pointer 0)))
    (dotimes (i (length str))
      (let ((code (char-code (char str i))))
        (cond
         ((< code #x80)
          (vector-push-extend code buf))
         ((< code #x800)
          (vector-push-extend (logior #xC0 (ash (logand #x7C0 code) -6)) buf)
          (vector-push-extend (logior #x80 (logand #x3F code)) buf))
         ((< code #x10000)
          (vector-push-extend (logior #xE0 (ash (logand #xF000 code) -12)) buf)
          (vector-push-extend (logior #x80 (ash (logand #xFC0 code) -6)) buf)
          (vector-push-extend (logior #x80 (logand #x3F code)) buf))
         (t (error "Character is out of the ucs-2 range")))))
    buf))


;;-----------------------------------------------------------------------------
;; 5: TEST
;;-----------------------------------------------------------------------------

(defun test ()
  (and
   (string= (digest-hex "")
            "D41D8CD98F00B204E9800998ECF8427E")
   (string= (digest-hex "The quick brown fox jumps over the lazy dog.")
            "E4D909C290D0FB1CA068FFADDF22CBD0")
   (string= (digest-hex #(1 2 3))
            "5289DF737DF57326FCDD22597AFB1FAC")
   (string= (hmac-hex "" "")
            "74E6F7298A9C2D168935F58C001BAD88")
   (string= (hmac-hex "key" "The quick brown fox jumps over the lazy dog")
            "80070713463E7749B90C2DC24911E275")
   (string= (hmac-hex #(1 2 3) #(1 2 3))
            "E68999C8E9978048D759AE4BEA59200C")))
