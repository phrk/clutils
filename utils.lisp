(ql:quickload "bordeaux-threads")
(ql:quickload :ironclad)
(ql:quickload :postmodern)
(ql:quickload "local-time")
(ql:quickload :flexi-streams)
(ql:quickload :yason)
;(ql:quickload "hunchentoot")
(ql:quickload :do-urlencode)
(ql:quickload :trivial-utf-8)
(ql:quickload :cl-base64)

(defpackage :utils
	(:export :make-smart-vec :iterate-array :iterate-list :escape-string :with-lock :with-cond-wait :with-kick :notnil :field-setp :field-not-setp
			:flatten :sha1-hash :unix-time :bytes-to-string :remove-symbols :dbquery :list-conc-prefixes :round-minutes :unix-time-to-hour-min-str
			:to-json-string :round-hours :read-file-to-string :url-decode :unix-time-to-date :escape-json-postgres :unescape-json-postgres
			:url-encode :split :merge-unique-vecs :merge-unique-lists :replace-all :erase-tags :decode-octets-if-need :string-to-bytes
			:string-to-array :encode-octets-if-need :str-appendf) 
	(:use :common-lisp))

(in-package :utils)

(load "clutils/queue.lisp")

(defparameter *dblock* (bordeaux-threads:make-lock "db-lock"))

(defun make-smart-vec ()
	(make-array 1 :fill-pointer 0 :adjustable t))

(defun iterate-array (arr fn)
	(map nil fn arr))

(defun iterate-list (lst fn)
	(apply fn lst))
	
(defun with-lock (lock fn)
	(bordeaux-threads:acquire-lock lock)
	(handler-case
		(progn
			(setf ret (funcall fn))
		;(format t "with-lock ret ~A~%" ret)
			(bordeaux-threads:release-lock lock)
			(return-from with-lock ret))
	(error (cnd) (print "with lock error~%")))
	(bordeaux-threads:release-lock lock)
	nil)

(defun cond-wait (condvar lock check-reached reset-condition)
	(bordeaux-threads:acquire-lock lock)
	(loop do
			(bordeaux-threads:condition-wait condvar lock)
		while (not (funcall check-reached)))
	(funcall reset-condition)
	(bordeaux-threads:release-lock lock))

(defun escape-string(str)
	(let ((qpos (search "\'" str)))
		(if (null qpos)
			str
			(concatenate 'string (escape-string (subseq str 0 qpos))
								(escape-string (subseq str (+ 1 qpos) (length str)))))))

(defun with-kick (cond lock fn)
	(bordeaux-threads:acquire-lock lock)
	(funcall fn)
	(bordeaux-threads:condition-notify cond)
	(bordeaux-threads:release-lock lock))

(defun notnil(field)
	(not (equal nil field)))

(defun field-setp (field)
	(and (notnil field)
		(not (equal "" field))))

(defun field-not-setp (field)
	(not (field-setp field)))


(defun flatten (ls)
  (labels (
    (mklist (x) (if (listp x) x (list x)))
    )
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

(defun split (s str)
	(let ((pos (position s str :test #'equal)))
		(if (null pos)
			(list str)
			(progn 
				(let ((left_str (subseq str 0 pos))
						(right_str (subseq str (+ pos 1))))
							(cons left_str (split s right_str)))))))

(defun sha1-hash (data)
  (let ((sha1 (ironclad:make-digest 'ironclad:sha1))
        (bin-data (ironclad:ascii-string-to-byte-array data)))
    (ironclad:update-digest sha1 bin-data)
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest sha1))))

(defun unix-time ()
	(local-time:timestamp-to-unix (local-time:now)))

(defun bytes-to-string (arr)
	(flexi-streams:octets-to-string arr :external-format :utf-8))

(defun string-to-bytes (str)
	(flexi-streams:string-to-octets str :external-format :utf-8))

(defun string-to-array (str)
	(let* ((arr (make-array (length str) :element-type '(UNSIGNED-BYTE 8) :fill-pointer nil :adjustable nil ))
		(i 0))
			(map nil
			    #'(lambda (e)
					(setf (aref arr i) (aref str i))
					(setf i (+ 1 i)))
			str)
		arr))

(defun decode-octets-if-need (str)

	(if (every #'(lambda (e)
					(typep e '(UNSIGNED-BYTE 8)))
			str)
		(bytes-to-string str)
		str))

(defun encode-octets-if-need (str)
	(if (every #'(lambda (e)
					(typep e '(UNSIGNED-BYTE 8)))
			str)
		str
		(string-to-bytes str)))

(defun remove-symbols (str)
	(string-trim " " (remove-if-not #'(lambda (s) (or (alphanumericp s)  (equal #\Space s) (equal #\. s)  )) str)))
	
(defun dbquery (q)
	(setf ret nil)
	;(handler-case
		
		(with-lock *dblock* #'(lambda ()
			(setf ret (postmodern:query q))))
			
	;(error (cnd) (print "dbquery error")))
	;(format nil "dbquery ret ~A~%" ret)
	ret)
	
	
(defun list-conc-prefixes (lst prefix)
	(let ((newlist_absolute '()))	
		(mapcar #'(lambda (css_rel)
					(setf newlist_absolute (cons (format nil "~A~A" prefix css_rel)
												newlist_absolute) )) 
				lst)
		(reverse newlist_absolute)))

(defun round-minutes (ts)
	(* 3600 (floor (/ ts 3600))))

(defun round-hours (ts)
	(- (* (* 24 3600) (floor (/ ts (* 24 3600) ))) (* 5 3600)))

(defun unix-time-to-hour-min-str (ts)
	(let ((localtime (local-time:unix-to-timestamp ts))
		(hour 0)
		(min 0))
		(local-time:with-decoded-timestamp (:minute min :hour hour) localtime
			(if (equal min 0)
				(format nil "~A:00" hour)
				(format nil "~A:~A" hour min ) ))))

(defun unix-time-to-date (ts)
	(let ((localtime (local-time:unix-to-timestamp ts))
		(day 0)
		(month 0))
		(local-time:with-decoded-timestamp (:day day :month month) localtime
				(format nil "~A.~A" day month))))

(defun replace-all (string part replacement &key (test #'char=))
	"Returns a new string in which all the occurences of the part 
	is replaced with replacement."
	    (with-output-to-string (out)
	      (loop with part-length = (length part)
	            for old-pos = 0 then (+ pos part-length)
	            for pos = (search part string
	                              :start2 old-pos
	                              :test test)
	            do (write-string string out
	                             :start old-pos
	                             :end (or pos (length string)))
	            when pos do (write-string replacement out)
	            while pos)))
		
(defun to-json-string (obj)
	(let ((s (make-string-output-stream)))
		(yason:encode obj s)
		(get-output-stream-string s)))

(defun escape-json-postgres (str)
	(replace-all str "'" "''"))

(defun unescape-json-postgres ()
	(replace-all str "''" "'"))

(defun read-file-to-string (filename)
	(let ((in (open filename :if-does-not-exist nil))
			(ret ""))
			  (when in
			    (loop for line = (read-line in nil)
			         while line do (setf ret (format nil "~A~A" ret line)))
			    (close in))
			ret))

(defun url-encode (str)
	(do-urlencode:urlencode str))

(defun url-decode (str)
	;(format t "TYEPOF: ~A~%" (type-of str))
	;(if (stringp str)
	;	(hunchentoot:url-decode str)
		
		;)
		(do-urlencode:urldecode str :lenientp t))

(defun merge-unique-vecs (v1 v2)
	(let ((res_hash (make-hash-table :test #'equal))
			(res (make-smart-vec)))

		(map nil #'(lambda (f)
						(setf (gethash f res_hash) t))
				v1)
		
		(map nil #'(lambda (f)
						(setf (gethash f res_hash) t))
				v2)
		
		(maphash #'(lambda (k v)
						(vector-push-extend k res)
				res_hash)
		res)))

(defun merge-unique-lists (l1 l2)
	(let ((res_hash (make-hash-table :test #'equal))
			(res '()))

		(map nil #'(lambda (f)
						(setf (gethash f res_hash) t))
				l1)

		(map nil #'(lambda (f)
						(setf (gethash f res_hash) t))
				l2)

		(maphash #'(lambda (k v)
						(setf res (cons k res)))
				res_hash)
		res))


(defun erase-tags (str)
	(if (null str)
		(return-from erase-tags nil))

	(setf str (utils:replace-all str "'" ""))
	(setf str (utils:replace-all str "<" ""))
	(setf str (utils:replace-all str ">" ""))
	str)

(defmacro str-appendf (a b)
	`(setf ,a (concatenate 'string ,a ,b)))

;(setf val nil)

;(setf cond (bordeaux-threads:make-condition-variable))

;(setf lock (bordeaux-threads:make-lock))

;(defun reached()
;	val)

;(defun wait-thread()
;	(with-cond-wait cond lock #'reached #'(lambda () (setf cond (bordeaux-threads:make-condition-variable))))
;	(format t "COND REACHED!!!!!!!"))

;(defun kick()
;	(bordeaux-threads:acquire-lock lock)
;	(setf val t)
;	(bordeaux-threads:condition-notify cond)
;	(format t "Kicked!")
;	(bordeaux-threads:release-lock lock))

;(bordeaux-threads:make-thread #'wait-thread)
