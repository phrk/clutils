(ql:quickload "bordeaux-threads")
(ql:quickload :ironclad)
(ql:quickload :postmodern)
(load "clutils/queue.lisp")
(ql:quickload "local-time")
(ql:quickload :flexi-streams)
(ql:quickload :yason)

(defpackage :utils
	(:export :make-smart-vec :iterate-array :iterate-list :escape-string :with-lock :with-cond-wait :with-kick :notnil :field-setp :field-not-setp
			:flatten :sha1-hash :unix-time :bytes-to-string :remove-symbols :dbquery :list-conc-prefixes :round-minutes :unix-time-to-hour-min-str
			:to-json-string)
	(:use :common-lisp))

(in-package :utils)

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

(defun sha1-hash (data)
  (let ((sha1 (ironclad:make-digest 'ironclad:sha1))
        (bin-data (ironclad:ascii-string-to-byte-array data)))
    (ironclad:update-digest sha1 bin-data)
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest sha1))))

(defun unix-time ()
	(local-time:timestamp-to-unix (local-time:now)))

(defun bytes-to-string (arr)
	(flexi-streams:octets-to-string arr :external-format :utf-8))

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
	(* 3600 (truncate (/ (utils:unix-time) 3600))))

(defun unix-time-to-hour-min-str (ts)
	(let ((localtime (local-time:unix-to-timestamp ts))
		(hour 0)
		(min 0))
		(local-time:with-decoded-timestamp (:minute min :hour hour) localtime
			(if (equal min 0)
				(format nil "~A:00" hour)
				(format nil "~A:~A" hour min ) ))))
		
(defun to-json-string (obj)
	(let ((s (make-string-output-stream)))
		(yason:encode obj s)
		(get-output-stream-string s)))
	
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
