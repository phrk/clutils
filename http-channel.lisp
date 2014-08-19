
(load "clutils/utils.lisp")

(defpackage :utils
	(:export :notif-channel-class :on-notif-wait :on-notif-kick)
	(:use :common-lisp)
)

(in-package :utils)

(defstruct resp-info
	cond-lock
	resp-setp
	resp)

(defclass notif-channel-class ()
	((wait-resps 
		:accessor wait-resps
		:initform (make-hash-table :test #'equal))
	(lock 
		:accessor lock
		:initform (bordeaux-threads:make-lock ))))

(defmethod on-notif-wait ((ch notif-channel-class) nusess req)
	(let ((i 0)
			(got-notif nil)
			(resp ""))
			
		(utils:with-lock (lock ch) #'(lambda ()
			(if (null (gethash nusess (wait-resps ch) ))
				(setf (gethash nusess (wait-resps ch)) (make-resp-info :resp-setp nil)))))
	    
		(dotimes (i 100)
			(progn
				
				(utils:with-lock (lock ch) #'(lambda ()
					(if (resp-info-resp-setp (gethash nusess (wait-resps ch)))
						(progn
							(setf (resp-info-resp-setp (gethash nusess (wait-resps ch))) nil)
							(setf got-notif t)
							(setf resp (resp-info-resp (gethash nusess (wait-resps ch))))))))
				
				(if got-notif
					(return-from on-notif-wait resp))
				(sleep 0.1)))
	
		(format nil "reconnect")))

(defmethod on-notif-kick ((ch notif-channel-class) req)
	(let* ((nusess (tbnl:get-parameter "nusess"))
			(data (tbnl:get-parameter "data")))
			
			(utils:with-lock (lock ch) #'(lambda ()	
					(setf (resp-info-resp-setp (gethash nusess (wait-resps ch))) t)
					(setf (resp-info-resp (gethash nusess (wait-resps ch))) data)))
			
	(format nil "kicked")))