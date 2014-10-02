
(load "clutils/utils.lisp")

(defpackage :utils
	(:export :hipath :hiconn :hiprint)
	(:use :common-lisp)
)

(in-package :utils)

(defparameter *hiconn-paths* (utils:make-smart-vec))
(defparameter *hiconn-loaded-files* (make-hash-table :test #'equal))

(defun hipath (path)
	(vector-push-extend path *hiconn-paths*))

(defun hiconn (file)
	
		(map nil #'(lambda(path)
						(let* ((relpath (concatenate 'string path file))
								(fullpath (directory relpath)  ))
								
									(if (null (gethash fullpath *hiconn-loaded-files*))
										(if (load relpath :IF-DOES-NOT-EXIST nil :verbose t) 
											(progn
												(setf (gethash fullpath *hiconn-loaded-files*) t)
												(return-from hiconn))))))
			*hiconn-paths*)
	nil)

(defun hiprint ()
	(maphash  
		#'(lambda (k v)
			(format t "~A~%" (car k) ))
		*hiconn-loaded-files*))
