(utils:hiconn "utils.lisp")

(ql:quickload :do-urlencode)
(ql:quickload :hunchentoot)

(defpackage :utils
	(:export :http-api-class :add-user :add-api-method :add-api-method-signed :handle-request)
	(:use :common-lisp)
)

(in-package :utils)

(defclass http-api-class () (
	(url :accessor url
			:initarg :url)
	(methods-args :accessor methods-args
					:initform (make-hash-table :test #'equal) )
	(keys :accessor keys
					:initform (make-hash-table :test #'equal) )
					
	(get-auth-error :accessor get-auth-error
		 			:initarg :get-auth-error)
	
	(methods-callbacks :accessor methods-callbacks
		 			:initform (make-hash-table :test #'equal) )
	(methods-signed :accessor methods-signed
		 			:initform (make-hash-table :test #'equal) )
	 ))


(defmethod signedp ((api http-api-class) method)
	(not  (null (gethash method (methods-signed api)))))

(defmethod add-user((api http-api-class) userid key)
	(setf (gethash userid (keys api)) key))

(defmethod add-api-method((api http-api-class) name args_names onreq)
	(setf (gethash name (methods-args api)) args_names)
	(setf (gethash name (methods-callbacks api)) onreq))

(defmethod add-api-method-signed((api http-api-class) name args_names onreq)
	(add-api-method api name args_names onreq)
	(setf (gethash name (methods-signed api)) t))

(defmethod check-fields ((api http-api-class) get-param post-param)
	;(format t "check-fields ~%~%")
	(let ((fields (make-hash-table :test #'equal))
			(method (funcall get-param "method"))
		(api_userid (funcall get-param "api_userid"))
		(ts (funcall get-param "ts"))
		(sign (funcall get-param "sign")))
		
		; merge post params
		(handler-case
			(progn
				(let* ((post_raw (funcall post-param "params"))
						(postfields (yason:parse (utils:decode-octets-if-need (cl-base64:BASE64-STRING-TO-USB8-ARRAY  post_raw))))
						)
					(maphash
						#'(lambda (k v)
							(setf (gethash k fields) v)
							;(format t "POSTFIELD: ~A:~A~%" k v)
							)
						postfields)
						))
		(error (cnd) nil))
		
		
		(setf (gethash "method" fields) method)
		(setf (gethash "ts" fields) ts)
		(setf (gethash "api_userid" fields) api_userid)
		(setf (gethash "sign" fields) sign)
		
		
		(format t "~%~%METHOD: ~A~%" method)
		(format t "signed: ~A~%" (signedp api method))
		(maphash
			#'(lambda (k v)
				(format t "FIELD: ~A/~A ~%" k v))
			fields)
		
		(if (null method)
			(return-from check-fields nil))
		
		(if (null (gethash method (methods-args api)))
			(return-from check-fields nil))
		
		; merge get params 
		(utils:iterate-array (gethash method (methods-args api)) 
							#'(lambda (arg)
								(let ((field (funcall get-param arg)))
									(if field
										(setf  (gethash arg fields) field)))))
		
		(utils:iterate-array (gethash method (methods-args api)) 
							#'(lambda (arg)
											(let ((field (gethash arg fields)))
												(if (null field)
													(progn
														(format t "field not set: ~A ~%" arg)
														(return-from check-fields nil))
													(progn
														(setf (gethash arg fields)   field )
														;(format t "FIELD: ~A/~A~%" arg (gethash arg fields))
														)
													))))
		
		(if (not (signedp api method))
			(return-from check-fields fields))
		
		(if (null api_userid)
			(return-from check-fields nil))
		(if (null ts)
			(return-from check-fields nil))
		(if (null sign)
			(return-from check-fields nil))
			
		(if (not (equal sign (sha1-hash (format nil "~A~A~A" method ts (gethash api_userid (keys api)) ) )))
			(progn
				;(format t "~%~%SIGNS NOT EQUAL~%~%")
				(return-from check-fields nil)))

		fields))
;(hunchentoot:url-decode (cdr a) :utf-8 )
;(do-urlencode:urldecode
(defmethod handle-request((api http-api-class) get-param post-param)
	(let ((fields_raw (check-fields api get-param post-param))
			(fields (make-hash-table :test #'equal)))
		;(mapcar #'(lambda (a) (setf (gethash (car a) fields) (utils:url-decode (cdr a)))) (tbnl:get-parameters req))
		
		(if (null fields_raw)
			(return-from handle-request "not all params set"))
		
		(maphash #'(lambda (k v)
							(setf (gethash k fields) (utils:url-decode v)))
				fields_raw)
		
		(if (null fields)
			(funcall (get-auth-error api))
			(progn
				;(format t "CALLING METHOD ~A~%~%" (gethash "method" fields))
				(funcall (gethash (gethash "method" fields) (methods-callbacks api)) fields)
				))))
