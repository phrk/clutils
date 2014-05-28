(load "clutils/utils.lisp")
(ql:quickload :drakma)
(ql:quickload :yason)

(defpackage :utils
	(:export :http-api-client-class :call-signed)
	(:use :common-lisp)
)

(in-package :utils)

(defclass http-api-client-class() (
	(url :accessor url
		:initarg :url)
	(api-userid :accessor api-userid
				:initarg :api-userid)
	(api-key :accessor api-key
			:initarg :api-key)
	(is-auth-info-set :accessor is-auth-info-set
					:initarg :is-auth-info-set
					:initform nil)
					
	))
	
(defmethod build-url-signed((api http-api-client-class) method params)
	(if (null (is-auth-info-set api))
		(error "auth-info-not-set"))
	(let* ((ts (unix-time))
			(sign_raw (format nil "~A~A~A" method ts (api-key api) ))
			(sign (sha1-hash sign_raw))
			(req-url (format nil "~A?method=~A&api_userid=~A&ts=~A&sign=~A" (url api) method (api-userid api) ts sign)))
				(maphash #'(lambda (k v)
									(setf req-url (format nil "~A&~A=~A" req-url k v)) ) params)
										(return-from build-url-signed req-url) ))

(defmethod build-url((api http-api-client-class) method params)
	)

(defmethod call ((api http-api-client-class) method params)
	)

(defmethod call-signed ((api http-api-client-class) method params)
	(let ((url (build-url-signed api method params)))
		;(format t "BUILDURL: ~A ~%" url)
		(drakma:http-request url )))


(defun test-build-url ()
	(setf api (make-instance 'http-api-client-class :url "http://localhost/api/" :api-userid "_user_" :api-key "_key_" :is-auth-info-set t ))
	(setf params (make-hash-table :test #'equal))
	(setf (gethash "a0" params) "b0")
	(setf (gethash "a1" params) "b1")
	(format t "BUILD-URL:~A~%" (build-url-signed api "merge" params)))
	
;(test-build-url)