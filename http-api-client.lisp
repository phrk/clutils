(utils:hiconn "clutils/utils.lisp")
(utils:hiconn "HttpApiPostData.lisp")

(ql:quickload :drakma)
(ql:quickload :yason)

(defpackage :utils
	(:export :http-api-client-class :call-signed :call :build-url :call-signed-post :pb-to-base64-string)
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
					:initform nil)))

(defmethod build-url-signed((api http-api-client-class) method params)
	(if (null (is-auth-info-set api))
		(error "auth-info-not-set"))
	(let* ((ts (unix-time))
			(sign_raw (format nil "~A~A~A" method ts (api-key api)))
			(sign (sha1-hash sign_raw))
			(req-url (format nil "~A?method=~A&api_userid=~A&ts=~A&sign=~A" (url api) method (api-userid api) ts sign)))
				(maphash #'(lambda (k v)
									(setf req-url (format nil "~A&~A=~A" req-url k (drakma:url-encode (format nil "~A" v) :utf-8)))) params)
										(return-from build-url-signed req-url)))

(defmethod build-post-url-signed ((api http-api-client-class) method)
	(if (null (is-auth-info-set api))
		(error "auth-info-not-set"))
	(let* ((ts (unix-time))
			(sign_raw (format nil "~A~A~A" method ts (api-key api)))
			(sign (sha1-hash sign_raw)))
				(format nil "~A?method=~A&api_userid=~A&ts=~A&sign=~A" (url api) method (api-userid api) ts sign)))

(defmethod build-url((api http-api-client-class) method params)
	(let* ((req-url (format nil "~A?method=~A&api_userid=~A" (url api) method (api-userid api))))
				(maphash #'(lambda (k v)
									(setf req-url  (format nil "~A&~A=~A" req-url k (drakma:url-encode (format nil "~A" v) :utf-8)))) params)
										(return-from build-url req-url)))

(defmethod call ((api http-api-client-class) method params)
	(drakma:http-request (build-url api method params) :close t :connection-timeout 1 ))

(defmethod call-signed ((api http-api-client-class) method params)
	(let ((url (build-url-signed api method params)))
		;(format t "BUILDURL: ~A ~%" url)
		(drakma:http-request url :close t :connection-timeout 1)))

(defun pb-to-base64-string (obj)
	(let* ((size (pb:octet-size obj))
			(buffer (make-array size :element-type '(unsigned-BYTE 8)))
			(ret (pb:serialize obj buffer 0 size))
			(dump_str (cl-base64:USB8-ARRAY-TO-BASE64-STRING buffer)))
		 dump_str))

(defmethod pack-post-params ((api http-api-client-class) params)
;	(return-from pack-post-params "")
	(let ((post-pb (make-instance 'pb:http-api-post-data)))
		
		(maphash #'(lambda (k v)
						(let ((f (make-instance 'pb:http-api-post-data-field)))
							(setf (pb:field f) (pb:string-field k))
							(setf (pb:value f)
									(utils:string-to-bytes (format nil "~A" v) ))
							;(format t "TYPE: ~A~%" (type-of (pb:value f)))
							(vector-push-extend f (pb:fields post-pb))
							;(format t "SETTING POST FIELDS: ~A/~A~%" k v)
							))
			params)
			
			(pb-to-base64-string post-pb)))

(defmethod call-signed-post ((api http-api-client-class) method params)
	(let ((url (build-post-url-signed api method))
			(content (pack-post-params api params)))
				(drakma:http-request url :method :post :content content :close t :connection-timeout 1)))

(defun test-build-url ()
	(setf api (make-instance 'http-api-client-class :url "http://localhost/api/" :api-userid "_user_" :api-key "_key_" :is-auth-info-set t))
	(setf params (make-hash-table :test #'equal))
	(setf (gethash "a0" params) "b0")
	(setf (gethash "a1" params) "b1")
	(format t "BUILD-URL:~A~%" (build-url-signed api "merge" params)))
	
;(test-build-url)
