(load "~/quicklisp/setup.lisp")
(ql:quickload "linedit")
(linedit:install-repl)

(load "clutils/http-api.lisp")
(load "clutils/http-api-client.lisp")
(load "clutils/listener.lisp")

(defpackage :utils
	(:export )
	(:use :common-lisp)
)

(in-package :utils)

(defun on-signed-method(params)
	(format t "ON-SIGNED-METHOD!~%")
	(format nil "on-signed-method calledsdhfuoshdfusodhfoushdfoshdfoushdofhsodhfoshdf!"))

(defun on-method(params)
	(format t "ON-METHOD!~%")
	(flexi-streams:string-to-octets (format nil "on-method called!______~%") :external-format :utf-8 ))

(defun http-api-run (*lstnr* *api* listenport listenpath)
	
	(utils:add-user *api* "_user_" "_key_")

	(setf signed-method-args (utils:make-smart-vec))
	(vector-push-extend "foo" signed-method-args)
	(utils:add-api-method-signed *api* "signed-method" signed-method-args #'on-signed-method)
	
	(setf method-args (utils:make-smart-vec))
	(vector-push-extend "foo" method-args)
	(utils:add-api-method *api* "method" method-args #'on-method)
		
	(utils:listener-add-handler *lstnr* listenpath #'(lambda (req) (utils:handle-request *api* req)))
	(utils:listener-start *lstnr* listenport))
	

(defmethod http-api-call-test (api foo)
	(let ((params (make-hash-table :test #'equal)))
		(setf (gethash "foo" params) foo)
		(let* ((resp (utils:call api "method" params))
				(pprint resp))))) 
				;(resp-str (utils:bytes-to-string resp)))
				;	(pprint resp-str))))
				;(format t "~A" resp-str))))
				;(parsed (yason:parse resp-str)))
				;	(if (not (equal (gethash "status_code" parsed) "200"))
				;		(error (format nil "http-api-call-test unknown error ~A" resp-str))))))

(defmethod http-api-call-signed-test (api foo)
	(let ((params (make-hash-table :test #'equal)))
		(setf (gethash "foo" params) foo)
		(let* ((resp (utils:call-signed api "method-signed" params))
				(resp-str (utils:bytes-to-string resp))
				(parsed (yason:parse resp-str)))
					;(format t "~A~%" resp-str)
					(if (not (equal (gethash "status_code" parsed) "200"))
						(error (format nil "geber-client-class::update-item unknown error ~A" resp-str))))))


(defparameter *lstnr* (make-instance 'utils:listener))	
(defparameter *api* (make-instance 'utils:http-api-class :get-auth-error #'(lambda () "{ \"status_code\" : \"-1\" }")))


(setf port 1488)
(setf path "/listenpath/")

(http-api-run *lstnr* *api* port path)

(sleep 1)

(setf client-api (make-instance 'utils:http-api-client-class :url (format nil "http://localhost:~A~A" port path)
																								:api-userid "_user_"
																								:api-key "_key_"
																								:is-auth-info-set t))

;(http-api-call-signed-test client-api "foo param")
(http-api-call-test client-api "foo param")

