
(load "clutils/utils.lisp")
(ql:quickload "hunchentoot")

(defpackage :utils
	(:export :listener :listener-add-handler :listener-start :handler-info :create-handler-info :make-file-listener)
	(:use :common-lisp)
)

(in-package :utils)

;;; Subclass ACCEPTOR
(defclass vhost (tbnl:acceptor)
	;; slots
	((dispatch-table
		:initform '()
		:accessor dispatch-table
		:documentation "List of dispatch functions"))
  ;; options
	(:default-initargs                    ; default-initargs must be used
;	:address "127.0.0.1"
	:persistent-connections-p nil)
	)               ; because ACCEPTOR uses it

;;; Specialise ACCEPTOR-DISPATCH-REQUEST for VHOSTs
(defmethod tbnl:acceptor-dispatch-request ((vhost vhost) request)
	;; try REQUEST on each dispatcher in turn
	(mapc (lambda (dispatcher)
		(let ((handler (funcall dispatcher request)))
			(when handler               ; Handler found. FUNCALL it and return result
				(return-from tbnl:acceptor-dispatch-request (funcall handler request)))))
		(dispatch-table vhost))
	(call-next-method))

(defstruct (handler-info (:constructor create-handler-info (path fn) ))
	path fn)

(defclass listener () (
	(srv :accessor listener-srv)
	(port :accessor listener-port
			:initform 8080)
	(handers :accessor listener-handlers
			:initform (utils:make-smart-vec) )))

(defmethod listener-add-handler ((lstnr listener) path fn)
	(vector-push-extend (create-handler-info path fn) (listener-handlers lstnr)))

(defmethod listener-start((lstnr listener) port)
	(setf (listener-srv lstnr) (make-instance 'vhost :port port :persistent-connections-p nil :output-chunking-p nil))
	(utils:iterate-array (listener-handlers lstnr)
		#'(lambda (obj) (push (tbnl:create-prefix-dispatcher (handler-info-path obj) (handler-info-fn obj))
		      								(dispatch-table (listener-srv lstnr)))))
	(tbnl:start (listener-srv lstnr)))

(defun remove-parent-dir(path)
	(setf path (subseq path 1 (length path)) )
	(subseq path (search "/" path) (length path) ) )

(defun get-req-path (url path)
	(setf b (search path url))
	(setf e (- (length url) 1))

	(setf q (search "?" url))
	(if q
		(subseq url b q)
		(subseq url b q)))

(defmethod make-file-listener(uriprefix dir)
	#'(lambda (req)
		(let* ((uri (tbnl:request-uri req))
				(filepath (concatenate 'string dir (remove-parent-dir (get-req-path uri uriprefix)) )))
			(format t "GETTING FILE: ~A~%" filepath)
			(tbnl:handle-static-file filepath ))))

;(defparameter lstnr (make-instance 'listener))
;(listener-add-handler lstnr "/foo/" #'(lambda (req) (format nil "FOO") ))
;(listener-add-handler lstnr "/bar/" #'(lambda (req) (format nil "BAR") ))
;(start-listener lstnr 8080)

