(ql:quickload :bordeaux-threads)

(utils:hiconn "clutils/utils.lisp")

(defpackage :utils
	(:export :threadpool :threadpool-start :threadpool-stop :add-task :pool-test :threadpool-add-threads)
	(:use :common-lisp))

(in-package :utils)

(defclass threadpool()
	((queue 
		:accessor threadpool-queue
		:initform (make-queue))
	(cond
		:accessor threadpool-cond
		:initform (bordeaux-threads:make-condition-variable))
	(waitlock
		:accessor threadpool-waitlock
		:initform (bordeaux-threads:make-lock))
	(lock
		:accessor threadpool-lock
		:initform (bordeaux-threads:make-lock))
	(running
		:accessor threadpool-running
		:initform t)))

(defmethod add-task((pool threadpool) task)
	(with-kick (threadpool-cond pool) (threadpool-waitlock pool)
				#'(lambda () (enqueue task (threadpool-queue pool)))))

(defmethod threadpool-notempty ((pool threadpool))
	(not (queue-empty-p (threadpool-queue pool))))

(defmethod threadpool-empty ((pool threadpool))
	(queue-empty-p (threadpool-queue pool)))

(defmethod threadpool-thread((pool threadpool))
	(loop do
		(handler-case
			(progn
				(bordeaux-threads:acquire-lock (threadpool-lock pool))
				(if (threadpool-empty pool)
					(cond-wait (threadpool-cond pool)
								(threadpool-waitlock pool)
								#'(lambda () (threadpool-notempty pool))
								#'(lambda () (setf (threadpool-cond pool) (bordeaux-threads:make-condition-variable)))))
				(setf fn (dequeue (threadpool-queue pool)))
				(bordeaux-threads:release-lock (threadpool-lock pool))
				(funcall fn))
		(error (cnd) (princ cnd) ) )
	while (threadpool-running pool)))

(defmethod threadpool-add-threads ((pool threadpool) nthreads)
	(loop for i from 1 to nthreads do
		(bordeaux-threads:make-thread #'(lambda () (threadpool-thread pool)))))

(defmethod threadpool-stop ((pool threadpool))
	(setf (threadpool-running pool) nil))

;(defun pool-test()
;	(defparameter pool (make-instance 'threadpool))
;	(threadpool-add-threads pool 3)
;	(add-task pool #'(lambda () (format t "HEIL THREAD!"))))

;(cond-wait (threadpool-cond ) )
