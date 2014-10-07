
(utils:hiconn "utils.lisp")
(ql:quickload :postmodern)

(defpackage :owner-sess
  (:export :create-user :get-user :check-login :gen-session-data :check-session)
  (:use :common-lisp))

(in-package :owner-sess)

(defun create-user (sch login pass ownerid enabled_opts)
	(let ((resp (utils:dbquery (format nil "select * from ~A.users WHERE login='~A'" sch login))))
		(if (null resp)
			(progn 
				(let* ((salt (format nil "~A" (random 100000 (make-random-state t))))
						(passhash (utils:sha1-hash (format nil "~A~A" pass salt)))
							(userid (car (car (utils:dbquery (format nil "INSERT INTO ~A.users (login, passhash, salt, ownerid, enabled_opts) VALUES ('~A','~A','~A', ~A, '~A') RETURNING id"
							 sch login passhash salt ownerid (utils:to-json-string enabled_opts) )))))))
				t)
			nil)))

(defun get-user (sch userid)
	;(format t "get-user ~A~%" userid)
	(let ((resp (utils:dbquery (format nil "select row_to_json (row) from ~A.users row WHERE id=~A" sch userid))))
		(if (null resp)
			nil
			(let ((ret (yason:parse (car (car resp)))))
				(setf (gethash "enabled_opts" ret) (yason:parse (gethash "enabled_opts" ret)))
			 	ret))))

; returns userid
(defun check-login (sch login pass)
	(let ((resp (utils:dbquery (format nil "select row_to_json (row) from ~A.users row WHERE login='~A'" sch login))))
		
		(setf obj nil)
		
		(handler-case
			(setf obj (yason:parse (car (car resp))))
		(error (cnd) (return-from check-login nil)))
		
		(setf passhash_orig (gethash "passhash" obj))
		(setf salt (gethash "salt" obj))
		
		(setf passhash (utils:sha1-hash (format nil "~A~A" pass salt)))
		
		(if (equal passhash passhash_orig)
			(gethash "id" obj)
			nil)))

; returns (userid token)
(defun gen-session-data (sch login pass)
	(setf userid (check-login sch login pass))
	(if (null userid)
		(return-from gen-session-data nil))
	(setf resp (utils:dbquery (format nil "select row_to_json (row) from ~A.sessions row WHERE userid='~A'" sch userid)))
	
	(setf token (utils:sha1-hash (format nil "~A~A" (random 10000000 (make-random-state t)) (utils:unix-time) )))
	
	(if (null resp)
		(utils:dbquery (format nil "INSERT INTO ~A.sessions(userid, token, creation_ts) VALUES(~A, '~A', ~A)" sch userid 
				 token (utils:unix-time)))
		
 		(utils:dbquery (format nil "UPDATE ~A.sessions SET  token = '~A', creation_ts=~A WHERE userid=~A"  
 				 sch token (utils:unix-time) userid)))
	
	(list userid token))

(defun check-session (sch userid token)
	(setf resp (utils:dbquery (format nil "select row_to_json (row) from ~A.sessions row WHERE userid='~A' AND token='~A'" sch userid token)))
	(if (null resp)
		nil
		(progn
			(setf obj nil)
		
			(handler-case
				(setf obj (yason:parse (car (car resp))))
			(error (cnd) (return-from check-session nil)))
			
			(setf token_orig (gethash "token" obj))
			
			(equal token token_orig))))

