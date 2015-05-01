
(utils:hiconn "clutils/utils.lisp")

(ql:quickload :anaphora)

(defpackage :utils
	(:export :read-ct-file :print-types :verify-ct-object :ct-obj :ct-obj-fields :ct-obj-types :create-ct-obj :ct-field :ct-type
			:ct-type-id :ct-type-nameru :ct-type-children :gen-json-types-repr :ct-obj-dump :ct-obj-field :ct-obj-id :get-type-name
			:get-fields-for-types :ct-field :ct-field-name :ct-field-caption :ct-field-required :ct-field-expl-value :ct-field-possible-values
			:ct-obj-set-field :merge-objects :ct-obj-in-type :ct-obj-copy)
	(:use :common-lisp)
)

(in-package :utils)

(defstruct ct-obj
	id
	types	; list
	fields)	; hash

(defstruct ct-field
	name
	caption
	required
	expl-value
	possible-values ; ids of types
	private)

(defstruct ct-type
	id
	pid
	nameru
	nameen
	fields		; hash
	children)	; list

(defun ct-obj-copy (o)
 (let ((new (make-ct-obj)))
  (setf (ct-obj-id new) (ct-obj-id o))
  (setf (ct-obj-types new) (ct-obj-types o))
  (setf (ct-obj-fields new) (copy-hash (ct-obj-fields o)))
  new))

(defun merge-objects (orig new)
	
	(let ((obj-types-hash (make-hash-table :test #'equal))
			(typeslist (utils:make-smart-vec)))
		
		(map nil
			#'(lambda (tp)
				(setf (gethash tp obj-types-hash) t))
			(ct-obj-types orig))
	
		(map nil
			#'(lambda (tp)
				(setf (gethash tp obj-types-hash) t))
			(ct-obj-types new))
		
		(maphash 
			#'(lambda (k v)
				(vector-push-extend k typeslist))
			obj-types-hash)
		
		(setf (ct-obj-types orig) typeslist)
		
		(maphash
			#'(lambda (k v)
				(setf (gethash k (ct-obj-fields orig)) v))
			(ct-obj-fields new))
	orig))

(defun ct-obj-in-type (obj type)
	(member type (ct-obj-types obj)))

(defun ct-field-ispublic (field)
	(null (ct-field-private field)))

(defun ct-obj-set-field (f v obj)
	(setf (gethash f (ct-obj-fields obj)) v))

(defun ct-obj-field (field obj)
	(if (null obj)
		nil
		(gethash field (ct-obj-fields obj))))

(defun get-type-name (types id)
	(let ((type (gethash id types)))
		(if (null type)
			nil
			(ct-type-nameru type))
	))

(defun parse-ct-type (line)
	(let ((tokens (split #\Tab line))
			(type (make-ct-type)))
				(setf (ct-type-fields type) (make-hash-table :test #'equal))
				(map nil #'(lambda (v)
								(if (string= v "")
									(setf tokens (cdr tokens))))
					tokens)
				;(map nil #'(lambda (v)
				;				(format t "~A|" v))
				;	tokens)
				;(format t "~%")
				(setf (ct-type-id type) (parse-integer (car tokens)))
				(setf (ct-type-pid type) (parse-integer (second tokens)))
				(setf (ct-type-nameru type) (third tokens))
				(setf (ct-type-nameen type) (fourth tokens))
				(if (fifth tokens)
					(let ((parsed (yason:parse (fifth tokens))))
						(map nil
							#'(lambda (field_info)
									(let ((field (make-ct-field :name (gethash "f" field_info)
															:caption (gethash "c" field_info)
															:required (gethash "r" field_info)
															:expl-value (gethash "expl" field_info)
															:private (gethash "priv" field_info))))
												
												(setf (gethash (gethash "f" field_info) (ct-type-fields type)) field)))
							parsed)))
				type))

(defun build-inheritance (types)
	
	; set children for each type
	(maphash #'(lambda (id v)
					(let ((p (gethash (ct-type-pid v) types)))
					
						(if (null (ct-type-children p))
							(setf (ct-type-children p) (utils:make-smart-vec)))
					
						(vector-push-extend	id (ct-type-children p)))) 
		types)
	
	; inherit all fields to children
	(maphash #'(lambda (id v)
					(map nil #'(lambda (cid)
									(let ((c (gethash cid types)))
										(maphash #'(lambda (fn f)
														(setf (gethash fn (ct-type-fields c)) f))
											(ct-type-fields v))))
						(ct-type-children v)))
		types)
	
	; set possible values for "expl" fields
	(maphash #'(lambda (id type)
					(maphash #'(lambda (fname field)
									(if (ct-field-expl-value field)
										(setf (ct-field-possible-values field)
											 	(ct-type-children (gethash (ct-field-expl-value field) types)))))
						(ct-type-fields type)))
		types)
	
	types)

(defun print-types (types)
	(maphash #'(lambda (id v)
						(format t "id:~A ~%pid:~A ~%nameru:~A ~%nameen:~A ~%fields: " id (ct-type-pid v) (ct-type-nameru v) (ct-type-nameen v) )
						(maphash #'(lambda (k f)
										(format t " ~A " (ct-field-name f)))
								(ct-type-fields v))
						(format t "~%children: ")
						(map nil #'(lambda (c)
										(format t "~A " c))
								(ct-type-children v))
						(format t "~%~% "))
		types))

(defun read-ct-file (filename)
	(let ((in (open filename :if-does-not-exist nil :external-format :utf-8))
			(types (make-hash-table :test #'equal)))
				(when in
					(loop for line = (read-line in nil)
						while line do
								(let ((type (parse-ct-type line)))
									(setf (gethash (ct-type-id type) types) type)))
				(close in))
		(build-inheritance types)))

(defun gen-json-types-repr (types)
	(let ((ret (make-hash-table :test #'equal)))
		(maphash #'(lambda (id type)
						(let ((type-repr (make-hash-table :test #'equal)))
							(setf (gethash "id" type-repr) (ct-type-id type))
							(setf (gethash "pid" type-repr) (ct-type-pid type))
							(setf (gethash "nameru" type-repr) (ct-type-nameru type))
							(setf (gethash "nameen" type-repr) (ct-type-nameen type))	
							(aif (ct-type-children type)
								(setf (gethash "children" type-repr) it))
							(setf (gethash (inttostr id) ret) type-repr)))
			types)
		ret))

(defun do-get-hierarchy-hash (tp types hash)
	(setf (gethash tp hash) t)
	(let ((type (gethash tp types)))
		(if (equal tp (ct-type-pid type))
			hash
			(do-get-hierarchy-hash (ct-type-pid type) types hash))))

(defun get-hierarchy-hash (tp types)
	(nif (gethash tp types)
		nil
		(let ((hash (make-hash-table :test #'equal)))
			; check type existance
			(do-get-hierarchy-hash tp types hash))))

(defun get-fields-hash-for-types (types typesof)
	(let ((fields-hash (make-hash-table :test #'equal)))
		(map nil
			#'(lambda (typeid)
				(aif (gethash typeid types)
					(maphash
						#'(lambda (k field)
							(setf (gethash k fields-hash) field))
						(ct-type-fields it))))
			typesof)
		fields-hash))

(defun get-fields-for-types (types typesof &key public)
	(let ((hash (get-fields-hash-for-types types typesof))
			(ret (utils:make-smart-vec)))
		
			(maphash
				#'(lambda (k field)
					;if field
						(if (or (null public)
								(ct-field-ispublic field))
							(vector-push-extend field ret)))
							;
				hash)
		ret))

(defun create-ct-obj (types &key typesof fields id)

	;(format t "create-ct-obj start~%")

	(let ((obj (make-ct-obj :fields fields))
			(obj-types-hash (make-hash-table :test #'equal))
			(fields-descrs (get-fields-for-types types typesof))
			(expl-types-hash (make-hash-table :test #'equal)))

		;(pprint obj)

		; push expl values to types
		(map nil #'(lambda (field)
						(if (ct-field-expl-value field)
						(progn 
							(if (ct-obj-field (ct-field-name field) obj)
								; set
								(setf (gethash (ct-obj-field (ct-field-name field) obj) expl-types-hash) t)
								; not set
								;(if (ct-field-required field)
								;	)
								))))
			fields-descrs)
		
		;(format t "pushed expl values~%")
		
		(setf obj-types-hash (merge-hash-tables obj-types-hash expl-types-hash))
		
		(map nil #'(lambda (tp)
					(setf obj-types-hash (merge-hash-tables obj-types-hash (get-hierarchy-hash tp types))))
			typesof)
		
		; set types list
		(maphash #'(lambda (k v)
						;(format t "type: ~A~%" k )
						(setf (ct-obj-types obj) (cons k (ct-obj-types obj))))
			obj-types-hash)
		
		;(maphash #'(lambda (k v)
		;				(format t "field: ~A/~A~%" k v))
		;	fields)
		
		;(if id
		(setf (ct-obj-id obj) id)
		;(format t "create-ct-obj id : ~A~%~%~%" id)
		
		;(format t "create-ct-obj finished~%")
		
		obj))

(defun verify-ct-object (obj types)

	;(format t "verify-ct-object start~%")

	(map nil #'(lambda (tp)
					(aif (gethash tp types)
						(maphash #'(lambda (k field)
										(anif (gethash k (ct-obj-fields obj))
											(if (ct-field-required field)
												(return-from verify-ct-object (format nil "Field not set: ~A" k))))
											; check expl values
											
										)
							(ct-type-fields it))
						(return-from verify-ct-object (format nil "No Such type: ~A" tp))))
		(ct-obj-types obj))
	;(format t "verify-ct-object finished~%")
	nil)

(defun ct-obj-dump (obj)
	(anaphora:alet (make-hash-table :test #'equal)
		
		(if (ct-obj-id obj)
			(setf (gethash "id" anaphora:it) (ct-obj-id obj)))
		
		(setf (gethash "types" anaphora:it) (ct-obj-types obj))
		(setf (gethash "fields" anaphora:it) (ct-obj-fields obj))
		(utils:to-json-string anaphora:it)))

