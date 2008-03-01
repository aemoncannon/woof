;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitive functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define execute-primitive
  (lambda (state prim-name debug)
    (case prim-name
      ('write_string (prim-write_string state))
      ('do_with_handler (prim-do_with_handler state))
      ('raise_exception (prim-raise_exception state))
      ('load_block_from_file (prim-load_block_from_file state))
      ('new_instance (prim-new_instance state))
      ('create_class (prim-create_class state))
      ('super_list_of (prim-super_list_of state))
      ('class_conforms (prim-class_conforms state))
      ('show_object (prim-show_object state))
      ('show_number (prim-show_number state))
      ('show_object (prim-show_object state))
      ('list_ref (prim-list_ref state))
      ('integer_add (prim-integer_add state))
      ('integer_mult (prim-integer_mult state))
      ('integer_div (prim-integer_div state))
      ('integer_eq (prim-integer_eq state))
      ('integer_greater_than (prim-integer_greater_than state))
      ('integer_less_than (prim-integer_less_than state))
      ('integer_greater_than_or_equal (prim-integer_greater_than_or_equal state))
      ('bool_or (prim-bool_or state))
      ('bool_and (prim-bool_and state))
      ('get_instance_var (prim-get_instance_var state))
      ('set_instance_var (prim-set_instance_var state))
      ('obj_eq (prim-obj_eq state))
      ('string_concat (prim-string_concat state))
      ('list_concat (prim-list_concat state))
      ('string_length (prim-string_length state))
      ('list_length (prim-list_length state))
      (else (raise (string-append "Unknown-primitive: " (symbol->string prim-name)))))
    ))

(define prim-write_string
  (lambda (state) 
    (let* ((str (pop-stack! state))
	   (port (woof-output-port-prim-value (pop-stack! state)))
	   (prim-str (woof-string-prim-value str)))
      (display prim-str port)
      (push-stack! state str))))


(define prim-new_instance
  (lambda (state) 
    (push-stack! state (woof-instance state
				      (pop-stack! state)))))

(define prim-create_class
  (lambda (state) 
    (let* ((s-name (woof-string-prim-value
		    (pop-stack! state)))
	   (super (pop-stack! state))
	   (vars (pop-stack! state))
	   (classVars (pop-stack! state))
	   (new-class
	    (woof-class-instance
	     state
	     s-name
	     super
	     )))
      (install-class! state new-class)
      (push-stack! state new-class))))

(define prim-super_list_of
  (lambda (state)
    (push-stack! state (woof-list-instance 
			state
			(woof-class-super-list (pop-stack! state))))))

(define prim-class_conforms
  (lambda (state)
    (push-stack! state (woof-boolean-instance
			state
			(class-conforms? state
					 (pop-stack! state)
					 (pop-stack! state)
					 )))))

(define prim-show_object
  (lambda (state) 
    (let* ((class (woof-object-class (pop-stack! state)))
	   (class-name (woof-class-name class))
	   (desc (string-append
		  "<instance of "
		  class-name
		  ">"
		  )))
      (push-stack! state (woof-string-instance
			  state
			  desc)))))

(define prim-show_number
  (lambda (state) 
    (let* ((i (woof-integer-prim-value (pop-stack! state)))
	   (desc (number->string i)))
      (push-stack! state 
		   (woof-string-instance
		    state
		    desc)))))


(define prim-list_ref
  (lambda (state) 
    (let ((s-list (woof-list-prim-value (pop-stack! state)))
	  (s-index (woof-integer-prim-value (pop-stack! state))))
      (if (> (length s-list) s-index)
	  (push-stack! state (list-ref s-list s-index))
	  (push-stack! state (woof-nil state))))))

(define prim-integer_add
  (lambda (state)
    (push-stack! state (woof-integer-instance 
			state 
			(+ (woof-integer-prim-value (pop-stack! state))
			   (woof-integer-prim-value (pop-stack! state)))))))

(define prim-integer_mult
  (lambda (state)
    (push-stack! state (woof-integer-instance 
			state 
			(* (woof-integer-prim-value (pop-stack! state))
			   (woof-integer-prim-value (pop-stack! state)))))))

(define prim-integer_div
  (lambda (state)
    (push-stack! state (woof-integer-instance 
			state 
			(/ (woof-integer-prim-value (pop-stack! state))
			   (woof-integer-prim-value (pop-stack! state)))))))

(define prim-integer_eq
  (lambda (state)
    (push-stack! state (woof-boolean-instance 
			state 
			(= (woof-integer-prim-value (pop-stack! state))
			   (woof-integer-prim-value (pop-stack! state)))))))

(define prim-integer_greater_than
  (lambda (state) 
    (push-stack! state (woof-boolean-instance 
			state 
			(> (woof-integer-prim-value
			    (pop-stack! state))
			   (woof-integer-prim-value
			    (pop-stack! state)))))))

(define prim-integer_less_than
  (lambda (state) 
    (push-stack! state (woof-boolean-instance 
			state 
			(< (woof-integer-prim-value
			    (pop-stack! state))
			   (woof-integer-prim-value
			    (pop-stack! state)))))))

(define prim-bool_or
  (lambda (state) 
    (push-stack! state (woof-boolean-instance 
			state (or (woof-true? state (pop-stack! state))
				  (woof-true? state (pop-stack! state)))))))

(define prim-bool_and
  (lambda (state) 
    (push-stack! state (woof-boolean-instance 
			state (and (woof-true? state (pop-stack! state))
				   (woof-true? state (pop-stack! state)))))))

(define prim-integer_greater_than_or_equal
  (lambda (state) 
    (push-stack! state (woof-boolean-instance 
			state (>= (woof-integer-prim-value
				   (pop-stack! state))
				  (woof-integer-prim-value
				   (pop-stack! state)))))))

(define prim-get_instance_var
  (lambda (state) 
    (let ((object (pop-stack! state))
	  (field-name (woof-string-prim-value (pop-stack! state))))
      (push-stack! state (woof-object-get-field state object field-name)))))

(define prim-set_instance_var
  (lambda (state) 
    (let ((object (pop-stack! state))
	  (field-name (woof-string-prim-value (pop-stack! state)))
	  (value (pop-stack! state)))
      (push-stack!
       state
       (woof-object-set-field!
	state
	object
	field-name 
	value)))))

(define prim-obj_eq
  (lambda (state) 
    (push-stack! state (woof-boolean-instance 
			state
			(eq? (pop-stack! state) (pop-stack! state))))))

(define prim-string_concat
  (lambda (state) 
    (let ((str1 (woof-string-prim-value (pop-stack! state)))
	  (str2 (woof-string-prim-value (pop-stack! state))))
      (push-stack! state (woof-string-instance
			  state
			  (string-append str1 str2))))))

(define prim-list_concat
  (lambda (state)
    (let ((list1 (woof-list-prim-value (pop-stack! state)))
	  (list2 (woof-list-prim-value (pop-stack! state))))
      (push-stack! state 
		   (woof-list-instance
		    state
		    (append list1 list2))))))

(define prim-string_length
  (lambda (state) 
    (push-stack! state (woof-integer-instance
			state
			(string-length (woof-string-prim-value
					(pop-stack! state)))))))

(define prim-list_length
  (lambda (state) 
    (push-stack! state (woof-integer-instance
			state
			(length (woof-list-prim-value
				 (pop-stack! state)))))))


(define prim-do_with_handler
  (lambda (state)
    (let* ((try-block (pop-stack! state))
	   (error-class (pop-stack! state))
	   (handler-block (pop-stack! state))
	   (context (new-context-for-block
		     state
		     try-block
		     '())))
      (add-block-context-exception-handler!
       state
       (woof-exception-handler-instance
	state
	error-class
	handler-block
	context)
       context)
      (activate-block-context! state context))))


(define prim-raise_exception
  (lambda (state)
    (let* ((e (pop-stack! state)))
      (raise-exception! state e))))


(define prim-load_block_from_file
  (lambda (state) 
    (let* ((raw-filename (woof-string-prim-value
			  (pop-stack! state)))
	   (filename (resolve-woof-file raw-filename)))
      (call-with-input-file filename
	(lambda (p)
	  (let* ((code (compile-woof-stream p))
		 (block (woof-block-instance
			 state
			 code
			 0
			 )))
	    (push-stack! state block)))))))


