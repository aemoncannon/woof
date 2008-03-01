
(define functions-by-sig
  (lambda (state sig)
    (let ((globals (machine-state-globals state)))
      (table-ref (woof-globals-functions globals) sig '()))))


(define applicable-functions
  (lambda (state s-sig s-args)
    (let ((globals (machine-state-globals state)))
      (let ((arg-classes
	     (map (lambda (ea) (woof-object-class ea))
		  s-args)))

	(sort-functions-by-specificity
	 (filter (lambda (f)
		   (if (not (= (length arg-classes) (length (woof-function-specs f))))
		       (step))
		   (every (lambda (pair) (class-conforms? state (first pair) (second pair )))
			  (zip arg-classes (woof-function-specs f))))
		 (functions-by-sig state s-sig)))))))


;; Check if c1 is a subtype of c2 or equal to c2
(define class-conforms?
  (lambda (state c1 c2)
    (or (eq? c1 c2)
	(let ((c1-super (woof-class-super c1)))
	  (if (not (eq? c1 (class-by-name state "nil")))
	      (class-conforms? state c1-super c2)
	      #f)))))



(define class-more-specific?
  (lambda (c1 c2)
    (> (length (woof-class-super-list c1))
       (length (woof-class-super-list c2)))))


;; sorts the funcs from most specific to least specific
(define sort-functions-by-specificity
  (lambda (funcs)
    (quicksort funcs (lambda (f1 f2)
		       (any (lambda (c1 c2)
			      (class-more-specific? c1 c2))
			    (woof-function-specs f1)
			    (woof-function-specs f2))))))



(define run-function-calling-tests
  (lambda ()
    (run-tests
     (list
      
      
      (lambda ()
	(with-new-machine-state
	 (lambda (state)
	   (let* ((func-list (applicable-functions state 
						   "define:over:as:"
						   (list
						    (woof-string-instance state "kiss:")
						    (woof-list-instance state (list
									       (class-by-name state 
											      "Integer")))
						    (woof-block-instance state '() 0)))))
	     (assert-true (equal? (length func-list) 1)
			  msg: "should only be 1 applicable function")
	     
	     (assert-true (is-a? (first func-list) (class-by-name state "Function"))
			  msg: "should be a Function")
	     
	     ))))
      
      
      
      
      
      
      (lambda ()
	(with-new-machine-state
	 (lambda (state)
	   (install-function! state (woof-function-instance
				     state
				     "yelp:to:"
				     (list (class-by-name state "String")
					   (class-by-name state "Integer"))
				     (woof-block-instance state '() 0)
				     ))
	   (install-function! state (woof-function-instance
				     state
				     "yelp:to:"
				     (list (class-by-name state "String")
					   (class-by-name state "Object"))
				     (woof-block-instance state '() 0)
				     ))
	   (install-function! state (woof-function-instance
				     state
				     "yelp:to:"
				     (list (class-by-name state "Integer")
					   (class-by-name state "Integer"))
				     (woof-block-instance state '() 0)
				     ))
	   (install-function! state (woof-function-instance
				     state
				     "yelp:to:"
				     (list (class-by-name state "List")
					   (class-by-name state "Integer"))
				     (woof-block-instance state '() 0)
				     ))
	   (let* ((func-list (applicable-functions state 
						   "yelp:to:"
						   (list
						    (woof-string-instance state "Hello")
						    (woof-integer-instance state 5)))))
	     (assert-true (equal? (length func-list) 2)
			  msg: "there should be 2 applicable functions")
	     ))))
      
))))