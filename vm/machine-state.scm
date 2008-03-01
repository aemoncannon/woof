
(define-structure machine-state
  active-context globals)

(define-structure woof-globals 
  functions classes misc)

(define new-woof-globals
  (lambda ()
    (make-woof-globals
     (make-table test: string=?)
     (make-table test: string=?)
     (make-table test: string=?))))

(define new-machine-state
  (lambda ()
    (let* ((globals (new-woof-globals))
	   (state (make-machine-state 
		   #f
		   globals))
	   (nil (make-woof-object
		 #f
		 #f
		 (make-table test: string=?)))
	   (nil-prim-data (make-woof-class-prim-data
			   nil
			   (list nil)
			   "nil"))
	   (metaclass (make-woof-object
		       #f
		       #f
		       (make-table test: string=?)))
	   (metaclass-prim-data (make-woof-class-prim-data
				 nil
				 '(nil)
				 "MetaClass")))
      
      (woof-object-prim-data-set! nil nil-prim-data)
      (woof-object-class-set! nil nil)
      (install-class! state nil)
      (install-global! state nil "nil")
      
      (woof-object-prim-data-set! metaclass metaclass-prim-data)
      (woof-object-class-set! metaclass metaclass)
      (install-class! state metaclass)
      
      (install-class! state (woof-class-instance state "Class" nil))
      (install-class! state (woof-class-instance state "Object" nil))
      
      ;;Set MetaObject's super to Class
      (woof-class-super-set!
       (woof-object-class (class-by-name state "Object"))
       (class-by-name state "Class"))
      
      
      (install-class! state (woof-class-instance state "String" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "Integer" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "List" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "Function" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "Block" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "BlockContext" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "Continuation" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "Exception" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "NoApplicableFunctionException"
						 (class-by-name state "Exception" )))
      (install-class! state (woof-class-instance state "ExceptionHandler" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "Boolean" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "True" (class-by-name state "Boolean" )))
      (install-class! state (woof-class-instance state "False" (class-by-name state "Boolean" )))
      
      (install-global! state (woof-instance state (class-by-name state "False")) "false")
      (install-global! state (woof-instance state (class-by-name state "True")) "true")
      
      (install-class! state (woof-class-instance state "Maybe" (class-by-name state "Object" )))
      
      (install-class! state (woof-class-instance state "Just" (class-by-name state "Maybe" )))
      
      (install-class! state (woof-class-instance state "Nothing" (class-by-name state "Maybe" )))
      
      (install-class! state (woof-class-instance state "Port" (class-by-name state "Object" )))
      (install-class! state (woof-class-instance state "OutputPort" (class-by-name state "Port" )))
      
      (let ((stdout (woof-instance state (class-by-name state "OutputPort"))))
	(woof-object-prim-data-set! stdout (make-woof-output-port-prim-data (current-output-port)))
	(install-global! state stdout "STDOUT"))
      
      (install-function! state (woof-function-instance
				state
				"define:over:as:"
				(list (class-by-name state "String")
				      (class-by-name state "List")
				      (class-by-name state "Block"))
				(woof-block-instance
				 state
				 '((LVAR 0 0)
				   (LVAR 0 1)
				   (LVAR 0 2)
				   (CRFN)
				   (GETC "nil")
				   (RETURN)
				   )
				 0)
				))
      
      (install-function! state (woof-function-instance
				state
				"require:"
				(list (class-by-name state "String"))
				(woof-block-instance
				 state
				 '((LVAR 0 0)
				   (PRIM load_block_from_file)
				   (CALLJ 0)
				   (GETC "nil")
				   (RETURN)
				   )
				 0)
				))
      
      state
      )))


(define with-new-machine-state
  (lambda (func)
    (func (new-machine-state))))



(define ready-machine-state-for-code!
  (lambda (state code)
    (let* ((context (new-top-level-context-for-block 
		     state
		     (new-top-level-block
		      state
		      code))))
      (machine-state-active-context-set! state context)
      state
      )))


(define new-top-level-block
  (lambda (state code)
    (woof-block-instance
     state
     code
     0
     )))

;; Create activation context for block..
(define new-top-level-context-for-block
  (lambda (state block)
    (let* ((globals (machine-state-globals state))
	   (tmps (make-vector 0))
	   (new-context (woof-block-context-instance
			 state
			 (woof-block-code-pc block) ;pc
			 block              ;block
			 '()              ;stack
			 (woof-nil state) ;sender
			 (woof-nil state) ;home
			 tmps             ;tmps
			 '()              ;env
			 '()              ;exception handlers
			 )))
      (woof-block-home-set! block new-context)
      (block-context-env-set! new-context (list new-context))
      new-context)))

(define new-context-for-block
  (lambda (state block args)
    (let* ((num-args (length args))
	   (home (woof-block-home block))
	   (tmps (let ((tmps (make-vector num-args)))
		   (do ((i 0 (+ i 1)))
		       ((= i num-args))
		     (vector-set! tmps i (list-ref args i)))
		   tmps
		   ))
	   (new-context (woof-block-context-instance
			 state
			 (woof-block-code-pc block)
			 block
			 '()
			 (machine-state-active-context state)
			 home
			 tmps
			 '()
			 '()
			 )))
      (if (nil? state home)
	  (block-context-env-set! new-context
				  (list new-context))
	  (block-context-env-set! new-context
				  (cons new-context (block-context-env home))))
      new-context)))


(define install-global!
  (lambda (state obj name)
    (let ((globals (machine-state-globals state)))
      (table-set! (woof-globals-misc globals)
		  name
		  obj))))

(define install-class!
  (lambda (state class)
    (let ((globals (machine-state-globals state)))
      (install-global! state class (woof-class-name class)))))

(define install-function!
  (lambda (state w-func)
    (let ((globals (machine-state-globals state)))
      (let* ((sig (woof-function-sig w-func))
	     (new-group (append (list w-func)
				(table-ref
				 (woof-globals-functions globals)
				 sig
				 '()))))
	(table-set! (woof-globals-functions globals) sig new-group)))))


(define global-by-name
  (lambda (state name)
    (let ((globals (machine-state-globals state)))
      (table-ref (woof-globals-misc globals) name))))

(define class-by-name
  (lambda (state name)
    (global-by-name state name)))

(define activate-block-context!
  (lambda (state context)
    (machine-state-active-context-set! state context)))

(define top-of-stack
  (lambda (state)
    (first (machine-state-stack state))))

(define tos top-of-stack)

(define top-n-of-stack
  (lambda (state n)
    (take (machine-state-stack state) n)))

(define pop-n-of-stack!
  (lambda (state n)
    (let ((result (take (machine-state-stack state) n)))
      (do ((i 0 (+ i 1)))
	  ((= i n))
	(pop-stack! state))
      result)))

(define push-stack!
  (lambda (state val)
    (let* ((context (machine-state-active-context state))
	   (stack (block-context-stack context)))
      (block-context-stack-set! context (cons val stack)))))

(define pop-stack!
  (lambda (state)
    (let* ((context (machine-state-active-context state))
	   (stack (block-context-stack context))
	   (val (first stack)))
      (block-context-stack-set! context (cdr stack))
      val)))

(define inc-pc!
  (lambda (state)
    (let* ((context (machine-state-active-context state))
	   (pc (block-context-pc context)))
      (block-context-pc-set! context (+ 1 pc)))))

(define current-instr
  (lambda (state)
    (instr-at (machine-state-code state)
	      (machine-state-pc state))))

(define opcode
  (lambda (instr)
    (first instr)))

(define arg1
  (lambda (instr)
    (second instr)))

(define arg2
  (lambda (instr)
    (third instr)))

(define instr-at
  (lambda (code i)
    (list-ref code i)))

(define env-var-set!
  (lambda (state i j obj)
    (let* ((context (machine-state-active-context state))
	   (env (block-context-env context))
	   (target-context (list-ref env i))
	   (target-frame (block-context-tmps target-context)))
      (if (>= j (vector-length target-frame))
	  (block-context-tmps-set! target-context (vector-append 
						   target-frame 
						   '#(0 0 0 0 0 0 0))))
      (vector-set! (block-context-tmps target-context) j obj))))


(define env-get-var
  (lambda (state i j)
    (let* ((context (machine-state-active-context state))
	   (env (block-context-env context))
	   (target-context (list-ref env i))
	   (target-frame (block-context-tmps target-context)))
      (vector-ref target-frame j))))

(define machine-state-env
  (lambda (state)
    (block-context-env (machine-state-active-context state))))

(define add-block-context-exception-handler!
  (lambda (state handler context)
    (block-context-exception-handlers-set!
     context
     (cons handler (block-context-exception-handlers context)))))

(define print-env
  (lambda (state)
    (let ((env (machine-state-env state)))
      (map (lambda (ea) (begin (display "(")
			       (vector-for-each
				(lambda (var)
				  (if (woof-object? var)
				      (print-woof-object var)))
				(block-context-tmps ea))
			       (display ")")))
	   env))))


(define machine-state-stack
  (lambda (state)
    (block-context-stack (machine-state-active-context state))))

(define machine-state-pc
  (lambda (state)
    (block-context-pc (machine-state-active-context state))))

(define machine-state-pc-set!
  (lambda (state pc)
    (let ((context (machine-state-active-context state)))
      (block-context-pc-set! context pc))))

(define machine-state-code
  (lambda (state)
    (woof-block-code
     (block-context-block
      (machine-state-active-context state)))))

(define run-machine-state-tests
  (lambda ()
    (run-tests
     (list
      
      (lambda ()
	(with-new-machine-state
	 (lambda (state)
	   (let* ((obj (woof-nil state)))
	     (assert-true (is-a? obj (global-by-name state "nil"))
			  msg: "nil should be of class nil")))))
      
      (lambda ()
	(with-new-machine-state
	 (lambda (state)
	   (let* ((obj (woof-boolean-instance state #t)))
	     (assert-true (is-a? obj (global-by-name state "True"))
			  msg: "true should be of class True")))))
      
      (lambda ()
	(with-new-machine-state
	 (lambda (state)
	   (let* ((obj (woof-boolean-instance state #f)))
	     (assert-true (is-a? obj (global-by-name state "False"))
			  msg: "false should be of class False")))))
      
      ))))