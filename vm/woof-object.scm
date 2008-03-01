;;The principle Gambit representation of a woof object
(define-structure woof-object prim-data class fields)

;;Generate accessors for major woof-types' primitive data
(define-macro (define-woof-object-type name #!rest vars)
  (let* ((prim-struct-name (string->symbol
			    (string-append
			     (symbol->string name)
			     "-prim-data")))
	 (to-prim-getter (lambda (var)
			   (string->symbol
			    (string-append
			     (symbol->string name)
			     "-prim-data-"
			     (symbol->string var)))))
	 (to-prim-setter (lambda (var)
			   (string->symbol
			    (string-append
			     (symbol->string name)
			     "-prim-data-"
			     (symbol->string var)
			     "-set!"
			     ))))
	 (to-getter (lambda (var)
		      (string->symbol
		       (string-append
			(symbol->string name)
			"-"
			(symbol->string var)))))
	 (to-setter (lambda (var)
		      (string->symbol
		       (string-append
			(symbol->string name)
			"-"
			(symbol->string var)
			"-set!"
			))))
	 (pred-name (string->symbol
		     (string-append
		      (symbol->string name)
		      "?"
		      )))
	 (prim-pred-name (string->symbol
			  (string-append
			   (symbol->string name)
			   "-prim-data?"
			   ))))
    `(begin
       (define-structure ,prim-struct-name ,@vars)
       
       ,@(map (lambda (var)
		`(define ,(to-getter var)
		   (lambda (o)
		     (,(to-prim-getter var)
		      (woof-object-prim-data o)))))
	      vars)
       
       ,@(map (lambda (var)
		`(define ,(to-setter var)
		   (lambda (o val)
		     (,(to-prim-setter var)
		      (woof-object-prim-data o) val))))
	      vars)
       
       (define ,pred-name
	 (lambda (o)
	   (,prim-pred-name 
	    (woof-object-prim-data o))))
       )))

(define-structure woof-object-prim-data)
(define-woof-object-type woof-class super super-list name)
(define-woof-object-type woof-string prim-value)
(define-woof-object-type woof-list prim-value)
(define-woof-object-type woof-integer prim-value)
(define-woof-object-type woof-function sig specs block)
(define-woof-object-type woof-block code code-pc home)
(define-woof-object-type woof-output-port prim-value)
(define-woof-object-type block-context pc block stack sender home tmps env exception-handlers)
(define-woof-object-type exception-handler error-class handler-block context)
(define-woof-object-type woof-exception msg)
(define-woof-object-type woof-continuation context)

(define woof-object-set-field!
  (lambda (state object name value)
    (table-set! (woof-object-fields object) name value)))

(define woof-object-get-field
  (lambda (state object name)
    (table-ref (woof-object-fields object) name (woof-nil state))))

;; Conveniance helpers..

(define is-a?
  (lambda (object class)
    (eq? (woof-object-class object) class)))



;; Helpers for woof classes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globally visible instances ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define woof-nil
  (lambda (state)
    (global-by-name state "nil")))

;; singleton instance of true
(define woof-true
  (lambda (state)
    (global-by-name state "true")))

;; singleton instance of false
(define woof-false
  (lambda (state)
    (global-by-name state "false")))

(define woof-boolean-instance
  (lambda (state s-bool)
    (if s-bool
	(woof-true state)
	(woof-false state)
	)))

(define nil?
  (lambda (state object)
    (eq? object (woof-nil state))))

(define not-nil?
  (lambda (state object)
    (not (nil? state object))))

(define woof-true?
  (lambda (state object)
    (eq? object (woof-true state))))

(define woof-false?
  (lambda (state object)
    (eq? object (woof-false state))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; instance utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define create-meta-class-for
  (lambda (state class)
    (let* ((globals (machine-state-globals state))
	   (super (woof-object-class (woof-class-super class)))
	   (new-class (woof-class-instance
		       state
		       (string-append "Meta" (woof-class-name class))
		       super
		       class: (global-by-name state "MetaClass")
		       )))
      (install-class! state new-class)
      new-class)))

;; Instance creation

(define woof-class-instance
  (lambda (state name super #!key (class #f))
    (let* ((prim-data (make-woof-class-prim-data 
		       super 
		       (append (list super)
			       (woof-class-super-list super))
		       name))
	   
	   ;; We have to create the metaclass for this class in a sec..(#f for now)
	   (new-object (make-woof-object 
			prim-data
			#f
			(make-table test: string=?)))
	   (my-class (if class class (create-meta-class-for state new-object))))
      
      (woof-object-class-set! new-object my-class)	       
      (woof-object-set-field! state new-object "class" my-class)
      new-object)))

(define woof-function-instance
  (lambda (state sig specs block)
    (let* ((class (global-by-name state "Function"))
	   (prim-data (make-woof-function-prim-data sig specs block))
	   (new-object (make-woof-object
			prim-data
			class
			(make-table test: string=?))))
      (woof-object-set-field! state new-object "class" class)
      new-object)))

(define woof-block-instance
  (lambda (state code code-pc #!key (home (woof-nil state)))
    (let* ((class (global-by-name state "Block"))
	   (prim-data (make-woof-block-prim-data code code-pc home))
	   (new-object (make-woof-object
			prim-data
			class
			(make-table test: string=?))))
      (woof-object-set-field! state new-object "class" class)
      new-object)))

(define woof-block-context-instance
  (lambda (state pc block stack sender home tmps env exception-handlers)
    (let* ((class (global-by-name state "BlockContext"))
	   (prim-data (make-block-context-prim-data 
		       pc 
		       block 
		       stack 
		       sender 
		       home 
		       tmps 
		       env 
		       exception-handlers))
	   (new-object (make-woof-object
			prim-data
			class
			(make-table test: string=?))))
      (woof-object-set-field! state new-object "class" class)
      new-object)))

(define woof-instance
  (lambda (state class)
    (let* ((prim-data (make-woof-object-prim-data))
	   (new-object (make-woof-object
			prim-data
			class
			(make-table test: string=?))))
      (woof-object-set-field! state new-object "class" class)
      new-object)))

(define woof-string-instance
  (lambda (state str)
    (let* ((class (global-by-name state "String"))
	   (prim-data (make-woof-string-prim-data str))
	   (new-object (make-woof-object
			prim-data
			class
			(make-table test: string=?))))
      (woof-object-set-field! state new-object "class" class)
      new-object)))

(define woof-integer-instance
  (lambda (state int)
    (let* ((class (global-by-name state "Integer"))
	   (prim-data (make-woof-integer-prim-data int))
	   (new-object (make-woof-object
			prim-data
			class
			(make-table test: string=?))))
      (woof-object-set-field! state new-object "class" class)
      new-object)))

(define woof-list-instance
  (lambda (state lst)
    (let* ((class (global-by-name state "List"))
	   (prim-data (make-woof-list-prim-data lst))
	   (new-object (make-woof-object
			prim-data
			class
			(make-table test: string=?))))
      (woof-object-set-field! state new-object "class" class)
      new-object)))

(define woof-exception-instance
  (lambda (state msg #!key (class (global-by-name state "Exception")))
    (let* ((prim-data (make-woof-exception-prim-data msg))
	   (new-object (make-woof-object
			prim-data
			class
			(make-table test: string=?))))
      (woof-object-set-field! state new-object "message" (woof-string-instance state msg))
      (woof-object-set-field! state new-object "class" class)
      new-object)))

(define woof-exception-handler-instance
  (lambda (state error-class handler-block context)
    (let* ((class (global-by-name state "ExceptionHandler"))
	   (prim-data (make-exception-handler-prim-data error-class handler-block context))
	   (new-object (make-woof-object
			prim-data
			class
			(make-table test: string=?))))
      (woof-object-set-field! state new-object "class" class)
      new-object)))

(define woof-continuation-instance
  (lambda (state context)
    (let* ((class (global-by-name state "Continuation"))
	   (prim-data (make-woof-continuation-prim-data context))
	   (new-object (make-woof-object
			prim-data
			class
			(make-table test: string=?))))
      (woof-object-set-field! state new-object "class" class)
      new-object)))

(define print-woof-object
  (lambda (o)
    (display "<a ")
    (display (woof-class-name (woof-object-class o)))
    (display ">")))



(define run-woof-object-tests
  (lambda ()
    (run-tests
     (list
      
      (lambda ()
	(with-new-machine-state
	 (lambda (state)
	   (let* ((obj (woof-nil state)))
	     (assert-true (eq? (woof-class-super obj) (woof-nil state))
			  msg: "nil's super should be nil")))))
      
      
      (lambda ()
	(with-new-machine-state
	 (lambda (state)
	   (let* ((obj (woof-nil state)))
	     (assert-true (eq? (woof-object-class obj) (woof-nil state))
			  msg: "nil's class should be nil")))))
      
      
      (lambda ()
	(with-new-machine-state
	 (lambda (state)
	   (let* ((obj (woof-nil state)))
	     (assert-true (equal? (woof-class-super-list obj) (list (woof-nil state)))
			  msg: "nil's super-list should only contain nil")))))
      
      ))))


