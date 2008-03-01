(define resolve-woof-file
  (lambda (file)
    (let ((in-modules (string-append (getenv "WOOF_MODULES") "/" file))
	  (in-cwd (string-append "./" file)))
      (cond ((file-exists? in-cwd) in-cwd)
	    ((file-exists? in-modules) in-modules)
	    (else (raise (string-append "File could not be found: " file)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top level eval functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-woof-from-filename-with-prelude
  (lambda (filename #!key (debug #f))
    (eval-woof-string (string-append "require: 'prelude.woof'."
			      "evalFile: '" filename "'"))))
(define eval-woof-string
  (lambda (woof-str #!key (debug #f))
    (eval-woof-stream (open-input-string woof-str))))

(define eval-woof-stream
  (lambda (file-port #!key (debug #f))
    (eval-woof-bytecode (compile-woof-stream file-port))))

(define eval-woof-bytecode
  (lambda (bc #!key (debug #f))
    (machine bc)))

;; Run bytecode directly

(define eval-woof-bytecode-from-filename
  (lambda (filename #!key (debug #f))
    (call-with-input-file filename
      (lambda (port)(eval-woof-bytecode-stream port)))))

(define eval-woof-bytecode-stream
  (lambda (file-port #!key (debug #f))
    (let ((bc (read-all file-port)))
      (machine bc))))


;;;;;;;;;;;;;;;;;;;;;
;; the interpreter ;;
;;;;;;;;;;;;;;;;;;;;;

(define machine
  (lambda (code #!key (state (new-machine-state))(debug #f))
    (ready-machine-state-for-code! state code)
    
    (with-exception-catcher
     (lambda (e) (cond
		  ((list? e) (case (first e)

			       ('halt state)

			       ('no-exception-handler-found
				(raise (woof-exception-msg (second e))))

			       (else (raise e))

			       ))
		  (#t (raise e))))
     
     (lambda ()
       (let loop ()
	 (let* ((instr (current-instr state))
		(opcode (opcode instr)))
	   (begin
	     (if debug
		 (begin
		   (print (machine-state-pc state))
		   (print instr)
		   (display "\n")))
	     (inc-pc! state)
	     (case opcode
	       ('POP (do-POP state instr))
	       
	       ('CONST  (do-CONST state instr))
	       ('GETC  (do-GETC state instr))
	       ('GETG  (do-GETG state instr))
	       ('SETG  (do-SETG state instr))
	       
	       ('STRL  (do-STRL state instr))
	       ('LSTL  (do-LSTL state instr))
	       ('INTL  (do-INTL state instr))
	       ('BLKL  (do-BLKL state instr))
	       ('CRFN  (do-CRFN state instr))
	       
	       ('GVAR  (do-GVAR state instr))
	       ('LVAR  (do-LVAR state instr))
	       ('LSET  (do-LSET state instr))
	       
	       ('JUMP  (do-JUMP state instr))
	       ('FJUMP (do-FJUMP state instr))
	       ('TJUMP (do-TJUMP state instr))
	       
	       ('FUNC  (do-FUNC state instr))
	       ('CALLJ (do-CALLJ state instr))
	       ('LSTCALLJ (do-LSTCALLJ state instr))
	       ('RETURN (do-RETURN state instr))
	       
	       ('SAVECC  (do-SAVECC state instr))
	       ('RETCC  (do-RETCC state instr))
	       
	       ('PRIM  (do-PRIM state instr))
	       
	       ('HALT  (do-HALT state instr))
	       
	       (else (error "UNKNOWN OPCODE")))
	     
	     (if debug
		 (begin
		   (display "STACK: ")
		   (map (lambda (ea) (if (woof-object? ea)
					 (print-woof-object ea)
					 ;; (print ea)
					 )) (machine-state-stack state))
		   (display "\n")
		   (display "ENV: ")
		   (print-env (machine-state-env state))
		   (display "\n\n")))
	     (loop)
	     )))))))


  ;;;;;;;;;;;;;
;; testing ;;
  ;;;;;;;;;;;;;

(define with-state-with-active-context
  (lambda (func)
    (with-new-machine-state
     (lambda (state)
       (ready-machine-state-for-code! state '((HALT)))
       (func state)))))


(define with-state-with-testing-functions
  (lambda (func)
    (with-new-machine-state
     (lambda (state)
       (install-function! state (woof-function-instance
				 state
				 "echo:"
				 (list (class-by-name state "Integer"))
				 (woof-block-instance
				  state
				  '(
				    (LVAR 0 0)
				    (RETURN)
				    )
				  0)
				 ))
       (func state)))))


(define run-machine-tests
  (lambda ()
    (run-tests
     (list
      
      (lambda ()
	"Test CONST"
	(let ((state (machine '((CONST 1) (HALT)))))
	  (assert-equal? (tos state) 1)))
      
      (lambda ()
	"test POP"
	(let ((state (machine
		      '((CONST 1)
			(CONST 2)
			(CONST 3)
			(POP)
			(HALT)
			))))
	  (assert-equal? (tos state) 2)))
      
      (lambda ()
	"pop empty stack"
	(assert-true (with-exception-catcher 
		      (lambda (e) #t) 
		      (lambda () (machine '((POP)
					    (HALT)
					    ))))))
      
      
      (lambda ()
	"Test JUMP"
	(let ((state (machine
		      '((CONST 1)
			(JUMP 3)
			(CONST 5)
			(HALT)
			))))
	  (assert-equal? (tos state) 1)))
      
      (lambda ()
	"Test FJUMP fails"
	(let ((state (machine
		      '((CONST 1)
			(CONST #t)
			(FJUMP 4)
			(CONST 5)
			(HALT)
			))))
	  (assert-equal? (tos state) 5)))
      
      (lambda ()
	"Test FJUMP succeeds"
	(let ((state (machine
		      '((CONST 1)
			(CONST #f)
			(FJUMP 4)
			(CONST 5)
			(HALT)
			))))
	  (assert-equal? (tos state) 1)))
      
      (lambda ()
	"Test TJUMP fails"
	(let ((state (machine
		      '((CONST 1)
			(CONST #f)
			(TJUMP 4)
			(CONST 5)
			(HALT)
			))))
	  (assert-equal? (tos state) 5)))
      
      (lambda ()
	"Test TJUMP succeeds"
	(let ((state (machine
		      '((CONST 1)
			(CONST #t)
			(TJUMP 4)
			(CONST 5)
			(HALT)
			))))
	  (assert-equal? (tos state) 1)))
      
      (lambda ()
	"Test LSTL"
	(let ((state (machine
		      '((INTL 1)
			(INTL 2)
			(INTL 3)
			(LSTL 3)
			(HALT)
			))))
	  (assert-equal? (length (woof-list-prim-value (tos state))) 3)))
      
      (lambda ()
	"Test LSET and LVAR"
	(let ((state (machine
		      '(
			(CONST 4)
			(LSET 0 3)
			(CONST 6)
			(LSET 0 5)
			(LVAR 0 3)
			(HALT)
			))))
	  (assert-equal? (tos state) 4)
	  ))
      
      (lambda ()
	"Test LSET with bad frame index"
	(assert-true (with-exception-catcher (lambda (e) #t)
					     (lambda () (machine
							 '(
							   (CONST 5)
							   (LSET 1 1)
							   (HALT)))))))
      
      (lambda ()
	"Test env-var-set!"
	(with-state-with-active-context
	 (lambda (state)
	   (env-var-set! state 0 2 6)
	   (assert-equal? (env-get-var state 0 2) 6)
	   )))
      
      (lambda ()
	"Test env-var-set!"
	(with-state-with-active-context
	 (lambda (state)
	   (env-var-set! state 0 5 6)
	   (env-var-set! state 0 2 6)
	   (assert-equal? (env-get-var state 0 5) 6)
	   (assert-equal? (env-get-var state 0 2) 6)
	   (assert-equal? (env-get-var state 0 4) 0)
	   )))
      
      
      (lambda ()
	"test FUNC opcode"
	(with-state-with-testing-functions
	 (lambda (state)
	   (machine '((INTL 1)
		      (FUNC "echo:" 1)
		      (HALT))
		    state: state
		    )
	   (assert-true (woof-block? (tos state))))))
      
      
      (lambda ()
	"test CALLJ"
	(with-state-with-testing-functions
	 (lambda (state)
	   (machine '(
		      (INTL 1)
		      (FUNC "echo:" 1)
		      (CALLJ 1)
		      (HALT))
		    state: state
		    )
	   (assert-equal? (woof-integer-prim-value (tos state)) 1)
	   )))
      
      (lambda ()
	"test LSTCALLJ"
	(with-state-with-testing-functions
	 (lambda (state)
	   (machine '(
		      
		      (INTL 1)
		      (LSTL 1)
		      (BLKL 3)
		      (LVAR 0 0)
		      (RETURN)
		      (LSTCALLJ 1)
		      (HALT))
		    state: state
		    )
	   (assert-equal? (woof-integer-prim-value (tos state)) 1)
	   )))
      
      
      (lambda ()
	"test BLKL opcode"
	(with-state-with-testing-functions
	 (lambda (state)
	   (machine '(
		      (BLKL 3)
		      (INTL 5)
		      (RETURN)
		      (HALT))
		    state: state
		    )
	   (assert-true (woof-block? (tos state))))))
      
      
      (lambda ()
	"test CALLJ on BLKL-generated anonymous block"
	(with-state-with-testing-functions
	 (lambda (state)
	   (machine '(
		      
		      (BLKL 3)
		      (INTL 5)
		      (RETURN)
		      (CALLJ 0)
		      (HALT))
		    state: state
		    )
	   (assert-equal? (woof-integer-prim-value (tos state)) 5))
	 ))
      
      (lambda ()
	"Save block to local var, call later"
	(with-state-with-testing-functions
	 (lambda (state)
	   (machine '(
		      (BLKL 3)
		      (STRL "YO")
		      (RETURN)
		      (LSET 0 0)
		      (INTL 2)
		      (POP)
		      (LVAR 0 0)
		      (CALLJ 1)
		      (HALT))
		    state: state
		    )
	   (assert-equal? (woof-string-prim-value (tos state)) "YO"))
	 ))
      
      (lambda ()
	"Test block closure over local var"
	(with-state-with-testing-functions
	 (lambda (state)
	   (machine '(				
		      (BLKL 8) ;; def outer block
		      (STRL "APPLE")
		      (LSET 0 0)
		      (POP) ;; pop after assignment to local
		      (BLKL 3) ;; def inner block
		      (LVAR 1 0) ;; references var from outer block's scope
		      (RETURN)
		      (RETURN)
		      
		      (LSET 0 0) ;; save outer block instance as local
		      (POP)
		      
		      (LVAR 0 0)
		      (CALLJ 0) ;; call outer block instance
		      
		      (LSET 0 1) ;; save result, inner block instance, to local
		      (POP)
		      
		      (LVAR 0 1)
		      (CALLJ 0) ;; call inner block instance
		      
		      (HALT))
		    state: state
		    )
	   (assert-equal? (woof-string-prim-value (tos state)) "APPLE"))))
      
      
      (lambda ()
	"test GETC"
	(with-state-with-testing-functions
	 (lambda (state)
	   (machine '(
		      (GETC "Integer")
		      (HALT))
		    state: state
		    )
	   (assert-true (woof-class? (tos state))))
	 ))
      
      
      (lambda ()
	"test CFRN"
	(with-state-with-testing-functions
	 (lambda (state)
	   (machine '(
		      (BLKL 3)
		      (INTL 5)
		      (RETURN)
		      (GETC "Integer")
		      (LSTL 1)
		      (STRL "ape:")
		      (CRFN)
		      (INTL 1)
		      (FUNC "ape:" 1)
		      (HALT))
		    state: state
		    )
	   (assert-true (woof-block? (tos state))))))
      
      ))))
















