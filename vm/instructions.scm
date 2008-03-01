  ;;;;;;;;;;;;;;;;;;;;;;
;; CPU instructions ;;
  ;;;;;;;;;;;;;;;;;;;;;;

;; Push primitive scheme constant to stack (not in use)
(define do-CONST
  (lambda (state instr)
    (push-stack! state (arg1 instr))))

;; Push value of constant to stack.
(define do-GETC
  (lambda (state instr)
    (push-stack! state (global-by-name state (arg1 instr)))))

;; Push value of constant to stack.
(define do-GETG
  (lambda (state instr)
    (push-stack! state (global-by-name state (arg1 instr)))))

;; Push value of global to stack.
(define do-SETG
  (lambda (state instr)
    (install-global! state
		     (top-of-stack state)
		     (arg1 instr))))

(define do-STRL
  (lambda (state instr)
    (push-stack! state 
		 (woof-string-instance
		  state
		  (arg1 instr)))))


(define do-LSTL
  (lambda (state instr)
    (push-stack! state (woof-list-instance
			state
			(let ((items '()))
			  (do ((i 0 (+ i 1)))
			      ((= i (arg1 instr)))
			    (set! items
				  (append items (list (pop-stack! state)))))
			  items)))))

(define do-INTL
  (lambda (state instr)
    (push-stack! state (woof-integer-instance
			state (arg1 instr)))))


(define do-BLKL
  (lambda (state instr)
    (begin
      (push-stack! state (woof-block-instance
			  state
			  (machine-state-code state)
			  (machine-state-pc state)
			  home: (machine-state-active-context state)
			  ))
      (let ((pc (machine-state-pc state)))
	(machine-state-pc-set!
	 state
	 (- (+ pc (arg1 instr)) 1)))
      )))

;; install a function in global namespace
(define do-CRFN
  (lambda (state instr)
    (begin
      (install-function! state (woof-function-instance
				state
				(woof-string-prim-value
				 (pop-stack! state))
				(woof-list-prim-value
				 (pop-stack! state))
				(pop-stack! state))))))


(define do-POP
  (lambda (state instr)
    (pop-stack! state)))

(define do-GVAR
  (lambda (state instr)
    (push-stack! state (global-by-name 
			state 
			(woof-string-prim-value (arg1 instr))))))

(define do-LVAR
  (lambda (state instr)
    (push-stack! state (env-get-var
			state
			(arg1 instr)
			(arg2 instr)))))


(define do-LSET
  (lambda (state instr)
    (env-var-set!
     state
     (arg1 instr)
     (arg2 instr)
     (top-of-stack state))))

(define do-JUMP
  (lambda (state instr)
    (machine-state-pc-set! state (arg1 instr))))

(define do-FJUMP
  (lambda (state instr)
    (if (not (pop-stack! state))
	(machine-state-pc-set! state (arg1 instr)))))

(define do-TJUMP
  (lambda (state instr)
    (if (pop-stack! state)
	(machine-state-pc-set! state (arg1 instr)))))



;; FUNC (signature) (number of arguments)
(define do-FUNC
  (lambda (state instr)
    (let* ((func-name (arg1 instr))
	   (args (reverse (top-n-of-stack 
			   state (arg2 instr))))
	   (funcs
	    (applicable-functions
	     state
	     func-name
	     args
	     )))
      (if (> (length funcs) 0)
	  (push-stack! state (woof-function-block (first funcs)))
	  
	  ;; No function found!
	  (raise-exception! state (woof-exception-instance 
				   state
				   (string-append
				    "No applicable functions found for selector: "
				    func-name
				    ", for arguments of type ("
				    (string-join
				     (map (lambda (ea) (woof-class-name
							(woof-object-class ea))) args) ", ")
				    ")"
				    )
				   class: (global-by-name state "NoApplicableFunctionException")
				   ))))))

;; SAVECC (pc offset on return)
(define do-SAVECC
  (lambda (state instr)
    (let ((saved-context
	   (copy-block-context-for-continuation
	    state
	    (machine-state-active-context state))))
      (block-context-pc-set! saved-context
			     (- (+ (machine-state-pc state)
				   (arg1 instr)) 1))
      (push-stack!
       state
       (woof-continuation-instance
	state
	saved-context)
       ))))

;; RETCC
(define do-RETCC
  (lambda (state instr)
    (let* ((val (pop-stack! state))
	   (cc (pop-stack! state))
	   (returning-to
	    (copy-block-context-for-continuation
	     state
	     (woof-continuation-context cc))))
      (activate-block-context! state returning-to)
      (push-stack! state val))))

;; CALLJ, install Block context, goto top of Block code
(define do-CALLJ
  (lambda (state instr)
    (let ((b (pop-stack! state)))
      (activate-block-context! state (new-context-for-block
				      state
				      b
				      (pop-n-of-stack! state (arg1 instr) )))
      
      )))

;; Like above, but provide args as a list object
(define do-LSTCALLJ
  (lambda (state instr)
    (let* ((b (pop-stack! state))
	   (arg-list (pop-stack! state)))
      (activate-block-context! state (new-context-for-block
				      state
				      b
				      (reverse (woof-list-prim-value arg-list))))
      )))

;;Use return-addr at tos+1 to return from call
(define do-RETURN
  (lambda (state instr)
    (let ((ret-val (pop-stack! state))
	  (returning-to (block-context-sender
			 (machine-state-active-context state))))
      (activate-block-context! state returning-to)
      (push-stack! state ret-val)
      )))

;; CALL primitive function
(define do-PRIM
  (lambda (state instr)
    (let ((prim-name (arg1 instr)))
      (execute-primitive
       state
       prim-name
       #f ))))

(define do-HALT
  (lambda (state instr)
    ;; We treat HALT differently if this is not the top-level code block.
    (if (not-nil? state (block-context-sender
			 (machine-state-active-context state)))
	(do-RETURN state instr)
	(raise (list 'halt)))))

