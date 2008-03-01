
;; Starting at active-context, search down the sender chain for the first
;; applicable exception handler.
(define next-handler
  (lambda (state exception-object)
    (let* ((exception-class (woof-object-class exception-object)))
      (letrec ((find-handler
		(lambda (context)
		  (let ((handlers (applicable-handlers state exception-class context))
			(next-context (block-context-sender context)))
		    (if (not (null? handlers))
			(first handlers)
			(if (block-context? next-context)
			    (find-handler next-context)
			    (raise (list 'no-exception-handler-found exception-object))))
		    ))))
	(find-handler (machine-state-active-context state))))))


;; For given context, return any applicable exception handlers
(define applicable-handlers
  (lambda (state exception-class context)
    (block-context-exception-handlers context)))


(define raise-exception!
  (lambda (state e)
    (let* ((handler (next-handler state e))
	   (handler-block (exception-handler-handler-block handler))
	   (handler-sender (block-context-sender (exception-handler-context handler)))
	   (handler-context (new-context-for-block
			     state
			     handler-block
			     (list e))))
      (block-context-sender-set! handler-context handler-sender)
      (activate-block-context! state handler-context))))
