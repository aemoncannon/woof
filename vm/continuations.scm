(define copy-block-context-for-continuation
  (lambda (state context)
    (let* ((sender (block-context-sender context))
	   (home (block-context-home context)))
      (woof-block-context-instance
       state
       (block-context-pc context)
       (block-context-block context)
       (list-copy (block-context-stack context))
       (if (block-context? sender)
	   (copy-block-context-for-continuation state sender)
	   sender)
       (if (block-context? home)
	   (copy-block-context-for-continuation state home)
	   home)
       (block-context-tmps context)
       (list-copy (block-context-env context))
       (list-copy (block-context-exception-handlers context))
       ))))
