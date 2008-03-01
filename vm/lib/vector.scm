
(define (vector-for-each proc v)
  (let ((i 0))
    (let loop ()
      (if (not (= i (vector-length v)))
	  (begin 
	    (proc (vector-ref v i))
	    (set! i (+ 1 i))
	    (loop))))))