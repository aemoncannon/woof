
(define run-tests 
  (lambda (tests)
    (map 
     (lambda (ea) (ea)) tests)
    (pp (string-append "Finished "
		       (number->string (length tests))
		       " tests."))
    ))

(define assert-true
  (lambda (e #!key (msg ""))
    (if (not e)
	(raise (string-append
		"Assertion failed: "
		msg)))))

(define assert-not-false
  (lambda (e #!key (msg ""))
    (if (eq? e #f)
	(raise (string-append
		"Assertion failed: "
		msg)))))

(define assert-false
  (lambda (e #!key (msg ""))
    (if e
	(raise (string-append
		"Assertion failed: "
		msg)))))

(define assert-eq?
  (lambda (e1 e2 #!key (msg ""))
    (if (not (eq? e1 e2))
	(raise (string-append
		"Assertion failed: "
		msg)))))

(define assert-equal?
  (lambda (e1 e2 #!key (msg ""))
    (if (not (equal? e1 e2))
	(raise (string-append
		"Assertion failed: "
		msg)))))
