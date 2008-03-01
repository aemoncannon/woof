(define string-join
  (lambda (lst sep)
    (cond ((> (length lst) 1)
	(string-append (first lst) sep (string-join
					(cdr lst)
					sep)))
	  ((eq? 1 (length lst)) (first lst))
	  ((null? lst) ""))))
