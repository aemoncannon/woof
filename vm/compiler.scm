;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler functions  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(define compile-woof
  (lambda (woof)
    (call-with-input-string 
     woof 
     (lambda (s)(compile-woof-stream s)))))

(define compile-woof-stream
  (lambda (input-port)
    (let* ((p (open-process (list path: (getenv "WOOF_COMPILER")
				  buffering: #f))))
      (copy-char-port-to-char-port input-port p)
      (force-output p)
      (close-output-port p)
      (read-all p))))

