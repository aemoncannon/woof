
(define (copy-octet-port-to-octet-port in out)
   (let ((buf (make-u8vector 4096)))
     (let loop ()
       (let ((n (read-subu8vector buf 0 (u8vector-length buf) in)))
         (if (> n 0)
             (begin
               (write-subu8vector buf 0 n out)
               (loop)))))))

(define (copy-char-port-to-char-port in out)
   (let ((buf (make-string 4096)))
     (let loop ()
       (let ((n (read-substring buf 0 (string-length buf) in)))
         (if (> n 0)
             (begin
               (write-substring buf 0 n out)
               (loop)))))))

(define (copy-binary-file in-filename out-filename)
   (call-with-input-file in-filename
     (lambda (in)
       (call-with-output-file out-filename
         (lambda (out)
           (copy-octet-port-to-octet-port in out))))))

(define (copy-text-file in-filename out-filename)
   (call-with-input-file in-filename
     (lambda (in)
       (call-with-output-file out-filename
         (lambda (out)
           (copy-char-port-to-char-port in out))))))

(define bufsiz 4096)



;;(define (test)
;;
;;   ; binary copy gsi/gsi -> gsi2
;;
;;   (time (copy-binary-file "gsi/gsi" "gsi2"))
;;
;;   ; copy text file a.txt to b.txt, converting from UTF-8 character
;;   ; encoding (and any end-of-line encoding) to UCS-2 character
;;   ; encoding with BOM and DOS end-of-line encoding (compatible with
;;   ; Windows)
;;
;;   (time (copy-text-file
;;          (list path: "a.txt" char-encoding: 'utf8 eol-encoding: 'cr-lf)
;;          (list path: "b.txt" char-encoding: 'ucs2 eol-encoding: 'cr- 
;;lf))))

