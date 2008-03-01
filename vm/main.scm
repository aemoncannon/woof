(define debug-mode #f)
(define bytecode-mode #f)

(setenv "WOOF_COMPILER" "c:/my_system/src/misc/woof/trunk/woofc.exe")
(setenv "WOOF_MODULES" "c:/my_system/src/misc/woof/trunk/kernel")

(if (> (length (command-line)) 1)
    (let* ((filename (resolve-woof-file (second (command-line)))))
      (if (includes? "-d" (command-line))
	  (set! debug-mode #t))
      (if (includes? "-b" (command-line))
	  (set! bytecode-mode #t))
      (if (not bytecode-mode)
	  (eval-woof-from-filename-with-prelude filename debug: debug-mode)
	  (eval-woof-bytecode-from-filename filename debug: debug-mode))))


