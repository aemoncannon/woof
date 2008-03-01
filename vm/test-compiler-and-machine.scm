
(define run-compiler-and-machine-tests
  (lambda ()
    (run-tests
     (list
      (lambda ()
	"Test empty woof program."
	(let* ((code (compile-woof "")))
	  (assert-equal? code '((HALT)) 
			 msg: "Empty string should compile to a HALT.")
	  ))
      
      (lambda ()
	"Test eval a literal"
	(let* ((state (eval-woof-string (string-append "1"))))
	  (assert-true (woof-object? (tos state)) 
		       msg: "TOS should be a woof-object.")
	  (assert-equal? 1 (woof-integer-prim-value (tos state))
			 msg: "TOS should be integer = to 1.")
	  ))
      
      (lambda ()
	"Test eval an assignment, and then a reference."
	(let* ((state (eval-woof-string (string-append "a := 5."
						       "a"
						       ))))
	  (assert-true (woof-object? (tos state)) 
		       msg: "TOS should be a woof-object.")
	  (assert-equal? 5 (woof-integer-prim-value (tos state))
			 msg: "TOS should be integer = to 5."
			 )
	  ))
      
      (lambda ()
	"Test eval assignment of var to var, and then a reference."
	(let* ((state (eval-woof-string (string-append "a := 5."
						       "b := a."
						       "b"
						       ))))
	  (assert-true (woof-object? (tos state))
		       msg: "TOS should be a woof-object.")
	  (assert-equal? 5 (woof-integer-prim-value (tos state))
			 msg: "TOS should be an integer = to 5.")
	  ))
      
      (lambda ()
	"Test eval of block."
	(let* ((state (eval-woof-string (string-append "[||  ]"
						       ))))
	  (assert-true (woof-block? (tos state))
		       msg: "TOS should be a woof-block.")
	  ))
      
      (lambda ()
	"Test eval of block with parameters."
	(let* ((state (eval-woof-string (string-append "[|a,b,c,d|  ]"
						       ))))
	  (assert-true (woof-block? (tos state))
		       msg: "TOS should be a woof-block."
		       )
	  ))
      
      (lambda ()
	"Test call block with parameters."
	(let* ((state (eval-woof-string (string-append "a := 5."
						       "b := 2."
						       "@(call_block [|x| x], #(a))"
						       ))))
	  (assert-true (woof-object? (tos state))
		       msg: "TOS should be a woof-object.")
	  (assert-equal? 5 (woof-integer-prim-value (tos state))
			 msg: "TOS should be an integer = to 5."
			 )
	  ))
      
      (lambda ()
	"Test call block with no parameters."
	(let* ((state (eval-woof-string (string-append "a := 4."
						       "b := 2."
						       "@(call_block [|| a],#())"
						       ))))
	  (assert-true (woof-object? (tos state))
		       msg: "TOS should be a woof-object.")
	  (assert-equal? 4 (woof-integer-prim-value (tos state))
			 msg: "TOS should be an integer = to 4.")
	  ))
      
      (lambda ()
	"Test block closure."
	(let* ((state (eval-woof-string (string-append "c := @(call_block [|| q := 1."
						       "[|| q ]], #())."
						       "@(call_block c, #())"
						       ))))
	  (assert-true (woof-object? (tos state))
		       msg: "TOS should be a woof-object.")
	  (assert-equal? 1 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test block closure generator."
	(let* ((state (eval-woof-string (string-append "gen := [|q| [|| q ]]."
						       "f := @(call_block gen, #(4))."
						       "@(call_block f, #())"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 4 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test require function."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "do: [|i| i] with: #(7)"
						       )
					)))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 7 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test new_instance primitive."
	(let* ((state (eval-woof-string (string-append "@(new_instance Object)"))))
	  (assert-true (woof-object? (tos state)))
	  ))
      
      (lambda ()
	"Test new: function."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "a := new: Object"
						       )
					)))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (class-by-name state "Object")(woof-object-class (tos state)))
	  ))
      
      (lambda ()
	"Test create_class primitive."
	(let* (
	       (state (eval-woof-string (string-append "@(create_class 'Moose', Object, #(), #())"))))
	  (assert-true (woof-class? (tos state))
		       msg: "TOS should be a woof-class.")
	  ))
      
      (lambda ()
	"Test 'class:super:vars:classVars:' function."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "class: 'Moose' super: Object vars: #() classVars: #()."
						       "a := new: Moose"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (class-by-name state "Moose")(woof-object-class (tos state)))
	  ))
      
      (lambda ()
	"Test show_number primitive."
	(let* ((state (eval-woof-string (string-append "@(show_number 5)"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-string-prim-value (tos state)) "5")
	  ))
      
      (lambda ()
	"Test show_object primitive."
	(let* ((state (eval-woof-string (string-append "@(show_object [||])"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-string-prim-value (tos state)) "<instance of Block>")
	  ))
      
      (lambda ()
	"Test list_ref primitive."
	(let* ((state (eval-woof-string (string-append "list := #(1,2,3,4)."
						       "@(list_ref list, 2)"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-integer-prim-value (tos state)) 3)
	  ))
      
      (lambda ()
	"Test list_ref primitive out of bounds."
	(let* ((state (eval-woof-string (string-append "list := #(1,2,3,4)."
						       "@(list_ref list, 20)"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (tos state) (woof-nil state))
	  ))
      
      (lambda ()
	"Test obj_eq primitive."
	(let* ((state (eval-woof-string (string-append "jack := 5."
						       "jill := jack."
						       "@(obj_eq jack,jill)"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-object-class (tos state)) (class-by-name state "True"))
	  ))
      
      (lambda ()
	"Test nil singleton"
	(let* ((state (eval-woof-string (string-append "@(obj_eq nil,nil)"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (tos state) (woof-true state))
	  ))
      
      (lambda ()
	"Test equality operator."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "((false == false) == true)"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (tos state) (woof-true state))
	  ))
      
      (lambda ()
	"Test inequality operator."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "((false != true) == true)"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (tos state) (woof-true state))
	  ))
      
      (lambda ()
	"Addition primitive."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "1 + 2"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 3 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test forEachOf:do:"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "sum := 0."
						       "forEachOf: #(1,1,1,1,1) do:"
						       "[|ea| sum := sum + 1]."
						       "sum"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 5 (woof-integer-prim-value (tos state)))
	  ))
      
      
      (lambda ()
	"Test set/get_instance_var primitive."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "class: 'Moose' super: Object vars: #('feet') classVars: #()."
						       "o := new: Moose."
						       "@(set_instance_var o, 'feet', 4)."
						       "@(get_instance_var o, 'feet')"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-integer-prim-value (tos state)) 4)
	  ))
      
      (lambda ()
	"Test get_instance_var, unset var is nil."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "class: 'Moose' super: Object vars: #('feet') classVars: #()."
						       "o := new: Moose."
						       "@(get_instance_var o, 'feet')"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (tos state) (woof-nil state))
	  ))
      
      (lambda ()
	"Test string_concat primative"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "@(string_concat 'cat', 'dog')"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-string-prim-value (tos state)) "catdog")
	  ))
      
      (lambda ()
	"Test list_concat primative"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "@(list_concat #(1), #(2,3))"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (length (woof-list-prim-value (tos state))) 3)
	  ))
      
      (lambda ()
	"Test instance var accessors."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "class: 'Moose' super: Object vars: #('feet') classVars: #()."
						       "o := new: Moose."
						       "feetOf: o is: 4."
						       "feetOf: o"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-integer-prim-value (tos state)) 4)
	  ))
      
      (lambda ()
	"Test Maybe on value."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "m := new: Maybe on: 1."
						       "if: m do: [|val| val] else: [||2]"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-integer-prim-value (tos state)) 1)
	  ))
      
      (lambda ()
	"Test Maybe on nil."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "m := new: Maybe on: nil."
						       "if: m do: [|val| val] else: [||2]"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-integer-prim-value (tos state)) 2)
	  ))
      
      (lambda ()
	"Test list_length primative"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "@(list_length #(1,2,3))"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-integer-prim-value (tos state)) 3)
	  ))
      
      
      (lambda ()
	"Test string_length primative"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "@(string_length 'ape')"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? (woof-integer-prim-value (tos state)) 3)
	  ))
      
      (lambda ()
	"Test assignment to global, then lookup of global."
	(let* ((state (eval-woof-string (string-append "$a := 5."
						       "$a"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 5 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test forEachWithIndexOf:do:"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "sum := 0."
						       "forEachWithIndexOf: "
						       "#(1,1,1,1,1) do:"
						       "[|ea,i| sum := sum + i]."
						       "sum"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 10 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test selectFrom:where:"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "length: (selectFrom: "
						       "#(1,2,3,4,5) where:"
						       "[|ea| ea > 2])"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 3 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test selectWithIndexFrom:where:"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "length: (selectWithIndexFrom: "
						       "#(1,2,3,4,5) where:"
						       "[|ea,i| i > 2])"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 2 (woof-integer-prim-value (tos state)))
	  ))
      
      
      (lambda ()
	"Test take:from:"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "length: (take: 2 from: #(1,2,3,4,5,6))"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 2 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test drop:from:"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "length: (drop: 3 from: #(1,2,3,4,5,6))"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 3 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test fold:over:with:"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "fold: [|ea,sum| ea + sum] over: #(1,2,3)"
						       "with: 0"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 6 (woof-integer-prim-value (tos state)))
	  ))
      
      
      (lambda ()
	"Test withCC:"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "withCC: [|cc| cc]"
						       ))))
	  (assert-true (woof-continuation? (tos state)))
	  ))
      
      (lambda ()
	"Test withCC: and do:with: within static extent."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "cont := withCC: [|cc| do: cc with: 5. 10]"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 5 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test withCC: and do:with: with dynamic extent."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "cont := nil."
						       "i := withCC: [|cc| cont := cc. 2 ]."
						       "if: (i < 5)"
						       "do: [ do: cont with: 10 ]"
						       "else: [i]"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 10 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test withCC: and do:with:, returning multiple times."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "cont := nil."
						       "i := 0."
						       "i := withCC: [|cc| cont := cc. i]."
						       "if: (i < 20)"
						       "do: [ do: cont with: (i + 1) ]"
						       "else: [i]"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 20 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test try:catch: without an error."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "try: [ 1 + 2] catch: [|e| ]"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 3 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test try:catch: WITH an error."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "try: [ 1 + 2. raise: (new: Error)]"
						       "catch: [|e| 5]"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 5 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test machine raises an error, and showing that error."
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "try: [ 1 + nil ]"
						       "catch: [|e| show: e]"
						       ))))
	  (assert-true (woof-string? (tos state)) 
		       msg: "Error should be shown as string.")
	  ))
      
      (lambda ()
	"Test nested try:catch"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "a := 0."
						       "try: [ a := a + 1. try: [ raise: (new: Error)]"
						       "catch: [ a := a + 1. raise: (new: Error)]]"
						       "catch: [|e| a]"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? 2 (woof-integer-prim-value (tos state)))
	  ))
      
      (lambda ()
	"Test load block from file."
	(let* ((state (eval-woof-string (string-append "@(load_block_from_file 'prelude.woof')"))))
	  (assert-true (woof-block? (tos state)))
	  ))
      
      (lambda ()
	"Test error"
	(let* ((state (eval-woof-string (string-append "require: 'prelude.woof'."
						       "e := new: Error withMsg: 'hello'."
						       "messageOf: e"
						       ))))
	  (assert-true (woof-object? (tos state)))
	  (assert-equal? "hello" (woof-string-prim-value (tos state)))
	  ))
      
      ))
))