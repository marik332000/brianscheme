(load "compiler.sch")

(define (write-bytecode fn file)
  (let ((out (open-output-port file)))
    (write-port out fn)
    (close-output-port out)))

(define (assert pred message)
  (if (not pred)
      (throw-error message)
      #t))

(define-syntax vm-test (expr)
  `(let ((exp ',expr)
	 (comp nil)
	 (evres nil)
	 (vmres nil))

     (write 'expression exp)
     (set! comp (compiler exp))

     (write 'bytecode (compiled-bytecode comp))
     (set! evres (eval exp))

     (write 'eval-result evres)
     (set! vmres (vm-execute comp))

     (write 'vm-result vmres)
     (assert (equal? evres vmres))
     
     comp))

(macroexpand0 '(vm-test (begin (+ 1 (+ 2 (+ 3 4))))))

(vm-test (begin (+ 1 (+ 2 (+ 3 4)))))
(vm-test ((lambda (x) (cons 1 x)) 5))

(vm-test
 (((lambda (x) (if x
		   (lambda (y) (cons x y))
		   (lambda (y) (+ y y)))) #t) 5))
				      

(set! f (compiler '(lambda (x) (+ x 1))))

(set! f2 ((compiler '(lambda (x y) (cons x (+ x y))))))
