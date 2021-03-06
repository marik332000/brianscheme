;; now build up the clos environment and jump into the more
;; object aware repl
(require 'clos)

(define (write-port obj . port)
  (let ((stream (if port
		    (make <native-output-stream>
		      'port (car port))
		    stdout-stream)))
    (print-object stream obj)))

;; define a VM exception handler that uses our higher level condition
;; system
(set-error-restart!
 (lambda (ex)
   (raise (list 'vm-error ex))))

(define (clos-repl)
  (letrec ((repl-loop
	    (lambda ()
	      (display "bs> ")
	      (let* ((exp (read-port stdin))
		     (res (eval exp)))

		(when (eof-object? res)
		      (newline)
		      (exit 0))

		(print-object stdout-stream res)
		(newline)
		(repl-loop)))))

    (guard
     (ex (#t (printf "clos-repl: %a\n" ex)))

     (repl-loop)))

  (clos-repl))





