(define (manipulate-loop func port in)
  (when (not (eq? *eof-object* in))
    (func in)
    (manipulate-loop func port (read port))))

(define-syntax (manipulate expr var)
  `(let ((*tcl* (open-input-pipe "tclsh examples/slider.tcl")))
     (manipulate-loop (lambda (,var) ,expr) *tcl* (read *tcl*))))

;; Example usage:

(require 'plot)
(set! data (map (lambda (x) (/ x 5.0)) (upto 100)))

;; Manipulate the plot over x
(manipulate
 (plot:list (map (lambda (n) (sin (* x n))) data))
 x)
