(require 'math)

(define (string-append . args)
  "join a series of strings"
  (reduce prim-concat args))

(define (string-length str)
  "find the length of a string, not including null"
  (let ((len 0))
    (while (not (= (char->integer (string-ref str len))
		   0))
      (inc! len))
    len))

(define (substring str start . end)
  "Return given substring from start (inclusive) to end (exclusive)."
  (let* ((strlen (string-length str))
	 (end (car-else end strlen))
	 (len (- end start)))
    (if (or (> len strlen) (> end strlen) (> start strlen))
	(throw-error "out of string bounds" str start end))
    (if (> start end)
	(throw-error "invalid substring" start end))
    (let ((substr (make-string len #\.)))
      (dotimes (i len)
        (string-set! substr i (string-ref str (+ i start))))
      substr)))

(define (string->list str)
  "Turn a string into a character list."
  (if (= 0 (string-length str))
      '()
      (cons (string-ref str 0) (string->list (substring str 1)))))

(define (string=? . args)
  "Return #t if all strings arguments are equal?."
  (every-pair? equal? args))

(define (char->string char)
  "Return a string containing only char."
  (make-string 1 char))

;; String number processing

(define *digits* (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (digit? ch)
  "Return #t if character is a digit."
  (member? ch *digits*))

(define (integer-string-list? lst)
  "Return #t if string-list contains an integer."
  (and (every? digit? (cdr lst))
       (or (and (or (eq? (car lst) #\-)
                    (eq? (car lst) #\+))
                (not (null? (cdr lst))))
           (digit? (car lst)))))

(define (integer-string? string)
  "Return #t if string contains an integer."
  (integer-string-list (string->list string)))

(define (string-list->integer lst)
  "Convert the integer in the string-list to an integer."
  (letrec ((iter (lambda (number lst)
                   (if (null? lst)
                       number
                       (iter (+ (* 10 number)
                                (index-of (curry eq? (car lst)) *digits*))
                             (cdr lst))))))
    (cond
     ((eq? (car lst) #\-) (- (iter 0 (cdr lst))))
     ((eq? (car lst) #\+) (iter 0 (cdr lst)))
     (#t (iter 0 lst)))))

(define (string->integer string)
  "Convert the integer in the string to an integer."
  (let ((lst (string->list string)))
    (if (not (integer-string-list? lst))
        (error "invalid integer in string" string)
        (string-list->integer lst))))
