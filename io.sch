;;; Input/Output functions and macros

(define (open-output-port file)
  (assert-types (file string?))
  (%open-output-port file))

(define (open-input-port file))
  (assert-types (file string?))
  (%open-input-port file))

(define (close-output-port port))
  (assert-types (port output-port?))
  (%close-output-port port))

(define (close-input-port port))
  (assert-types (port input-port?))
  (%close-input-port port))

(define (write-port obj port))
  (assert-types (port output-port?))
  (%write-port obj port))

(define (read-port port))
  (assert-types (port input-port?))
  (%read-port port))

(define (read-char port))
  (assert-types (port input-port?))
  (%read-char port))

(define (write-char ch port))
  (assert-types (ch char?) (port output-port?))
  (%write-char ch port))

(define (unread-char ch port))
  (assert-types (ch char?) (port input port?))
  (%unread-char ch port))

(define-syntax (with-open-file binding . body)
  (let ((var (first binding))
        (file (second binding))
        (res (gensym)))
    `(let ((,var (open-input-port ,file)))
       (if (eof-object? ,var)
           (throw-error "failed to open" ,file)
           (let ((,res (begin ,@body)))
             (close-input-port ,var)
             ,res)))))
