(require 'ncurses)

(define *disc-win* (nc:initscr))
(define *width* (nc:getmaxx *disc-win*))
(define *height* (nc:getmaxy *disc-win*))

(define-class <map> ()
  "Game world map."
  (:width :height :grid :creatures))

(define-method (initialize (m <map>) args)
  "Create a new empty map."
  (slot-set! m :width (or (first args) 10))
  (slot-set! m :height (or (second args) 10))
  (let ((grid (make-vector (height m) #f)))
    (dotimes (y (height m))
      (vector-set! grid y (make-vector (width m) #f)))
    (slot-set! m :grid grid)
    (clear! m (lambda () (list :type 'rock)))
    (slot-set! m :creatures '())))

(define-generic clear!
  "Clear out the given object.")

(define-method (clear! (m <map>) valf)
  (dotimes (y (height m))
    (dotimes (x (width m))
      (pos! m x y (valf)))))

(define-generic width
  "Return width of object.")

(define-method (width (m <map>))
  (slot-ref m :width))

(define-generic height
  "Return height of object.")

(define-method (height (m <map>))
  (slot-ref m :height))

(define-generic pos
  "Get information on a position.")

(define-method (pos (m <map>) x y)
  (vector-ref (vector-ref (slot-ref m :grid) y) x))

(define-generic pos!
  "Set information on a position.")

(define-method (pos! (m <map>) x y val)
  (vector-set! (vector-ref (slot-ref m :grid) y) x val))

(define *space-types* '()
  "The types of things a space on the map can be.")

(define (define-space name char open doc)
  "Define a new type of map space."
  (set! *space-types* (assq-set! *space-types* name
				 (list :char char :open open :doc doc))))

(define-space 'floor #\. #t "Open floorspace.")
(define-space 'rock  #\  #f "Solid rock.")
(define-space 'hall  #\# #t "Open hallway.")

(define (char name)
  "Get character representation for map position."
  (plist-get (cdr (assq name *space-types*)) :char))

(define (open? name)
  "Return #t if space type is open."
  (plist-get (cdr (assq name *space-types*)) :open))

(define (pos-char m x y)
  "Get char for map position."
  (char (plist-get (pos m x y) :type)))

(define (pos-type m x y)
  "Type of space at position."
  (plist-get (pos m x y) :type))

(define (pos-open? m x y)
  "Return #t if position is open."
  (open? (plist-get (pos m x y) :type)))

(define (map-pos! m x y type)
  "Set type at map position."
  (plist-set! (pos m x y) :type type))

(define-generic draw-map
  "Draw a map to the ncurses display.")

(define-method (draw-map (m <map>))
  (nc:clear)
  (dotimes (y (height m))
    (nc:move y 0)
    (dotimes (x (width m))
      (nc:addch (pos-char m x y))))
  (nc:refresh))

(define *map* (make <map> *width* *height*))

(define (randn lst)
  "Return random element from the list."
  (list-ref lst (random (length lst))))

(define (gen-map m)
  "Fill the map with interesting things."
  (let ((rooms '()))
    (dotimes (i (+ 6 (random 6)))
      (set! rooms (add-room rooms))
      (fill-room m (car rooms))
      (draw-map m)
      (if (> (length rooms) 1)
	  (dotimes (i (+ 1 (random 4)))
	    (let ((a (car rooms))
		  (b (randn (cdr rooms))))
	      (add-path m (first a) (second a) (first b) (second b) #t)))))))

(define (gen-room)
  "Generate a new room within the display's dimensions."
  (let ((x (random *width*))
	(y (random *height*))
	(w (round (+ 1 (abs (* 4 (random:uniform))))))
	(h (round (+ 1 (abs (* 4 (random:uniform)))))))
    (if (or (>= (+ x w) *width*)
	    (< (- x w) 0)
	    (>= (+ y h) *height*)
	    (< (- y h) 0))
	(gen-room)
	(list x y w h))))

(define (overlap? a b)
  "Return #t if the two rooms overlap."
  (let ((x first) (y second) (w third) (h fourth))
    (not
     (or (or (and (< (x a) (x b))
		  (> (- (- (x b) (w b)) (+ (x a) (w a))) 1))
	     (and (> (x a) (x b))
		  (> (- (- (x a) (w a)) (+ (x b) (w b))) 1)))
	 (or (and (< (y a) (y b))
		  (> (- (- (y b) (h b)) (+ (y a) (h a))) 1))
	     (and (> (y a) (y b))
		  (> (- (- (y a) (h a)) (+ (y b) (h b))) 1)))))))

(define (add-room rooms)
  "Add a room that doesn't overlap existing rooms."
  (let ((room (gen-room)))
    (if (any? (curry overlap? room) rooms)
	(add-room rooms)
	(cons room rooms))))

(define (fill-room m room)
  "Insert the given room into the map grid."
  (dotimes (j (+ 1 (* 2 (fourth room))))
    (dotimes (i (+ 1 (* 2 (third room))))
      (map-pos! m (+ (- (first room)  (third room))  i)
		  (+ (- (second room) (fourth room)) j) 'floor))))

(define (add-path m x y gx gy start)
  "Create a hallway between (x, y) and (dx, dy)."
  (nc:mvaddch y x #\#)
  (nc:refresh)
  (if (not (and (= x gx) (= y gy)))
      (let ((open (pos-open? m x y)))
	(unless (and (not start) open)
		(if (not open)
		    (map-pos! m x y 'hall))
		(let ((dx (signum (- gx x)))
		      (dy (signum (- gy y))))
		  (cond
		   ((= x gx) (add-path m x (+ y dy) gx gy open))
		   ((= y gy) (add-path m (+ x dx) y gx gy open))
		   ((zero? (random 2)) (add-path m x (+ y dy) gx gy open))
		   (else (add-path m (+ x dx) y gx gy open))))))))

(define *debug* (open-output-port "debug"))
(define (db form)
  (write-port form *debug*)
  (write-char #\newline *debug*)
  (flush-output *debug*))

;; test

;(load "disc.sch")

;(define *map* (make <map> *width* *height*))
;(gen-map *map*)

;(draw-map *map*)
;(define *map* (make <map> (nc:getmaxx *disc-win*) (nc:getmaxy *disc-win*)))
;(map-pos! *map* 3 3 'floor)
;(pos *map* 3 3)

;(nc:refresh)
;(nc:clear)
