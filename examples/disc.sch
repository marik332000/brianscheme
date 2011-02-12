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

(define (pos-char m x y)
  "Get char for map position."
  (char (plist-get (pos m x y) :type)))

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

(define (gen-map m (nrooms 4))
  "Fill the map with interesting things."
  (let ((rooms '()))
    (dotimes (i nrooms)
      (set! rooms (add-room rooms))
      (fill-room m (car rooms)))))

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
     (and (not (= (x a) (x b)))
	  (not (= (y a) (y b)))
	  (or (and (< (x a) (x b))
		   (< (+ (x a) (w a)) (- (x b) (w b))))
	      (and (> (x a) (x b))
		   (> (- (x a) (w a)) (+ (x b) (w b)))))
	  (or (and (< (y a) (y b))
		   (< (+ (y a) (h a)) (- (y b) (h b))))
	      (and (> (y a) (y b))
		   (> (- (y a) (h a)) (+ (y b) (h b)))))))))

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

;; test

;(define r '())
;(set! r (add-room r))
;(gen-map *map*)

;(draw-map *map*)
;(define *map* (make <map> (nc:getmaxx *disc-win*) (nc:getmaxy *disc-win*)))
;(map-pos! *map* 3 3 'floor)
;(pos *map* 3 3)

;(nc:refresh)
;(nc:clear)
