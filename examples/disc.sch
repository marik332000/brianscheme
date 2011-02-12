(require 'ncurses)

(define *disc-win* (nc:initscr))

(define *map* (make <map> (nc:getmaxx *disc-win*) (nc:getmaxy *disc-win*)))

(define-class <map> ()
  "Game world map."
  ('width 'height 'grid 'creatures))

(define-method (initialize (m <map>) args)
  "Create a new empty map."
  (slot-set! m 'width (or (first args) 10))
  (slot-set! m 'height (or (second args) 10))
  (let ((grid (make-vector (height m) #f)))
    (dotimes (y (height m))
      (vector-set! grid y (make-vector (width m) #f)))
    (slot-set! m 'grid grid)
    (clear! m (lambda () (list 'type 'rock)))
    (slot-set! m 'creatures '())))

(define-generic clear!
  "Clear out the given object.")

(define-method (clear! (m <map>) valf)
  (dotimes (y (height m))
    (dotimes (x (width m))
      (pos! m x y (valf)))))

(define-generic width
  "Return width of object.")

(define-method (width (m <map>))
  (slot-ref m 'width))

(define-generic height
  "Return height of object.")

(define-method (height (m <map>))
  (slot-ref m 'height))

(define-generic pos
  "Get information on a position.")

(define-method (pos (m <map>) x y)
  (vector-ref (vector-ref (slot-ref m 'grid) y) x))

(define-generic pos!
  "Set information on a position.")

(define-method (pos! (m <map>) x y val)
  (vector-set! (vector-ref (slot-ref m 'grid) y) x val))

(define *space-types* '()
  "The types of things a space on the map can be.")

(define (define-space name char open doc)
  "Define a new type of map space."
  (set! *space-types* (assq-set! *space-types* name
				 (list 'char char 'open open 'doc doc))))

(define-space 'floor #\. #t "Open floorspace.")
(define-space 'rock  #\  #f "Solid rock.")
(define-space 'hall  #\# #t "Open hallway.")

(define (char name)
  "Get character representation for map position."
  (plist-get (cdr (assq name *space-types*)) 'char))

(define (pos-char m x y)
  "Get char for map position."
  (char (plist-get (pos m x y) 'type)))

(define (map-pos! m x y type)
  "Set type at map position."
  (plist-set! (pos m x y) 'type type))

(define-generic draw-map
  "Draw a map to the ncurses display.")

(define-method (draw-map (m <map>))
  (nc:clear)
  (dotimes (y (height m))
    (nc:move y 0)
    (dotimes (x (width m))
      (nc:addch (pos-char m x y))))
  (nc:refresh))

(define (add-rooms m . n)
  "Add rooms to a map."
  (dotimes (i (car-else n 8))
    (add-room m (random (- (width m) 10))
	      (random (- (height m) 10))
	      (+ 4 (random 7))
	      (+ 4 (random 7)))))

(define (add-room m x y w h)
  "Add a room at given position of given size."
  (dotimes (j h)
    (dotimes (i w)
      (map-pos! m (+ x i) (+ h j) 'floor))))

;; test

(add-rooms *map*)

(draw-map *map*)
(define *map* (make <map> (nc:getmaxx *disc-win*) (nc:getmaxy *disc-win*)))
(map-pos! *map* 3 3 'floor)
(pos *map* 3 3)

(nc:refresh)
(nc:clear)
