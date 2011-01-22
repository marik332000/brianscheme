; Displays a 2D normal distribution using ncurses.
;
; Press q to quit. Try resizing your terminal once it's started!

(require 'random)
(require 'ncurses)

(with-curses win
  (let ((done #f))
    (while (not done)
      (let* ((w (nc:getmaxx win))
             (half-w (/ w 2.0))
             (h (nc:getmaxy win))
             (half-h (/ h 2.0)))
        (dotimes (i 250)
          (let ((x (+ (* (random:normal) (/ half-w 3.5)) half-w))
                (y (+ (* (random:normal) (/ half-h 3.5)) half-h)))
            (nc:mvprintw (floor y) (floor x) ".")))
        (if (= (nc:getch) (char->integer #\q))
            (set! done #t)
            (nc:clear))))))