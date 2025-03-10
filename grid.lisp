(ql:quickload "sketch")

(defpackage :grid-example
  (:use :cl :sketch))

(in-package :grid-example)

(defparameter *grid-width* 5)
(defparameter *grid-height* 5)
(defparameter *cell-size* 50)

(defun draw-grid (x y)
  (dotimes (row *grid-height*)
    (dotimes (col *grid-width*)
      (let ((xi (+ x (* col *cell-size*)))
            (yi (+ y (* row *cell-size*)))
            (number (+ (* row *grid-width*) col))) ; Calculate the number
        (with-pen (make-pen :fill (gray 0.9) :stroke +black+) ; Light gray cell, black border
          (rect xi yi *cell-size* *cell-size*)) 
        (with-font (make-font :color +black+ :align :center) ; Black text
          (text (write-to-string number) 
            (+ xi (/ *cell-size* 2)) 
            (- (+ yi (/ *cell-size* 2)) (/ 20 2))))))))

(defsketch grid-sketch 
    ((width 800)
     (height 500)
     (background +white+)) ; White background
  (draw-grid 100 100)
  )

; (line-height (font-line-height (env-font *env*)))

(make-instance 'grid-sketch)