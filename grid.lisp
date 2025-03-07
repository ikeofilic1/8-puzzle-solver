(ql:quickload :sketch)
(use-package :sketch)

;; CONSTANTS
(defconstant +window-width+ 1000)
(defconstant +window-height+ 800)
(defconstant +grid-width+ 600)
(defconstant +grid-height+ 600)

(defsketch grid 
  ((width +window-width+)
  (height +window-height+)
  (labels (("1" "2" "3") ("4" "5" "6") ("7" "8" ""))))

  )

(defmethod move-tile ((grid-obj grid) start-pos end-pos)
  )