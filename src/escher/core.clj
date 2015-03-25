(ns escher.core
  (require [quil.core :as q]
           [clj-tuple :as t])
  (:gen-class))

;;==================================================================
;;
;;  vector-manipulation primitives
;;
;;==================================================================

(defn make-vec [x y]
  (t/vector x y))

(defn add-vec [[x1 y1] [x2 y2]]
  (make-vec (+ x1 x2) (+ y1 y2)))

(defn sub-vec [[x1 y1] [x2 y2]]
  (make-vec (- x1 x2) (- y1 y2)))

(defn scale-vec [[x y] s]
  (make-vec (* x s) (* y s)))



;;==================================================================
;;
;;  What is a frame?
;;
;;  A frame represents a transformation from one coordinate system
;;  to another. The transformation described by:
;;
;;     {:origin [n1 n2], :e1 [n3 n4], :e2 [n5 n6]}
;;
;;  can express combinations of translation, scaling, shearing,
;;  and reflection.
;;
;;==================================================================



(defn frame-coord-map
  "Returns function that maps [x y] relative to the frame
  defined by origin (a vector to be interpreted as the point
  describing the origin) and vectors e1 (x-axis), e2 (y-axis)--
  see SICP, 2nd ed., page 183.

  In this function, the frame describes the portion of the canvas
  within which any untransformed picture will draw itself."
  [{:keys [origin e1 e2]}]
  (fn [[x y]]
    (add-vec origin
             (add-vec (scale-vec e1 x)
                      (scale-vec e2 y)))))



;;==================================================================
;;
;;  What is a picture?
;;
;;  A picture is a function that, when given a frame, knows how to
;;  draw itself in that frame. As such, it hides its implementation
;;  details.
;;
;;  This file defines two functions that draw pictures:
;;
;;      segment-painter: creates a picture from line segments
;;      image-painter: creates a picture from a GIF image
;;
;;==================================================================



;;==================================================================
;;
;;  Utilities for drawing simple graphics using line segments
;;
;;==================================================================

(defn make-segment
  "Represent a line segment as a vector of two vectors--e.g.,
  [[0 0] [1 0]] represents a line from [0 0] to [1 0]."
  [vec1 vec2]
  (make-vec vec1 vec2))


(defn path
  "Creates a seq of line-segments from a 'bare' list of points. Use to
  draw a continuous line through the list of points--e.g.,

  (path [[0 1] [0 0] [1 0]])

  returns two line segments connecting the three points in sequence:

  [ [[0 1] [0 0]] [[0 0] [1 0]] ]"
  [& veclist]
  ; COMPLETE
  )

(def draw-line q/line)



;;==================================================================
;;
;;  for images drawn with line segments, this function returns
;;  the picture (i.e., a function) that draws the desired image
;;
;;==================================================================


(defn frame-painter [{:keys [origin e1 e2]}]
  "Draws a parallelogram 'frame' based on origin and
  vectors e1 and e2. Must execute within a Quil sketch.

  (Used only to confirm minimal project functionality: in draw-image,
  uncomment '(frame-painter frame1)' and evaluate the code
  in this file.)"
  (let [corner (add-vec origin (add-vec e1 e2))]
    (draw-line origin (add-vec origin e1))
    (draw-line origin (add-vec origin e2))
    (draw-line (add-vec origin e2) corner)
    (draw-line (add-vec origin e1) corner)))

(defn segment-painter
  "Returns a picture that draws the given list of line segments.
  Must execute within a Quil sketch."
  [segment-list]
  ; xform-pt is a function that maps a point to the given frame
  (fn [frame]
    (let [xform-pt (frame-coord-map frame)]
      (doseq [[start end] segment-list]
        (draw-line (xform-pt start) (xform-pt end))))))

(defn transform-picture
  "Returns a transformed picture based on the values of origin,
  e1, and e2, which together describe a transformation frame.

  Note that the returned picture will draw itself inside a
  different frame (frame2), which can be considered to be a
  drawing frame. The coordinates of origin, e1, and e2 are
  usually within the range [0 1]--i.e.,a unit square with
  origin [0 0]. Coordinates > 1 will draw outside the drawing
  frame."
  [p origin e1 e2]
  (fn [frame]
    (let [unit-sq-xform (frame-coord-map frame)
          new-origin (unit-sq-xform origin)]
      (p ({:origin new-origin
           :e1 (sub-vec (unit-sq-xform e1) new-origin)
           :e2 (sub-vec (unit-sq-xform e2) new-origin)})))))



;;==================================================================
;;
;;  basic transformations of a single picture
;;
;;==================================================================

(defn flip-vert [p]
  (transform-picture p (make-vec 0 1) (make-vec 1 1) (make-vec 0 0)))

(defn flip-horiz [p]
  ;; COMPLETE (Ex 2.50)
  )

(defn rotate [p]
  ;; COMPLETE
  )

(defn rotate180 [p]
  (rotate (rotate p)))

(defn rotate270 [p]
  (rotate (rotate (rotate p))))



;;==================================================================
;;
;;  transformations that combine 2 or 4 pictures into a new picture
;;
;;==================================================================

(defn beside
  "Returns a picture that, splitting its frame in half vertically, draws
  p1 in the left half and p2 in the right half."
  [p1 p2]
  (let [split (make-vec 0.5 0)
        left (transform-picture p1 (make-vec 0 0) split (make-vec 0 1))
        right (transform-picture p2 split (make-vec 1 0) (make-vec 0.5 1))]
    (fn [frame]
      (left frame)
      (right frame))))

(defn below
  "Returns a picture that, splitting its frame in half horizintally,
  draws p1 in the top half and p2 in the bottom half."
  [p1 p2]
  ; COMPLETE (Ex 2.51)
  )

(defn quartet
  "Returns a picture that subdivides its frame into quarters and
  draws p1-p4 in order, left-to-right and top-to-bottom."
  [p1 p2 p3 p4]
  (below (beside p1 p2)
         (beside p3 p4)))



;;==================================================================
;;
;;  building-blocks for the "goal" function, square-limit
;;
;;==================================================================

(defn right-split [p n]
  (if (= n 0)
    p
    (let [smaller (right-split p (dec n))]
      (beside p (below smaller smaller)))))

(defn up-split
  "Returns a picture based on picture p and level-of-recursion n.
  Draws p in the top half of the frame. Draws two copies of
  (right-split p (- n 1)), one to the right of the other, in the
  bottom half of the frame. If n=0, draws p in the current frame."
  [p n]
  ;; COMPLETE (Ex 2.44)
  )


(defn split [f g]
  "Higher-order function that allows the functions right-split
  and left-split to be defined without duplicated code. (Hint:
  define each function independently, then analyze the resulting
  code to enable each function's redefinition in terms of the
  two picture transformations f and g.)

  Should be able to do
    (def right-split (split beside below))
    (def up-split (split below beside)
  and replace the existing *-split fns"
  ; COMPLETE (Ex 2.45)
  )

(defn corner-split
  "Returns a picture based on picture p and level-of-recursion n.
  Draws p in the upper-left quarter of frame. Draws two copies
  of (right-split p (- n 1)) in upper-right quarter of frame.
  Draws two copies of (top-split p (- n 1)) in the bottom-left
  quarter of frame. Draws (corner-split p (- n 1)) in the bottom-
  right quarter of frame. Wherever n=0, draws p in current frame."
  [p n]
  (if (= n 0)
    p
    (let [up (up-split p (dec n))
          right (right-split p (dec n))
          top-left (beside up up)
          bottom-right (below right right)
          corner (corner-split p (dec n))]
      (beside (below p top-left)
              (below bottom-right corner)))))

(defn square-of-four
  "Returns a picture that subdivides its frame into quarters and
  transforms the same picture p in four different ways, then
  draws the results in order, left-to-right and top-to-bottom."
  [tl tr
   bl br]
  (fn [p]
    (let [top (beside (tl p) (tr p))
          bottom (beside (bl p) (br p))]
      (below top
             bottom))))

(def combine-four (square-of-four flip-horiz
                                  identity
                                  rotate180
                                  flip-vert))

(defn square-limit
  "Returns a picture that has a 2 x 2 block of p in the center, encircled
  by n borders of picture p, with each successive version of p being
  half the size of the one that it immediately surrounds."
  [p n]
  (combine-four (corner-split p n)))



;;==================================================================
;;==================================================================
;;
;;  Quil setup
;;
;;==================================================================
;;==================================================================

;; In Quil, [0 0] is upper left corner of window.
;; Increasing x values move a point to the right.
;; Increasing y values move a point downward.


;; defines size of Quil window

(def width 600)
(def height 600)


;; origin, e1, and e2 (all 2-D vectors) define a frame.
;; Think of origin as a point, e1 as the x-axis, e2 as the y-axis.

(def whole-window {:origin (make-vec 0 0)
                   :e1 (make-vec width 0)
                   :e2 (make-vec 0 height)})

(def frame1 {:origin (make-vec 200 50)
             :e1 (make-vec 200 100)
             :e2 (make-vec 150 200)})

(def frame2 {:origin (make-vec 50 50)
             :e1 (make-vec 100 0)
             :e2 (make-vec 0 200)})


(defn draw
  "Draws picture, using the entire Quil window."
  [picture]
  (picture whole-window))



;;==================================================================
;;==================================================================
;;
;;  PICTURES THAT ARE DRAWN USING SEGMENT-PAINTER
;;
;;==================================================================
;;==================================================================


;;==================================================================
;;
;;  box: a picture that draws a square
;;
;;==================================================================


(def box
  ; COMPLETE
  )



;;==================================================================
;;
;;  x: a picture that draws an "x" (as if from the vertices of 'box')
;;
;;==================================================================

(def x
  ; COMPLETE
  )



;;==================================================================
;;
;;  diamond: a picture that draws a square with vertices at each
;;  midpoint of the left, top, right, and bottom faces of the frame
;;
;;==================================================================

(def diamond
  ; COMPLETE
  )



;;==================================================================
;;
;; george: a "man" figure using line segments
;;
;;==================================================================

(def p1 (make-vec 0 0.35))
(def p2 (make-vec 0.15 0.6))
(def p3 (make-vec 0.3 0.4))
(def p4 (make-vec 0.35 0.5))
(def p5 (make-vec 0.25 1.0))
(def p6 (make-vec 0.4 1.0))
(def p7 (make-vec 0.5 0.7))
(def p8 (make-vec 0.6 1.0))
(def p9 (make-vec 0.75 1.0))
(def p10 (make-vec 0.6 0.55))
(def p11 (make-vec 1 0.85))
(def p12 (make-vec 1 0.65))
(def p13 (make-vec 0.75 0.35))
(def p14 (make-vec 0.6 0.35))
(def p15 (make-vec 0.65 0.15))
(def p16 (make-vec 0.6 0))
(def p17 (make-vec 0.4 0))
(def p18 (make-vec 0.35 0.15))
(def p19 (make-vec 0.4 0.35))
(def p20 (make-vec 0.3 0.35))
(def p21 (make-vec 0.15 0.4))
(def p22 (make-vec 0.0 0.15))

(def george-segs
  (concat
   (path p1  p2  p3  p4  p5)
   (path p6  p7  p8)
   (path p9 p10 p11)
   (path p12 p13 p14 p15 p16)
   (path p17 p18 p19 p20 p21 p22)
   ))

(def george
  ; COMPLETE
  )




(def diag (segment-painter [[(make-vec 0 0) (make-vec 1 1)]]))




;;==================================================================
;;==================================================================
;;
;;  for images drawn with GIFs and JPGs, this function returns
;;  the picture (i.e., a function) that draws the desired image
;;
;;  (This function needs to be completed. People have found
;;  the following URLs helpful:
;;
;;    http://quil.info/api/transform
;;    http://homepages.inf.ed.ac.uk/rbf/HIPR2/affine.htm
;;    http://www.cs.colorado.edu/~mcbryan/5229.03/mail/55.htm )
;;
;;==================================================================
;;==================================================================

(defn image-painter [img]
  (fn [{[ox oy] :origin
        [e1x e1y] :e1
        [e2x e2y] :e2
        }]
    (let [width (.width img)
          height (.height img)]
      ; COMPLETE
      )))



(defn draw-image []
  (let [man (image-painter (q/load-image "data/man.gif"))
        bruce (image-painter (q/load-image "data/bruce.jpg"))
        angels (image-painter (q/load-image "data/angels.jpg"))]
    (q/background 255)
    ;; (frame-painter frame1)
    ;; (draw x)
    ;; (draw box)
    ;; (george frame2)
    ;; (draw (rotate george))
    ;; (draw (flip-horiz george))
    ;; (draw (beside box box))
    ;; (draw (combine-four george))
    ;; (draw (beside (below george george)
    ;;               (flip-horiz (below george george))))
    ;; (draw (below (beside george (flip-horiz george))
    ;;              (beside george (flip-horiz george))))

    ;; (draw ((square-of-four identity flip-vert
    ;;                        flip-horiz rotate)
    ;;        george))

    ; Needs image-painter
    ;; (bruce frame1)
    ;; (bruce frame2)
    ;; (draw (beside george bruce))
    ;; (draw (corner-split bruce 4))
    ;; (draw (square-limit bruce 3))
    ;; (draw (beside  bruce (below  bruce
    ;;                              george)))
    ))



;;==================================================================
;;
;;  this is the code that sets the size of the Quil window and
;;  causes draw-pictures to be executed
;;
;;==================================================================

(q/defsketch escher
  :title "Escher"
  :draw draw-image
  :size [width height])

(defn -main [])
