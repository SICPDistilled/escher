(ns escher.core
  (require [quil.core :as q])
  (:gen-class))

;; In Quil, [0 0] is upper left corner of window.
;; Increasing x values move a point to the right.
;; Increasing y values move a point downward.


;; defines size of Quil window
(def width 600)
(def height 600)

(def draw-line q/line)

;; origin, e1, and e2 (all 2-D vectors) define a frame.
;; Think of e1 as the x-axis, e2 as the y-axis.

(def whole-window {:origin [0 0]
                   :e1 [width 0]
                   :e2 [0 height]})

(def frame1 {:origin [200 50]
             :e1 [200 100]
             :e2 [150 200]})

(def frame2 {:origin [50 50]
             :e1 [100 0]
             :e2 [0 200]})


(defn make-vec [x y]
  [x y])

(defn make-segment [vec1 vec2]
  [vec1 vec2])

(defn add-vec [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)]
 )


(defn sub-vec [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)]
  )

(defn scale-vec [[x y] s]
  [(* x s) (* y s)]
  )


(defn frame-coord-map
  "Returns function that maps [x y] relative to the frame
  defined by origin (a vector to be interpreted as the point
  describing the origin) and vectors e1 (x-axis), e2 (y-axis).
   See SICP, 2nd ed., page 183."
  [{:keys [origin e1 e2]}]
  (fn [[x y]]
    (add-vec origin
             (add-vec (scale-vec e1 x)
                      (scale-vec e2 y)))))

(defn frame-painter [{:keys [origin e1 e2]}]
  "Draws parallelogram 'frame' based on origin and vectors e1 and e2.
  Must execute within a Quil sketch."
  (let [corner (add-vec origin (add-vec e1 e2))]
    (draw-line origin (add-vec origin e1))
    (draw-line origin (add-vec origin e2))
    (draw-line (add-vec origin e2) corner)
    (draw-line (add-vec origin e1) corner)))

(defn segment-painter
  "Returns a function that, when given a frame as its argument, draws
  the list of line segments in that frame."
  [segment-list]
  (fn [frame]
    (let [m (frame-coord-map frame)]
      (doseq [[start end] segment-list]
        (draw-line (m start) (m end))))))

(defn transform-picture
  "Transforms a picture by into a different picture, based on new vectors
  for origin, e1, and e2. See SICP, 2nd ed., page 187-8. (Note that SICP
  calls pictures 'painters' because they know how to paint themselves."
  [p origin e1 e2]
  (fn [frame]
    (let [map (frame-coord-map frame)
          new-origin (map origin)]
      (p {:origin new-origin
          :e1 (sub-vec (map e1) new-origin)
          :e2 (sub-vec (map e2) new-origin)}))))

(defn flip-vert [p]
  (transform-picture p [0 1] [1 1] [0 0]))

(defn flip-horiz [p]
  (transform-picture p [1 0] [0 0] [1 1]))

(defn rotate [p]
  (transform-picture p [1 0] [1 1] [0 0]))

(defn rotate180 [p]
  (rotate (rotate p)))

(defn rotate270 [p]
  (rotate (rotate (rotate p))))

(defn beside [p1 p2]
  (let [split [0.5 0]
        left (transform-picture p1 [0 0] split [0 1])
        right (transform-picture p2 split [1 0] [0.5 1])]
    (fn [frame]
      (left frame)
      (right frame))))


(defn below
  "Draws p2 below p1."
  [p1 p2]
  (rotate (beside (rotate270 p1)
                  (rotate270 p2))))


(defn path
  "Creates a seq of line-segments from a 'bare' list of points. Use to
  draw a continuous line through the list of points."
  [& veclist]
  (partition 2 1 veclist))


(defn quartet [p1 p2 p3 p4]
  (below (beside p1 p2)
         (beside p3 p4)))

(defn square-of-four [tl tr
                      bl br]
  (fn [p]
    (let [top (beside (tl p) (tr p))
          bottom (beside (bl p) (br p))]
      (below top
             bottom))))



(defn split [f g]
  "Higher-order function that allows the
  following redefinitions:

    (def right-split (split beside below))
    (def up-split (split below beside))"
  (fn self-fn [p n]
    (if (= n 0)
      p
      (let [smaller (self-fn p (dec n))]
        (f p (g smaller smaller))))))

(def right-split (split beside below))
(def up-split (split below beside))



(defn corner-split [p n]
  (if (= n 0)
    p
    (let [up (up-split p (dec n))
          right (right-split p (dec n))
          top-left (beside up up)
          bottom-right (below right right)
          corner (corner-split p (dec n))]
      (beside (below p top-left)
              (below bottom-right corner)))))


(def combine-four (square-of-four rotate180
                                  flip-vert
                                  flip-horiz
                                  identity))

(defn square-limit [p n]
  (combine-four (corner-split p n)))

;; Example usage of path:
;; (path p1 p2 p3) -->
;;    (([0 0.35] [0.15 0.6]) ([0.15 0.6] [0.3 0.4]))

;; By defining segment-lists separately, you can combine figures
;; by concat'ting their segment-lists--see diamond-x.


;; making George drawing

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
   (path  p1  p2  p3  p4  p5)
   (path p6  p7  p8)
   (path p9 p10 p11)
   (path p12 p13 p14 p15 p16)
   (path p17 p18 p19 p20 p21 p22)))

(def george (segment-painter george-segs))


;; making arrow drawing

(def length-delta 0.1)
(def arrow-dx 0.1)
(def arrow-dy 0.3)

(def a1 (make-vec 0.5 (- 1 length-delta)))
(def a2 (make-vec 0.5 length-delta))
(def shaft-segs   (path a1 a2))

(def a3 (make-vec (- 0.5 arrow-dx) (+ arrow-dy length-delta)))
(def a4 (make-vec (+ 0.5 arrow-dx) (+ arrow-dy length-delta)))
(def head-segs   (path a3 a2 a4))

(def arrow (segment-painter (concat shaft-segs head-segs)))


;; making box drawing

(def box-segs
  (path (make-vec 0 0) (make-vec 1 0)
        (make-vec 1 1) (make-vec 0 1)
        (make-vec 0 0)))
(def box
  (segment-painter box-segs))

(def x-segs
  (concat (path (make-vec 0 0) (make-vec 1 1))
          (path (make-vec 1 0) (make-vec 0 1))))
(def x
  (segment-painter x-segs))


;; making diamond drawing

(def diamond-segs
  (path (make-vec 0.5 0) (make-vec 1 0.5)
        (make-vec 0.5 1) (make-vec 0 0.5)
        (make-vec 0.5 0)))

(def diamond
  (segment-painter diamond-segs))


;; making diamond-x drawing

(def diamond-x
  (segment-painter (concat diamond-segs x-segs)))


;; making diag drawing

(def diag (segment-painter [[[0 0] [1 1]]]))



(defn draw
  "Draws picture, using the entire Quil window."
  [picture]
  (picture whole-window))

(defn image-painter [img]
  (fn [{[ox oy] :origin
        [e1x e1y] :e1
        [e2x e2y] :e2
        }]
    (let [width (.width img)
          height (.height img)]
      ; COMPLETE
      )))

(defn draw-image
  "'Container' for executing drawings within a Quil sketch.
  Experiment by uncommenting one or more lines, or add your own."
  []
  (let [man (image-painter (q/load-image "data/man.gif"))
        bruce (image-painter (q/load-image "data/bruce.jpg"))
        angels (image-painter (q/load-image "data/angels.jpg"))]
    (q/background 255)
    ;; (frame-painter frame1)
    ;; (draw x)
    ;; (draw box)
    ;; (draw george)
    ;; (draw (rotate180 george))
    ;; (draw (beside (flip-horiz george) (flip-vert george)))
    ;; (draw (beside (flip-horiz george) george))
    ;; (draw (below (flip-horiz george) george))
    ;; (draw (right-split george 3))
    ;; (draw (up-split george 3))
    ;; (draw (beside box x))
    ;; (draw (flip-vert george))
    ;; (draw (beside box box))
    ;; (draw (combine-four george))
    ;; (draw (beside (below george george)
    ;;               (flip-horiz (below george george))))
    ;; (draw (below (beside george (flip-horiz george))
    ;;              (beside george (flip-horiz george))))

    ;; (draw ((square-of-four identity flip-vert
    ;;                        flip-horiz rotate)
    ;;        george))
    (draw (square-limit arrow 4))


    ; Needs image-painter
    ;; (bruce frame1)
    ;; (bruce frame2)
    ;; (draw (beside george bruce))
    ;; (draw (corner-split george 4))
    ;; (draw (beside  bruce (below  bruce
    ;;                              george)))
    ))

(q/defsketch escher
  :title "Escher"
  :draw draw-image
  :size [width height])

(defn -main [])
