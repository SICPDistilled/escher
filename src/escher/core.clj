(ns escher.core
  (require [quil.core :as q])
  (:gen-class))

(def width 600)
(def height 600)

(def draw-line q/line)

(def whole-window {:origin [0 0]
                   :e1 [width 0]
                   :e2 [0 height]})

(def frame1 {:origin [200 50]
             :e1 [200 100]
             :e2 [150 200]})

(def frame2 {:origin [50 50]
             :e1 [100 0]
             :e2 [0 200]})

(defn add-vec [[x1 y1] [x2 y2]]
  ;; COMPLETE (Ex 2.46)
 )

(defn sub-vec [[x1 y1] [x2 y2]]
  ;; COMPLETE
  )

(defn scale-vec [[x y] s]
  ;; COMPLETE
  )

(defn frame-coord-map
  [{:keys [origin e1 e2]}]
  (fn [[x y]]
    (add-vec origin
             (add-vec (scale-vec e1 x)
                      (scale-vec e2 y)))))

(defn frame-painter [{:keys [origin e1 e2]}]
  (let [corner (add-vec origin (add-vec e1 e2))]
    (draw-line origin (add-vec origin e1))
    (draw-line origin (add-vec origin e2))
    (draw-line (add-vec origin e2) corner)
    (draw-line (add-vec origin e1) corner)))

(defn segment-painter [segment-list]
  (fn [frame]
    (let [m (frame-coord-map frame)]
      (doseq [[start end] segment-list]
        (draw-line (m start) (m end))))))

(defn transform-picture [p origin e1 e2]
  (fn [frame]
    (let [map (frame-coord-map frame)
          new-origin (map origin)]
      (p {:origin new-origin
          :e1 (sub-vec (map e1) new-origin)
          :e2 (sub-vec (map e2) new-origin)}))))

(defn flip-vert [p]
  (transform-picture p [0 1] [1 1] [0 0]))

(defn flip-horiz [p]
  ;; COMPLETE (Ex 2.50)
  )

(defn rotate [p]
  ;; COMPLETE
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

(defn below [p1 p2]
  ; COMPLETE (Ex 2.51)
  )

(defn path [& veclist]
  ; COMPLETE
  )


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

(defn right-split [p n]
  (if (= n 0)
    p
    (let [smaller (right-split p (dec n))]
      (beside p (below smaller smaller)))))

(defn up-split [p n]
  ;; COMPLETE (Ex 2.44)
  )


(defn split [f g]
  ; COMPLETE (Ex 2.45)
  "Should be able to do
    (def right-split (split beside below))
    (def up-split (split below beside)
  and replace the existing *-split fns"
  )

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

(def combine-four (square-of-four flip-horiz
                                  identity
                                  rotate180
                                  flip-vert))

(defn square-limit [p n]
  (combine-four (corner-split p n)))

; Ex2.49, Make these shapes with segment-painter/path
(def box )
(def x)
(def diamond)
(def george)

(defn draw [picture]
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

(def diag (segment-painter [[[0 0] [1 1]]]))

(defn draw-image []
  (let [man (image-painter (q/load-image "data/man.gif"))
        bruce (image-painter (q/load-image "data/bruce.jpg"))
        angels (image-painter (q/load-image "data/angels.jpg"))]
    (q/background 255)
    ;; (george frame2)
    ;; (bruce frame1)
    ;; (bruce frame2)
    ;; (draw (beside george bruce))
    ;; (draw (flip-horiz george))
    ;; (draw (beside box box))
    ;; (draw  (combine-four george))
    ;; (draw (corner-split bruce 4))
    ;; (draw (square-limit bruce 3))
    ;; (draw (beside  bruce (below  bruce
    ;;                              george)))
    ;; (draw (beside (below george george)
    ;;               (flip-horiz (below george george))))
    ;; (draw (below (beside george (flip-horiz george))
    ;;              (beside george (flip-horiz george))))
    ;; (draw (rotate george))
    ;; (frame-painter frame1)
    ;; (frame-painter frame2)
    ;; (diag frame1)
    ;; (diag frame2)
    ;; (draw (up-split bruce 4))
    ;; (draw (corner-split bruce 1))
    ;; (draw (square-limit bruce 4))
    ;; (draw (square-limit angels 4))
    ;; (draw bruce)
    ;; (draw (corner-split george 4))
    ;; (draw (quartet george box man bruce))
    ;; (draw ((square-of-four identity flip-vert
    ;;                        flip-horiz rotate)
    ;;        george))
    ;; (draw (below bruce
    ;;              man))
    ;; (q/save "square-of-four.png")
    ))

(q/defsketch escher
  :title "Escher"
  :draw draw-image
  :size [width height])

(defn -main [])
