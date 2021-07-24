;; Includes
(require 2htdp/image)
(require 2htdp/universe)

;; Constants
(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; Data Definitions

;; drop
(define-struct drop (x y))
;; drop is (make-drop Natural Natural)
;; A raindrop on screen with x and y coordinates

(define D1 empty)
(define D2 (make-drop 50 150))
(define D3 (make-drop 280 300))
(define D4 (make-drop 280 100))

#;
(define (func-for-drop d)
 (... (drop-x d) (drop-y d)))

;; ListOfdrop is one of:
;; -  empty
;; -  ListOfdrop (cons drop ListOfdrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons D2 (cons D3 empty)))
(define LOD3 (cons D3 (cons D2 empty)))
(define LOD4 (cons D4 (cons D3 (cons D2 empty))))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop


;; Functions

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
(check-expect (handle-mouse LOD1 250 250 "button-down") (cons (make-drop 250 250) LOD1))
(check-expect (handle-mouse LOD2 250 250 "button-down") (cons (make-drop 250 250) LOD2))

;(define (handle-mouse lod x y mevt) empty) ; stub

(define (handle-mouse lod x y mevt)
 (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
       [else lod]))


;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
(check-expect (next-drops LOD2) (cons (make-drop  (drop-x (first LOD2)) (+ (drop-y (first LOD2)) 10 ))
                                   empty))
(check-expect (next-drops LOD3) (cons (make-drop  (drop-x (first LOD3)) (+ (drop-y (first LOD3)) 10 ))
                                   (cons (make-drop (drop-x (first (rest LOD3))) (+ (drop-y (first (rest LOD3))) 20))
                                         empty)))

(check-expect (next-drops LOD4) (cons (make-drop (drop-x (first LOD4)) (+ (drop-y (first LOD4)) 10 ))
                                   (cons (make-drop (drop-x (first (rest (rest LOD4)))) (+ (drop-y (first (rest (rest LOD4)))) 30))
                                   empty)))

;(define (next-drops lod) empty) ; stub

(define (next-drops lod)
  (cond [(empty? lod) empty]
        [else
         (advance (cons (first lod)
              (next-drops (rest lod))))]))

;; ListOfDrop -> ListOfDrop
;; Takes images and moves pixel to down 10 and removes drops with y-coord greater than 300 (using remove-overflow)
(check-expect (advance LOD1) empty)
(check-expect (advance LOD2) (cons (make-drop  (drop-x (first LOD2)) (+ (drop-y (first LOD2)) 10 ))
                                   empty))
(check-expect (advance LOD3) (cons (make-drop  (drop-x (first LOD3)) (+ (drop-y (first LOD3)) 10 ))
                                   (cons (make-drop (drop-x (first (rest LOD3))) (+ (drop-y (first (rest LOD3))) 10))
                                         empty)))

(check-expect (advance LOD4) (cons (make-drop  (drop-x (first LOD4)) (+ (drop-y (first LOD4)) 10 ))
                                   (cons (make-drop (drop-x (first (rest (rest LOD4)))) (+ (drop-y (first (rest (rest LOD4)))) 10))
                                   empty)))
;(define (advance lod) lod)

(define (advance lod)
  (cond [(empty? lod) empty]
        [else
         (cons  (make-drop (drop-x (first lod)) (+ (drop-y (first lod)) 10))
              (advance (remover (rest lod))))]))


; ;; Drop -> Drop
; ;; If a drop's y cood is over 300 it 


;;ListOfdrop -> ListOfdrop
;; If a drop's y-coord is over 300 removes it from the list
(check-expect (remover LOD1) empty)
(check-expect (remover LOD2) (cons (make-drop 50 150) empty))
(check-expect (remover LOD3) (cons (make-drop 50 150) empty))

;(define (remover lod) lod) ;stub
(define (remover lod)
  (cond [(empty? lod) empty]
        [else
         (if (>= (drop-y (first lod)) 300)
              (remover (rest lod))
               (cons (first lod) (remover (rest lod))))]))

;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops LOD1) MTS)
(check-expect (render-drops LOD2) (place-image DROP (drop-x (first LOD2)) (drop-y (first LOD2))
                                               (place-image DROP (drop-x (first (rest LOD2))) (drop-y (first (rest LOD2))) MTS)))
(check-expect (render-drops LOD3) (place-image DROP (drop-x (first LOD3)) (drop-y (first LOD3))
                                               (place-image DROP (drop-x (first (rest LOD3))) (drop-y (first (rest LOD3))) MTS)))
(check-expect (render-drops LOD4) (place-image DROP (drop-x (first LOD4)) (drop-y (first LOD4))
                                               (place-image DROP (drop-x (first (rest LOD4))) (drop-y (first (rest LOD4)))
                                                            (place-image DROP (drop-x (first (rest (rest LOD4)))) (drop-y (first (rest (rest LOD4)))) MTS))))


;(define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (place-image DROP (drop-x (first lod)) (drop-y (first lod))
              (render-drops (rest lod)))]))
