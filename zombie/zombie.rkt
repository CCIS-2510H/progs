#lang class/0
(require class/universe)
(require 2htdp/image)

;; Start a new game of Zombie Attack!
(define (start)
  (big-bang (new world%
                 (new player% (new posn% 0 0))
                 (new mouse% (new posn% 0 0)))))

(define WIDTH 400)
(define HEIGHT 400)
(define MT-SCENE (empty-scene WIDTH HEIGHT))
(define PLAYER-SPEED 1)
(define PLAYER-RADIUS 10)
(define PLAYER-IMG (circle PLAYER-RADIUS "solid" "green"))

(define-class world%
  (fields player mouse)
  (define (on-tick)
    (new world%
         (send (send this player) move-toward (send (send this mouse) posn))
         (send this mouse)))
  (define (to-draw)
    (send (send this player) draw-on
          MT-SCENE)))

(define-class player%
  (fields posn)
  (define (move-toward p)
    (new player% (send (send this posn) move-toward/speed p PLAYER-SPEED)))
  (define (draw-on scn)
    (send (send this posn) draw-on/image PLAYER-IMG scn)))

(define-class mouse%
  (fields posn))

(define-class posn%
  (fields x y)
  ; Move this position toward that one at given velocity.
  ; move-toward : Point Number -> Posn
  (check-expect (send (new posn% 0 0) move-toward/speed (new posn% 0 0) 100)
                (new posn% 0 0))
  (check-expect (send (new posn% 0 0) move-toward/speed (new posn% 100 0) 50)
                (new posn% 50 0))
  (check-expect (send (new posn% 0 0) move-toward/speed (new posn% 0 100) 50)
                (new posn% 0 50))
  (check-expect (send (new posn% 0 0) move-toward/speed (new posn% 100 100) 50)
                (new posn% 50 0))
  (check-expect (send (new posn% 0 0) move-toward/speed (new posn% 100 101) 50)
                (new posn% 0 50))
  (check-expect (send (new posn% 0 0) move-toward/speed (new posn% 101 100) 50)
                (new posn% 50 0))
  (define (move-toward/speed that speed)
    (local [(define delta-x (- (send that x) (send this x)))
            (define delta-y (- (send that y) (send this y)))
            (define move-distance
              (min speed
                   (max (abs delta-x)
                        (abs delta-y))))]
 
      (cond [(< (abs delta-x) (abs delta-y))
             ; move along y-axis
             (cond [(positive? delta-y)
                    (send this move 0 move-distance)]
                   [else
                    (send this move 0 (- move-distance))])]
            [else
             ; move along x-axis
             (cond [(positive? delta-x)
                    (send this move move-distance 0)]
                   [else
                    (send this move (- move-distance) 0)])])))
 
  ; Move this position by given offset
  ; move : Number Number -> Posn
  (check-expect (send (new posn% 10 10) move 10 20)
                (new posn% 20 30))
  (define (move delta-x delta-y)
    (new posn%
         (+ (send this x) delta-x)
         (+ (send this y) delta-y)))
 
  ; Draw given image on scene at this position
  ; draw-on/image : Image Scene -> Scene
  (check-expect (send (new posn% 10 10) draw-on/image PLAYER-IMG MT-SCENE)
                (place-image PLAYER-IMG 10 10 MT-SCENE))
  (define (draw-on/image img scn)
    (place-image img
                 (send this x)
                 (send this y)
                 scn))
 
  ; dist : Point -> Number
  ; Compute the distance between this posn and that point.
  (define (dist that)
    (sqrt (+ (sqr (- (send that y) (send this y)))
             (sqr (- (send that x) (send this x)))))))

