#lang class/1
(require class/universe)
(require 2htdp/image)

;; Start a new game of Zombie Attack!
(define (start)
  (big-bang 
   (new world%
        (new player% (new posn% 0 0))
        (new mouse% (new posn% 0 0))
        (new cons-zombies%
                      (new slow-zombie% (new posn% 50 50))
                      (new empty-zombies%))
        (new cons-zombies%
             (new zombie% (new posn% 50 50))
             (new cons-zombies%
                  (new slow-zombie% (new posn% 50 50))
                  (new empty-zombies%))))))
                 
;; Constants
(define WIDTH 400)
(define HEIGHT 400)
(define MT-SCENE (empty-scene WIDTH HEIGHT))
(define PLAYER-SPEED 4)
(define ZOMBIE-SPEED 2)
(define ZOMBIE-RADIUS 10)
(define PLAYER-RADIUS 10)
(define PLAYER-IMG (circle PLAYER-RADIUS "solid" "green"))

;; A World is a (new world% Player Mouse Zombies Zombies)
(define-class world%
  (fields player mouse dead undead)
  
  ;; Update mouse position unless it has left the screen
  ;; on-mouse : Number Number MouseEvent -> World
  (define (on-mouse x y me)
    (new world%
         (this . player)
         (new mouse% 
              (cond [(string=? me "leave")
                     (this . player . posn)]
                    [else
                     (new posn% x y)]))
         (this . dead)
         (this . undead)))
  
  ;; Advance the player and zombies
  ;; on-tick : -> World
  (define (on-tick)
    (new world%
         (this . player . move-toward (this . mouse . posn))
         (send this mouse)
         (this . dead)
         (this . undead . move-toward (this . player . posn))))
  
  ;; Draw the player and zombies in this world
  ;; to-draw : -> Scene
  (define (to-draw)
    (this . player . draw-on          
          (this . dead . draw-on/color "gray"
                (this . undead . draw-on/color "red" MT-SCENE))))
  
  ;; Game is over when zombies touch the player
  ;; stop-when : -> Boolean
  (define (stop-when)
    (or (this . dead . touching-player? (this . player))
        (this . undead . touching-player? (this . player)))))


;; A Zombies implements
;;
;; Move all of these zombies toward the given position
;; move-toward : Posn -> Zombies
;;
;; Draw all of these zombies
;; draw-on/color : Color Scene -> Scene
;;
;; Are any of these zombies touching the given player?
;; touching-player? : Player -> Boolean

;; Interp: an empty set of zombies
(define-class empty-zombies%
  ;; Move none of these zombies toward the given position
  ;; move-toward : Posn -> Zombies
  (check-expect ((new empty-zombies%) . move-toward (new posn% 0 0))
                (new empty-zombies%))
  (define (move-toward p)
    this)
  
  ;; Draw none of these zombies
  ;; draw-on/color : Color Scene -> Scene
  (check-expect ((new empty-zombies%) . draw-on/color "purple" MT-SCENE)
                MT-SCENE)
  (define (draw-on/color color scn)
    scn)
  
  ;; Are any of these (zero) zombies touching the given player?
  ;; touching-player? : Player -> Boolean
  (check-expect ((new empty-zombies%) . touching-player? (new player% (new posn% 0 0)))
                false)
  (define (touching-player? player)
    false))

;; Interp: a zombie plus some zombies
;; A ConsZombies is a (new cons-zombies% Zombie Zombies)
(define-class cons-zombies%
  (fields first rest)
  ;; Move all of these zombies toward the given position
  ;; move-toward : Posn -> Zombies
  (check-expect ((new cons-zombies% 
                      (new zombie% (new posn% 0 0))
                      (new empty-zombies%)) . move-toward 
                                            (new posn% 50 50))
                (new cons-zombies%
                     ((new zombie% (new posn% 0 0)) . move-toward (new posn% 50 50))
                     (new empty-zombies%)))
  (define (move-toward p)
    (new cons-zombies% 
         (this . first . move-toward p)
         (this . rest . move-toward p)))
  
  ;; Draw all of these zombies in the given color
  ;; draw-on/color : Color Scene -> Scene
  (check-expect ((new cons-zombies% 
                      (new zombie% (new posn% 0 0))
                      (new empty-zombies%)) . draw-on/color "purple" MT-SCENE)
                ((new zombie% (new posn% 0 0)) . draw-on/color "purple" MT-SCENE))
  (define (draw-on/color color scn)
    (this . first . draw-on/color color
          (this . rest . draw-on/color color scn)))
  
  ;; Are any of these zombies touching the given player?
  ;; touching-player? : Player -> Boolean
  (check-expect ((new cons-zombies% 
                      (new zombie% (new posn% 0 0))
                      (new empty-zombies%)) . touching-player?
                                            (new player% (new posn% 0 0)))
                true)
  (define (touching-player? player)
    (or (this . first . touching-player? player)
        (this . rest . touching-player? player))))
    

;; A Zombie implements:
;;
;; move-toward : Posn -> Zombie
;; Move this zombie toward the given position
;;
;; draw-on/color : Color Scene -> Scene
;; Draw this zombie in given color on scene
;;
;; touching-player? : Player -> Boolean
;; Is this zombie touching the given player?

;; Interp: a full-speed zombie at posn
(define-class zombie%
  (fields posn)
  ;; move-toward : Posn -> Zombie
  ;; Move this zombie toward the given position
  (check-expect ((new zombie% (new posn% 0 0)) . move-toward
                                               (new posn% ZOMBIE-SPEED 0))
                (new zombie% (new posn% ZOMBIE-SPEED 0)))
  (define (move-toward p)
    (new zombie% (this . posn . move-toward/speed p ZOMBIE-SPEED)))
  
  ;; draw-on/color : Color Scene -> Scene
  ;; Draw this zombie in given color on scene
  (check-expect ((new zombie% (new posn% 0 0)) . draw-on/color "red" MT-SCENE)
                ((new posn% 0 0) . draw-on/image 
                                 (circle ZOMBIE-RADIUS "solid" "red")
                                 MT-SCENE))
  (define (draw-on/color color scn)
    (this . posn . draw-on/image
          (circle ZOMBIE-RADIUS "solid" color)
          scn))
  
  ;; touching-player? : Player -> Boolean
  ;; Is this zombie touching the given player?
  (check-expect ((new zombie% (new posn% 0 0)) . touching-player? 
                                               (new player% (new posn% 0 0)))
                true)
  (define (touching-player? player)
    (<= (player . posn . dist (this . posn))
        ZOMBIE-RADIUS)))
    
;; Interp: a half-speed zombie at posn
(define-class slow-zombie%
  (fields posn)
  ;; move-toward : Posn -> Zombie
  ;; Move this zombie toward the given position  
  (define (move-toward p)
    (new slow-zombie% (this . posn . move-toward/speed p
                            (/ ZOMBIE-SPEED 2))))
  
  ;; draw-on/color : Color Scene -> Scene
  ;; Draw this zombie in given color on scene  
  (define (draw-on/color color scn)
    (this . posn . draw-on/image
          (circle ZOMBIE-RADIUS "solid" color)
          scn))
  
  ;; touching-player? : Player -> Boolean
  ;; Is this zombie touching the given player?
  (define (touching-player? player)
    (<= (player . posn . dist (this . posn))
        ZOMBIE-RADIUS)))

;; Interp: a player at posn
(define-class player%
  (fields posn)
  ;; move-toward : Posn -> Player
  ;; Move this player toward the given position
  (check-expect ((new player% (new posn% 0 0)) . move-toward 
                                               (new posn% PLAYER-SPEED 0))
                (new player% (new posn% PLAYER-SPEED 0)))
  (define (move-toward p)
    (new player% (this . posn . move-toward/speed p PLAYER-SPEED)))
  
  ;; draw-on : Scene -> Scene
  ;; Draw this player on the scene
  (check-expect ((new player% (new posn% 0 0)) . draw-on MT-SCENE)
                ((new posn% 0 0) . draw-on/image PLAYER-IMG MT-SCENE))
  (define (draw-on scn)
    (this . posn . draw-on/image PLAYER-IMG scn)))

;; Interp: mouse position
(define-class mouse%
  (fields posn))

;; Interp: position in graphics-coordinates system
(define-class posn%
  (fields x y)
  ;; Move this position toward that one at given velocity.
  ;; move-toward : Point Number -> Posn
  (check-expect ((new posn% 0 0) . move-toward/speed (new posn% 0 0) 100)
                (new posn% 0 0))
  (check-expect ((new posn% 0 0) . move-toward/speed (new posn% 100 0) 50)
                (new posn% 50 0))
  (check-expect ((new posn% 0 0) . move-toward/speed (new posn% 0 100) 50)
                (new posn% 0 50))
  (check-expect ((new posn% 0 0) . move-toward/speed (new posn% 100 100) 50)
                (new posn% 50 0))
  (check-expect ((new posn% 0 0) . move-toward/speed (new posn% 100 101) 50)
                (new posn% 0 50))
  (check-expect ((new posn% 0 0) . move-toward/speed (new posn% 101 100) 50)
                (new posn% 50 0))
  (define (move-toward/speed that speed)
    (local [(define δ-x (- (that . x) (this . x)))
            (define δ-y (- (that . y) (this . y)))
            (define move-distance
              (min speed
                   (max (abs δ-x)
                        (abs δ-y))))]
 
      (cond [(< (abs δ-x) (abs δ-y))             
             (this . move ;; move along y-axis
                   0
                   (cond [(positive? δ-y) move-distance]
                         [else (- move-distance)]))]
            [else             
             (this . move ;; move along x-axis
                   (cond [(positive? δ-x) move-distance]
                         [else (- move-distance)])
                   0)])))
 
  ;; Move this position by given offsets
  ;; move : Number Number -> Posn
  (check-expect ((new posn% 10 10) . move 10 20)
                (new posn% 20 30))
  (define (move δ-x δ-y)
    (new posn%
         (+ (this . x) δ-x)
         (+ (this . y) δ-y)))
 
  ;; Draw given image on scene at this position
  ;; draw-on/image : Image Scene -> Scene
  (check-expect ((new posn% 10 10) . draw-on/image PLAYER-IMG MT-SCENE)
                (place-image PLAYER-IMG 10 10 MT-SCENE))
  (define (draw-on/image img scn)
    (place-image img
                 (this . x)
                 (this . y)
                 scn))
 
  ;; dist : Point -> Number
  ;; Compute the distance between this posn and that point.
  (check-expect ((new posn% 0 0) . dist (new posn% 3 4)) 5)
  (define (dist that)
    (sqrt (+ (sqr (- (that . y) (this . y)))
             (sqr (- (that . x) (this . x)))))))

