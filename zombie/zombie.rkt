#lang class/1
(require class/universe)
(require 2htdp/image)
(require "jack-o-lantern.rkt")

;; Start a new game of Zombie Attack!
(define (start) 
  (big-bang 
   (new world%
        (new player% origin)
        origin
        (new hoard%
             ;; undead zombies
             (new cons-zombies%
                  (new zombie% (new posn% 300 300))
                  (new cons-zombies%
                       (new slow-zombie% (new posn% 400 400))
                       empty-zs))
             ;; dead zombies
             (new cons-zombies%
                  (new slow-zombie% (new posn% 200 200))
                  empty-zs)))))
                 
;; Constants
(define WIDTH 400)
(define HEIGHT 400)
(define MT-SCENE (empty-scene WIDTH HEIGHT))
(define PLAYER-SPEED 4)
(define ZOMBIE-SPEED 2)
(define ZOMBIE-RADIUS 20)
(define PLAYER-RADIUS 20)
(define PLAYER-IMG (jack-o-lantern PLAYER-RADIUS "green"))

;; A Loc(ation) implements
;; - posn : -> Posn
;; - Get the position of this location

;; A Posn implements Loc and
;; - x : -> Real
;; - Get x-coorinate
;;
;; - y : -> Real
;; - Get y-coordinate
;;
;; - move-toward/speed : Loc Real -> Posn
;; - Move horizontally or vertically toward location at given speed
;;
;; - Draw image at this position on scene
;; - draw-on/image : Image Scene -> Scene
;;
;; - dist : Loc -> Real
;; - Compute distance from this position to given location

;; A Hoard implements:
;; - move-toward : Loc -> Hoard
;; - Move all zombies in this hoard toward location
;;
;; - eat-brains : -> Hoard
;; - Zombies eat all touching brains
;; 
;; - touching? : Loc -> Boolean
;; - Are any zombies in this hoard touching given location?
;;
;; - draw-on : Scene -> Scene
;; - Draw this hoard on the scene

;; A Zombie implements Loc and:
;; - move-toward : Loc -> Zombie
;; - Move this zombie toward the given position
;;
;; - draw-on/color : Color Scene -> Scene
;; - Draw this zombie in given color on scene
;;
;; - touching? : Loc -> Boolean
;; - Is this zombie touching the given location?

;; A Player implements Loc and
;; - move-toward : Loc -> Player
;; - Move this player toward given location
;;
;; - draw-on : Scene -> Scene
;; - Draw this player on scene

;; A Zombies implements
;; - move-toward : Posn -> Zombies
;; - Move all of these zombies toward the given position
;;
;; - draw-on/color : Color Scene -> Scene
;; - Draw all of these zombies
;;
;; - touching? : Loc -> Boolean
;; - Are any of these zombies touching the given location?


;; A World is a (new world% Player Loc Hoard)
;; Interp: Player, mouse location, hoard of zombies
(define-class world%
  (fields player mouse zombies)
  
  ;; Update mouse position unless it has left the screen
  ;; on-mouse : Number Number MouseEvent -> World
  (define (on-mouse x y me)
    (new world%
         (this . player)
         (cond [(string=? me "leave")
                (this . player)]
               [else
                (new posn% x y)])
         (this . zombies)))
  
  ;; Advance and eat brains!
  ;; on-tick : -> World
  (define (on-tick)    
    (new world%
         (this . player . move-toward (this . mouse))
         (this . mouse)
         (this . zombies . eat-brains . move-toward (this . player))))
  
  ;; Draw the player and zombies in this world
  ;; to-draw : -> Scene
  (define (to-draw)
    (this . player . draw-on          
          (this . zombies . draw-on MT-SCENE)))
  
  ;; Game is over when zombies touch the player
  ;; stop-when : -> Boolean
  (define (stop-when)
    (this . zombies . touching? (this . player))))

;; Interp: a hoard of undead and dead zombies
(define-class hoard%
  (fields undead dead)
  ;; draw-on : Scene -> Scene
  (define (draw-on scn)
    (this . undead . draw-on/color "red"
          (this . dead . draw-on/color "gray"
                scn)))
  
  ;; touching? : Loc -> Boolean
  (define (touching? loc)
    (or (this . undead . touching? loc)
        (this . dead . touching? loc)))
  
  ;; move-toward : Loc -> Hoard
  (define (move-toward loc)
    (new hoard%
         (this . undead . move-toward loc)
         (this . dead)))
  
  ;; eat-brains : -> Hoard
  (define (eat-brains)
    (this . undead . kill-all (this . dead))))


;; Interp: an empty set of zombies
(define-class empty-zombies%
  ;; Move none of these zombies toward the given position
  ;; move-toward : Posn -> Zombies
  (check-expect (empty-zs . move-toward origin) empty-zs)
  (define (move-toward p) this)
  
  ;; Draw none of these zombies
  ;; draw-on/color : Color Scene -> Scene
  (check-expect (empty-zs . draw-on/color "red" MT-SCENE) MT-SCENE)
  (define (draw-on/color color scn) scn)
  
  ;; Are any of these (zero) zombies touching the given location?
  ;; touching? : Loc -> Boolean
  (check-expect (empty-zs . touching? origin) false)
  (define (touching? loc) false)
  
  ;; kill-all : Zombies -> Hoard
  (define (kill-all dead)
    (new hoard% empty-zs dead)))


;; Interp: a zombie plus some zombies
;; A ConsZombies is a (new cons-zombies% Zombie Zombies)
(define-class cons-zombies%
  (fields first rest)
  ;; Move all of these zombies toward the given position
  ;; move-toward : Posn -> Zombies
  (check-expect (origin-zs . move-toward origin) origin-zs)
  (check-expect (origin-zs . move-toward (new posn% 50 50))
                (new cons-zombies%
                     (origin-z . move-toward (new posn% 50 50))
                     empty-zs))  
  (define (move-toward p)
    (new cons-zombies% 
         (this . first . move-toward p)
         (this . rest  . move-toward p)))
  
  ;; Draw all of these zombies in the given color
  ;; draw-on/color : Color Scene -> Scene  
  (check-expect (origin-zs . draw-on/color "purple" MT-SCENE)
                (origin-z . draw-on/color "purple" MT-SCENE))
  (define (draw-on/color color scn)
    (this . first . draw-on/color color
          (this . rest . draw-on/color color scn)))
  
  ;; Are any of these zombies touching the given location?
  ;; touching? : Loc -> Boolean
  (define (touching? loc)
    (or (this . first . touching? loc)
        (this . rest  . touching? loc)))
  
  ;; kill-all : Zombies -> Hoard
  ;; Kill all touching zombies in this set and given dead zombies
  (check-expect (origin-zs . kill-all empty-zs)
                (new hoard% origin-zs empty-zs))
  (check-expect (origin-zs . kill-all origin-zs)
                (new hoard%
                     empty-zs
                     (new cons-zombies% origin-z origin-zs)))
  (define (kill-all dead)
    (cond [(or (this . rest . touching? (this . first))
               (dead . touching? (this . first)))
           (this . rest . kill-all
                 (new cons-zombies% (this . first) dead))]
          [else
           (local [(define res (this . rest . kill-all dead))]
             (new hoard%
                  (new cons-zombies% (this . first) (res . undead))
                  (res . dead)))])))


;; Interp: a full-speed zombie at posn
(define-class zombie%
  (fields posn)
  ;; move-toward : Loc -> Zombie
  ;; Move this zombie toward the given location
  (check-expect (origin-z . move-toward (new posn% ZOMBIE-SPEED 0))
                (new zombie% (new posn% ZOMBIE-SPEED 0)))
  (define (move-toward p)
    (new zombie% (this . posn . move-toward/speed p ZOMBIE-SPEED)))
  
  ;; draw-on/color : Color Scene -> Scene
  ;; Draw this zombie in given color on scene
  (check-expect (origin-z . draw-on/color "red" MT-SCENE)
                (origin . draw-on/image 
                        (jack-o-lantern ZOMBIE-RADIUS "red")
                        MT-SCENE))
  (define (draw-on/color color scn)
    (this . posn . draw-on/image
          (jack-o-lantern ZOMBIE-RADIUS color)
          scn))
  
  ;; touching? : Loc -> Boolean
  ;; Is this zombie touching the given location?
  (check-expect (origin-z . touching? origin) true)
  (check-expect (origin-z . touching? origin-z) true)  
  (define (touching? loc)
    (<= (this . posn . dist loc)
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
          (jack-o-lantern ZOMBIE-RADIUS color)
          scn))
  
  ;; touching? : Loc -> Boolean
  ;; Is this zombie touching the given location?
  (check-expect (origin-zs . touching? origin-z) true)
  (define (touching? loc)
    (<= (this . posn . dist loc)
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


;; Interp: position in graphics-coordinates system
(define-class posn%
  (fields x y)
  
  ;; Loc implementation
  (define (posn) this)
  
  ;; Move this position toward that one at given velocity.
  ;; move-toward : Loc Number -> Posn
  (check-expect (origin . move-toward/speed origin 100)
                origin)
  (check-expect (origin . move-toward/speed (new posn% 100 0) 50)
                (new posn% 50 0))
  (check-expect (origin . move-toward/speed (new posn% 0 100) 50)
                (new posn% 0 50))
  (check-expect (origin . move-toward/speed (new posn% 100 100) 50)
                (new posn% 50 0))
  (check-expect (origin . move-toward/speed (new posn% 100 101) 50)
                (new posn% 0 50))
  (check-expect (origin . move-toward/speed (new posn% 101 100) 50)
                (new posn% 50 0))
  (define (move-toward/speed that speed)
    (local [(define δ-x (- (that . posn . x) (this . x)))
            (define δ-y (- (that . posn . y) (this . y)))
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
 
  ;; dist : Loc -> Number
  ;; Compute the distance between this posn and that location.
  (check-expect (origin . dist (new posn% 3 4)) 5)
  (define (dist that)
    (sqrt (+ (sqr (- (that . posn . y) (this . y)))
             (sqr (- (that . posn . x) (this . x)))))))


;; --
;; Definitions for testing
(define origin (new posn% 0 0))
(define origin-z (new zombie% origin))
(define empty-zs (new empty-zombies%))
(define origin-zs (new cons-zombies% origin-z empty-zs))
