;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 3)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
(define T3 (make-tank WIDTH 1))         ;going right and hitting right edge
(define T4 (make-tank 0 -1))            ;ging left and hitting left edge 

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 100 100 1)) ;regular invader, moving right
(define I5 (make-invader 50 50 -1)) ;regular invader, moving left
(define I6 (make-invader 0 50 -1)) ;moving left, hitting left edge
(define I7 (make-invader WIDTH 50 1)) ;moving right, hitting right edge
(define I9 (make-invader 7 7 1)) ;regular invader, moving right
#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile 100 100)) ;direct hit I4
(define M5 (make-missile (+ 100 HIT-RANGE) 100)) ;hit I4 on x-axis edge
(define M6 (make-missile (+ 100 (/ HIT-RANGE 2)) 100)) ;hit I4 on centre of x-axis HIT-RANGE
(define M7 (make-missile 100 (+ 100 HIT-RANGE))) ;hit I4 on y-axis edge
(define M8 (make-missile 100 (+ 100 (/ HIT-RANGE 2)))) ; ;hit I4 on centre of y-axis HIT-RANGE
(define M9 (make-missile 15 15)) ;hitting I9 within HIT-RANGE
(define M10 (make-missile 20 20)) ;no hits
(define M11 (make-missile 20 0)) ;missile half way off canvas
(define M12 (make-missile 50 (- 0 (image-height MISSILE)))) ;missile completely off canvas
(define M13 (make-missile 50 (- -2 (image-height MISSILE)))) ;missile more than a missile length off canvas

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvader)
;; - reference: (first loi) is Invader
;; - self reference: (rest loi) is ListOfInvader


;; ListOfMissile is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOM1 empty)
(define LOM2 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvader)
;; - reference: (first loi) is Invader
;; - self reference: (rest loi) is ListOfInvader


;; =================
;; Functions:

;; Game -> Game
;; start the world with an initial game state (s), i.e. (main G0)
;; 
(define (main s)
  (big-bang s                   ; Game
    (on-tick   next-state)     ; Game -> Game
    (to-draw   render-game)   ; Game -> Image
    (stop-when game-over?)      ; Game -> Boolean
    (on-key    control-tank)))    ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next state of the invaders game
;; This will:
;; - move the tank forward in the direction provided by on-key
;; - advance the missiles
;; - advance the invaders
;; - generate new invaders
(check-random (next-state (make-game (list I4 I5)
                                     (list M4 M1)
                                     T2))
              (cond [(> INVADE-RATE (random 100))
                     (make-game (list
                                 (make-invader
                                  (random WIDTH) 0 1)
                                 (make-invader
                                  (+ (invader-x I5) (* (invader-dx I5) INVADER-X-SPEED))
                                  (+ (invader-y I5) INVADER-Y-SPEED)
                                  (invader-dx I5)))
                                (list (make-missile
                                       (missile-x M1)
                                       (- (missile-y M1) MISSILE-SPEED)))
                                (make-tank
                                 (+ (tank-x T2) (* (tank-dir T2) TANK-SPEED))
                                 (tank-dir T2)))]
                    [else (make-game (list (make-invader
                                            (+ (invader-x I5) (* (invader-dx I5) INVADER-X-SPEED))
                                            (+ (invader-y I5) INVADER-Y-SPEED)
                                            (invader-dx I5)))
                                     (list (make-missile
                                            (missile-x M1)
                                            (- (missile-y M1) MISSILE-SPEED)))
                                     (make-tank
                                      (+ (tank-x T2) (* (tank-dir T2) TANK-SPEED))
                                      (tank-dir T2)))]))

;(define (next-state s) s) ;stub

(define (next-state s)
  (make-game (new-invaders (next-loinvader (clean-invaders-list (game-invaders s) (game-missiles s))))
             (next-lom (clean-missiles-list (game-missiles s) (game-invaders s)))
             (next-tank (game-tank s))))

;; ListOfInvaders -> ListOfInvaders
;; Randomly generate new invaders
;; Currently using a fixed invader-dx. Random could be used to create different movement speeds.
(check-random (new-invaders empty) (cond
                                     [(> INVADE-RATE (random 100))
                                      (list (make-invader (random WIDTH) 0 1))]
                                     [else empty]))
(check-random (new-invaders (list I4))
              (cond [(> INVADE-RATE (random 100))
                     (list (make-invader (random WIDTH) 0 1) I4)]
                    [else (list I4)]))


;(define (new-invaders loi) loi) ;stub

(define (new-invaders loi)
  (cond [(> INVADE-RATE (random 100))
         (cons (make-invader (random WIDTH) 0 1) loi)]
        [else loi]))
        

;; ListOfInvader -> ListOfInvader
;; Produce the next list of invaders
(check-expect (next-loinvader empty) empty) ; empty base case
(check-expect (next-loinvader (list I4 I5)) ; list with 2 regular invaders, moving both sides
              (list (make-invader
                     (+ (invader-x I4) (* (invader-dx I4) INVADER-X-SPEED))
                     (+ (invader-y I4) INVADER-Y-SPEED)
                     (invader-dx I4))
                    (make-invader
                     (+ (invader-x I5) (* (invader-dx I5) INVADER-X-SPEED))
                     (+ (invader-y I5) INVADER-Y-SPEED)
                     (invader-dx I5))))
(check-expect (next-loinvader (list I6)) ; list with invader hitting the left edge
              (list (make-invader
                     (- (invader-x I6) (* (invader-dx I6) INVADER-X-SPEED))
                     (+ (invader-y I6) INVADER-Y-SPEED) 1)))
(check-expect (next-loinvader (list I7)) ; list with invader hitting the right edge
              (list (make-invader
                     (- (invader-x I7) (* (invader-dx I7) INVADER-X-SPEED))
                     (+ (invader-y I7) INVADER-Y-SPEED) -1)))
                                 
;(define (next-loinvader loi) loi) ;stub
(define (next-loinvader loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-invader (first loi))
               (next-loinvader (rest loi)))]))


;; Invader -> Invader
;; Produce the next invader, taking in account the borders of the canvas
(check-expect (next-invader I4) ;regular invader moving right
              (make-invader
               (+ (invader-x I4) (* (invader-dx I4) INVADER-X-SPEED))
               (+ (invader-y I4) INVADER-Y-SPEED)
               (invader-dx I4)))
(check-expect (next-invader I5) ;regular invader moving left
              (make-invader
               (+ (invader-x I5) (* (invader-dx I5) INVADER-X-SPEED))
               (+ (invader-y I5) INVADER-Y-SPEED)
               (invader-dx I5)))
(check-expect (next-invader I6) ;hit left side of canvas
              (make-invader
               (- (invader-x I6) (* (invader-dx I6) INVADER-X-SPEED))
               (+ (invader-y I6) INVADER-X-SPEED) 1))
(check-expect (next-invader I7) ;hit right side of canvas
              (make-invader
               (- (invader-x I7) (* (invader-dx I7) INVADER-X-SPEED))
               (+ (invader-y I7) INVADER-Y-SPEED) -1))

;(define (next-invader i) i) ;stub

(define (next-invader i)
  (cond [(<= (invader-x i) 0)
         (make-invader (- (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       1)]
        [(>= (invader-x i) WIDTH)
         (make-invader (- (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       -1)]
        [else (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                            (+ (invader-y i) INVADER-Y-SPEED)
                            (invader-dx i))]))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; Check if a invader has been hit and remove it from the list
(check-expect (clean-invaders-list empty empty) empty) ;empty base case
(check-expect (clean-invaders-list
               (list I4 I5)
               (list M9 M10))
              (list I4 I5)) ;no hits
(check-expect (clean-invaders-list
               (list I4 I5)
               (list M4 M9))
              (list I5)) ;direct hit
(check-expect (clean-invaders-list
               (list I4 I5)
               (list M5 M9))
              (list I5)) ;hit on edge x-axis
(check-expect (clean-invaders-list
               (list I4 I5)
               (list M6 M9))
              (list I5)) ;hit inside x-axis HIT-RANGE
(check-expect (clean-invaders-list
               (list I4 I5)
               (list M7 M9))
              (list I5)) ;hit on edge y-axis
(check-expect (clean-invaders-list
               (list I4 I5)
               (list M8 M9))
              (list I5)) ;hit inside y-axis HIT-RANGE
(check-expect (clean-invaders-list (list I4 I5 I9)
                                   (list M4 M9))
              (list I5)) ;multiple hits

;(define (clean-invaders-list loi lom) loi) ;stub

(define (clean-invaders-list loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (check-invader-hit (first loi) lom)
             (clean-invaders-list (rest loi) lom)
             (cons (first loi)                           
                   (clean-invaders-list (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; Return true if an invader has been hit
(check-expect (check-invader-hit I4 empty) false) ;empty base case
(check-expect (check-invader-hit I4 
                                 (list M9 M10))
              false) ;invader missed
(check-expect (check-invader-hit I4 
                                 (list M4 M9))
              true) ;direct hit
(check-expect (check-invader-hit I4 
                                 (list M5 M9))
              true) ;hit on edge x-axis
(check-expect (check-invader-hit I4 
                                 (list M6 M9))
              true) ;hit inside x-axis HIT-RANGE
(check-expect (check-invader-hit I4
                                 (list M7 M9))
              true) ;hit on edge y-axis
(check-expect (check-invader-hit I4
                                 (list M8 M9))
              true) ;hit inside y-axis HIT-RANGE

;(define (invader-hit? i lom) true) ;stub

(define (check-invader-hit i lom)
  (cond [(empty? lom) false]
        [else (if (invader-hit? i (first lom))
                  true
                  (check-invader-hit i (rest lom)))]))

;; Invader Missile -> Boolean
;; Produce true if a missile hits an invader
(check-expect (invader-hit? I4 M9)
              false) ;no hit
(check-expect (invader-hit? I4 M4)
              true) ;direct hit
(check-expect (invader-hit? I4 M5)
              true) ;hit on edge x-axis
(check-expect (invader-hit? I4 M6)
              true) ;hit inside x-axis HIT-RANGE
(check-expect (invader-hit? I4 M7)
              true) ;hit on edge y-axis
(check-expect (invader-hit? I4 M8)
              true) ;hit inside y-axis HIT-RANGE

; (define (invader-hit? i m) true) ;stub

(define (invader-hit? i m)
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))


;; ListOfMissile ListOfInvader -> ListOfMissile
;; Check if a missile has hit an invader and remove it from the list
(check-expect (clean-missiles-list empty empty) empty) ;empty base case
(check-expect (clean-missiles-list (list M9 M10)
                                   (list I4 I5))
              (list M9 M10)) ;no hits
(check-expect (clean-missiles-list (list M4 M9)
                                   (list I4 I5))
              (list M9)) ;direct hit
(check-expect (clean-missiles-list (list M5 M9)
                                   (list I4 I5))
              (list M9)) ;hit on edge x-axis
(check-expect (clean-missiles-list (list M6 M9)
                                   (list I4 I5))
              (list M9)) ;hit inside x-axis HIT-RANGE
(check-expect (clean-missiles-list (list M7 M9)
                                   (list I4 I5))
              (list M9)) ;hit on edge y-axis
(check-expect (clean-missiles-list (list M8 M9)
                                   (list I4 I5))
              (list M9)) ;hit inside y-axis HIT-RANGE
(check-expect (clean-missiles-list (list M8 M9 M10)
                                   (list I4 I9))
              (list M10)) ;multiple hits

;(define (clean-missiles-list lom loi) lom) ;stub

(define (clean-missiles-list lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (check-missile-hit (first lom) loi)
             (clean-missiles-list (rest lom) loi)
             (cons (first lom)                           
                   (clean-missiles-list (rest lom) loi)))]))

;; Missile ListOfInvader -> Boolean
;; Produce true if a missile hits an invader
(check-expect (check-missile-hit M4 empty) false) ;empty base case
(check-expect (check-missile-hit M4 (list I5 I6))
              false) ;missed
(check-expect (check-missile-hit M4 (list I4 I5))
              true) ;direct hit
(check-expect (check-missile-hit M5 (list I4 I5))
              true) ;hit on edge x-axis
(check-expect (check-missile-hit M6 (list I4 I5))
              true) ;hit inside x-axis HIT-RANGE
(check-expect (check-missile-hit M7 (list I4 I5))
              true) ;hit on edge y-axis
(check-expect (check-missile-hit M8 (list I4 I5))
              true) ;hit inside y-axis HIT-RANGE

;(define (check-missile-hit m loi) true) ;stub

(define (check-missile-hit m loi)
  (cond [(empty? loi) false]
        [else (if (invader-hit? (first loi) m)
                  true
                  (check-missile-hit m (rest loi)))]))

;; ListOfMissiles -> ListOfMissiles
;; Produce the next list of missiles and excludes missiles that are fully off canvas
(check-expect (next-lom empty) empty) ;empty base case
(check-expect (next-lom (list M9 M10)) ;no missiles off canvas
              (list (make-missile (missile-x M9)
                                  (- (missile-y M9) MISSILE-SPEED))
                    (make-missile (missile-x M10)
                                  (- (missile-y M10) MISSILE-SPEED))))
(check-expect (next-lom (list M10 M11 M12)) ;M11 is half-way off canvas, M12 completely 
              (list (make-missile (missile-x M10)
                                  (- (missile-y M10) MISSILE-SPEED))
                    (make-missile (missile-x M11)
                                  (- (missile-y M11) MISSILE-SPEED))))

;(define (next-lom lom) lom) ;stub

(define (next-lom lom)
  (cond [(empty? lom) empty]
        [else 
         (if (off-canvas? (first lom))
             (next-lom (rest lom))
             (cons (next-missile (first lom))
                   (next-lom (rest lom))))]))

;; Missile -> Boolean
;; Produce true if the Missile is off canvas (- 0 (image-height MISSILE))
(check-expect (off-canvas? M4) false) ;regular missile
(check-expect (off-canvas? M11) false) ;missile half-way off
(check-expect (off-canvas? M12) true) ;missile completely off
(check-expect (off-canvas? M13) true)  ;missile more than a missile length off canvas

;(define (off-canvas? m) true) ;stub

(define (off-canvas? m)
  (<= (missile-y m) (- 0 (image-height MISSILE))))

;; Missile -> Missile
;; Produce the next location of the missile
(check-expect (next-missile M4)
              (make-missile (missile-x M4)
                            (- (missile-y M4) MISSILE-SPEED)))
(check-expect (next-missile M5)
              (make-missile (missile-x M5)
                            (- (missile-y M5) MISSILE-SPEED)))

;(define (next-missile m) m) ;stub
(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; Tank -> Tank
;; Move the tank TANK-SPEED in the defined direction, until it reaches the edge
(check-expect (next-tank T1)
              (make-tank (+ (tank-x T1) (* (tank-dir T1) TANK-SPEED))
                         (tank-dir T1))) ;regular tank, moving right
(check-expect (next-tank T2) 
              (make-tank (+ (tank-x T2) (* (tank-dir T2) TANK-SPEED))
                         (tank-dir T2))) ;regular tank, moving left
(check-expect (next-tank T3)
              (make-tank (tank-x T3) 0)) ;tank hitting right edge
(check-expect (next-tank T4)
              (make-tank (tank-x T4) 0)) ;tank hitting left edge

;(define (next-tank t) t) ;stub

(define (next-tank t)
  (cond [(and (= (tank-dir t) -1)(<= (tank-x t) 0))
         (make-tank 0 0)]
        [(and (= (tank-dir t) 1)(>= (tank-x t) WIDTH))
         (make-tank WIDTH 0)]
        [else (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))])) 


;; Game -> Image
;; render the current state of the invaders game on BACKGROUND
(check-expect (render-game (make-game empty empty T1))
              (place-image TANK (tank-x T1) TANK-Y BACKGROUND))
(check-expect (render-game (make-game
                            (list I6 I7)
                            (list M4 M5)
                            T1))
              (place-image INVADER
                           (invader-x I6)
                           (invader-y I6)
                           (place-image INVADER
                                        (invader-x I7)
                                        (invader-y I7)
                                        (place-image MISSILE
                                                     (missile-x M4)
                                                     (missile-y M4)
                                                     (place-image MISSILE
                                                                  (missile-x M5)
                                                                  (missile-y M5)
                                                                  (place-image TANK
                                                                               (tank-x T1)
                                                                               TANK-Y
                                                                               BACKGROUND))))))

;(define (render-game s) BACKGROUND) ;stub

(define (render-game s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s)))))

;; ListOfInvaders, Image -> Image
;; Render the invaders on the background with tank and missiles
(check-expect (render-invaders empty BACKGROUND) BACKGROUND) ;empty base case
(check-expect (render-invaders (list I4 I5)
                               (render-missiles
                                (list M9 M10)
                                (render-tank T1)))
              (place-image INVADER (invader-x I4) (invader-y I4)
                           (place-image INVADER (invader-x I5) (invader-y I5)
                                        (render-missiles
                                         (list M9 M10)
                                         (render-tank T1))))) ;2 invaders/missiles
                                                     
;(define (render-invaders loi img) img) ;stub

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (render-invader (first loi)
                         (render-invaders (rest loi) img))]))

;; Invader, Image -> Image
;; Render an invader on the current state of the canvas
(check-expect (render-invader I4 (render-missiles empty (render-tank T1)))
              (place-image INVADER (invader-x I4) (invader-y I4)
                           (render-missiles empty
                                            (render-tank T1)))) ;empty ListOfMissiles
(check-expect (render-invader I4
                              (render-missiles
                               (list M9 M10)
                               (render-tank  T1)))
              (place-image INVADER (invader-x I4) (invader-y I4)
                           (render-missiles
                            (list M9 M10)
                            (render-tank T1)))) ;2 missiles on canvas

;(define (render-invader i img) img) ;stub

(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))


;; ListOfMissiles, Image -> Image
;; Render the missiles on the background with tank
(check-expect (render-missiles empty (render-tank T1))
              (render-tank T1)) ;empty base case
(check-expect (render-missiles
               (list M9)
               (render-tank T1))
              (place-image MISSILE (missile-x M9) (missile-y M9)
                           (render-tank T1))) ;render a single missile
(check-expect (render-missiles
               (list M9 M10)
               (render-tank T1))
              (place-image MISSILE (missile-x M9) (missile-y M9)
                           (place-image MISSILE (missile-x M10) (missile-y M10)
                                        (render-tank T1)))) ;render multiple missiles
                     

;(define (render-missiles lom img) img) ;stub

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (render-missile (first lom)
                         (render-missiles (rest lom) img))]))

;; Missile, Image -> Image
;; Renders missile on the current state of the canvas
(check-expect (render-missile
               M9
               (render-tank T1))
              (place-image MISSILE (missile-x M9) (missile-y M9)
                           (render-tank T1)))

;(define (render-missile m img) img) ;stub

(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))

;; Tank -> Image
;; Render the tank on the background/canvas
(check-expect (render-tank T1) ;regular tank
              (place-image TANK (tank-x T1) TANK-Y BACKGROUND))
(check-expect (render-tank T3) ;tank hitting right edge 
              (place-image TANK (tank-x T3) TANK-Y BACKGROUND))
(check-expect (render-tank T4) ;tank hitting left edge
              (place-image TANK (tank-x T4) TANK-Y BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub

(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y BACKGROUND))


;; Game -> Boolean
;; check if an invader has reached the bottom of the screen
(check-expect (game-over? G0) false) ;game with no invaders
(check-expect (game-over? G2) false) ;game with a normal invader
(check-expect (game-over? G3) true) ;game with landed invader I2

;(define (game-over? s) true) ;stub

(define (game-over? s)
  (invaders-landed? (game-invaders s)))

;; ListOfInvaders -> Boolean
;; Produce true if an invader has hit the bottom of the canvas
(check-expect (invaders-landed? empty) false)
(check-expect (invaders-landed? (list I4 I5)) false) ;regular invaders
(check-expect (invaders-landed? (list I2)) true) ;invader on bottom
(check-expect (invaders-landed? (list I3)) true) ;invader passed bottom

;(define (invader-landed? s) true) ;stub
              
(define (invaders-landed? loi)
  (cond [(empty? loi) false]
        [else (or (>= (invader-y (first loi)) HEIGHT)
                  (invaders-landed? (rest loi)))]))

;; Game KeyEvent -> Game
;; Control the tank with the left and right keys and fire missiles with spacebar
(check-expect (control-tank (make-game (list I4) (list M9) T1) " ")
              (make-game (list I4)
                         (list (make-missile (tank-x T1) TANK-Y) M9)
                         T1))
(check-expect (control-tank (make-game (list I4) (list M9) T1) "left")
              (make-game (list I4) (list M9) (make-tank (tank-x T1) -1))) 
(check-expect (control-tank (make-game (list I4) (list M9) T1) "right")
              (make-game (list I4) (list M9) (make-tank (tank-x T1) 1)))
(check-expect (control-tank (make-game (list I4) (list M9) T1) "B")
              (make-game (list I4) (list M9) T1))

; (define (control-tank s ke) s) ;stub

(define (control-tank s ke)
  (cond [(key=? ke " ")
         (make-game
          (game-invaders s)
          (cons (make-missile (tank-x (game-tank s)) TANK-Y) (game-missiles s))
          (game-tank s))]
        [(key=? ke "left")
         (make-game
          (game-invaders s)
          (game-missiles s)
          (make-tank (tank-x (game-tank s)) -1))]
        [(key=? ke "right")
         (make-game
          (game-invaders s)
          (game-missiles s)
          (make-tank (tank-x (game-tank s)) 1))]
        [else s]))