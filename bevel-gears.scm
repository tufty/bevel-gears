;; https://khkgears.net/new/gear_knowledge/gear_technical_reference/calculation_gear_dimensions.html
;; http://mikesworkshop.weebly.com/designing-gear-cutters.html

(define pi 3.1415926)

(define (radians x) (/ (* x pi) 180))
(define (degrees-minutes-seconds x)
  (let* ((decimal-degrees (/ (* x 180) pi))
         (d (exact (floor decimal-degrees)))
         (m (exact (floor (* (- decimal-degrees d) 60))))
         (s (exact (floor (* (- decimal-degrees d (/ m 60)) 3600)))))
    (list d m s)))

(define (degrees x) (car (degrees-minutes-seconds x)))
(define (minutes x) (cadr (degrees-minutes-seconds x)))
(define (seconds x) (caddr (degrees-minutes-seconds x)))

(define delta
  (lambda (sigma z1 z2)
      (atan (/ (sin sigma) (+ (/ z2 z1) (cos sigma))))))      

(define cutter-for
  (lambda (t)
    (cond ((< t 14) 12)
          ((< t 17) 14)
          ((< t 21) 17)
          ((< t 26) 21)
          ((< t 35) 26)
          ((< t 55) 35)
          ((< t 135) 55)
          ( else 135))
    ))

;; Calculate bevel gear parameters for
;; sigma - gear angle
;; alpha - pressure angle
;; m - module
;; z1 z2 - tooth counts
;; Assume z1 <= z2

(define bevel-gears
  (lambda (sigma alpha m z1 z2)
    (let ((sigma (radians sigma)) (alpha (radians alpha)))
      (let* ((d1 (* z1 m))           ; Reference Diameter
             (d2 (* z2 m))
             (delta1 (delta sigma z1 z2))  ; Reference cone angles
             (delta2 (delta sigma z2 z1))
             (R (/ d2 (* 2 (sin delta2)))) ; Cone distance
             (b (floor (/ R 3)))
             (ha2 (+ (* 0.54 m) (/ (* 0.46 m) (/ (* z2 (cos delta1)) (* z1 (cos delta2))))))
             (ha1 (- (* 2 m) ha2))
             (hf1 (- (* 2.188 m) ha1))
             (hf2 (- (* 2.188 m) ha2))
             (thetaa1 (atan (/ ha1 R)))
             (thetaa2 (atan (/ ha2 R)))
             (thetaf1 (atan (/ hf1 R)))
             (thetaf2 (atan (/ hf2 R)))
             (deltaa1 (+ delta1 thetaa1))
             (deltaa2 (+ delta2 thetaa2))
             (deltaf1 (- delta1 thetaf1))
             (deltaf2 (- delta2 thetaf2))
             (da1 (+ d1 (* 2 ha1 (cos delta1))))
             (da2 (+ d2 (* 2 ha2 (cos delta2))))
             (X1 (- (* R (cos delta1)) (* ha1 (sin delta1))))
             (X2 (- (* R (cos delta2)) (* ha2 (sin delta2))))
             (Xb1 (/ (* b (cos deltaa1)) (cos thetaf2)))
             (Xb2 (/ (* b (cos deltaa2)) (cos thetaf1)))
             (di1 (- da1 (/ (* 2 b (sin deltaa1)) (cos thetaf2))))
             (di2 (- da2 (/ (* 2 b (sin deltaa2)) (cos thetaf1))))
             (td1 (* (sin delta1) d1))
             (td2 (* (sin delta2) d2))
             (o (* (sin (+ thetaa1 thetaf1)) (- b 1)))
             (c1 (cutter-for (floor (/ z1 (cos delta1)))))
             (c2 (cutter-for (floor (/ z2 (cos delta2)))))
             (involute1 (* c1 m (sin alpha)))
             (involute2 (* c2 m (sin alpha)))
             (rcd1 (* (- c1 2.314) m))
             (rcd2 (* (- c2 2.314) m))
             (bs1 (* c1 m (cos alpha) (sin (+ alpha (radians (/ 90 c1))))))
             (bs2 (* c2 m (cos alpha) (sin (+ alpha (radians (/ 90 c2))))))
             (infeed1 (/ (+ (- involute1 rcd1) (* c1 m (cos alpha) (cos (+ alpha (radians (/ 90 c1)))))) 2))
             (infeed2 (/ (+ (- involute2 rcd2) (* c2 m (cos alpha) (cos (+ alpha (radians (/ 90 c2)))))) 2))
             (abs1 (+ involute1 (* (/ (- R b) R) (- bs1 involute1))))
             (abs2 (+ involute2 (* (/ (- R b) R) (- bs2 involute2))))
             )

        (display (format "Shaft Angle       : ~d°~d′~d″\n" (degrees sigma) (minutes sigma) (seconds sigma)))
        (display (format "Pressure Angle    : ~d°~d′~d″\n" (degrees alpha) (minutes alpha) (seconds alpha)))
        (display (format "Module            : ~0,2F\n" m))
        (display (format "Face Width    (b) : ~0,2F mm\n" b))
        (display (format "Cut Depth         : ~0,2F mm\n" (* 2.188 m)))
        (display (format "Cut Setup specified as millimeters measured from compound slide perpendicularly to parallel per x mm compound slide travel (opposite and hypoteneuse of angle)\n"))
        (display "Pinion\n------\n")
        (display (format "\tTeeth                : ~d\n" z1))
        (display (format "\tReference ⌀     (d)  : ~0,2F mm\n" d1))
        (display (format "\tExterior ⌀      (da) : ~0,2F mm\n" da1))
        (display (format "\tInterior ⌀      (di) : ~0,2F mm\n" di1))
        (display (format "\tAxial Facewidth (Xb) : ~0,2F mm\n" Xb1))
        (display (format "\tMin Axial Width      : ~0,2F mm\n" (+ Xb1 (* (cos delta1) (+ ha1 hf1)))))
        (display (format "\tExterior Angle  (δa) : ~2,'0d°~2,'0d′~2,'0d″ : ~0,2F mm over 20 mm\n" (degrees deltaa1) (minutes deltaa1) (seconds deltaa1) (* (sin deltaa1) 20) ))
        (display (format "\tCut Angle       (δf) : ~2,'0d°~2,'0d′~2,'0d″ : ~0,2F mm over 20 mm\n" (degrees deltaf1) (minutes deltaf1) (seconds deltaf1) (* (sin deltaf1) 20)))
        (display (format "\tBack Angle    (90-δ) : ~2,'0d°~2,'0d′~2,'0d″ : ~0,2F mm over 20 mm\n" (degrees (- (/ pi 2) delta1)) (minutes (- (/ pi 2) delta1)) (seconds (- (/ pi 2) delta1)) (* (sin (- (/ pi 2) delta1)) 20)))
        (display (format "\tUse Cutter for ~d teeth\n" c1))
        (display (format "\t\tInvolute curve ⌀ : ~0,2F mm\n" involute1))
        (display (format "\t\tInfeed           : ~0,2F mm\n" infeed1))
        (display (format "\t\tButton Spacing   : ~0,2F mm\n" abs1))
        (display (format "\t\tOffset           : ~0,2F mm\n" (/ (* pi d1) (* 4 z1))))
        (display "Gear\n----\n")
        (display (format "\tTeeth                : ~d\n" z2))
        (display (format "\tReference ⌀     (d)  : ~0,2F mm\n" d2))
        (display (format "\tExterior ⌀      (da) : ~0,2F mm\n" da2))
        (display (format "\tInterior ⌀      (di) : ~0,2F mm\n" di2))
        (display (format "\tAxial Facewidth (Xb) : ~0,2F mm\n" Xb2))
        (display (format "\tMin Axial Width      : ~0,2F mm\n" (+ Xb2 (* (cos delta2) (+ ha2 hf2)))))
        (display (format "\tExterior Angle  (δa) : ~2,'0d°~2,'0d′~2,'0d″ : ~0,2F mm over 20 mm\n" (degrees deltaa2) (minutes deltaa2) (seconds deltaa2) (* (sin deltaa2) 20)))
        (display (format "\tCut Angle       (δf) : ~2,'0d°~2,'0d′~2,'0d″ : ~0,2F mm over 20 mm\n" (degrees deltaf2) (minutes deltaf2) (seconds deltaf2) (* (sin deltaf2) 20)))
        (display (format "\tBack Angle    (90-δ) : ~2,'0d°~2,'0d′~2,'0d″ : ~0,2F mm over 20 mm\n" (degrees (- (/ pi 2) delta2)) (minutes (- (/ pi 2) delta2)) (seconds (- (/ pi 2) delta2)) (* (sin (- (/ pi 2) delta2)) 20)))
        (display (format "\tUse Cutter for ~d teeth\n" c2))
        (display (format "\t\tInvolute curve ⌀ : ~0,2F mm\n" involute2))
        (display (format "\t\tInfeed           : ~0,2F mm\n" infeed2))
        (display (format "\t\tButton Spacing   : ~0,2F mm\n" abs2))
        (display (format "\t\tOffset           : ~0,2F mm\n" (/ (* pi d2) (* 4 z2))))


        ))))

(bevel-gears 90 20 1.5 15 30)




