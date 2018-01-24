
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2017                              *
; *  Student Version - Baljit Kaur            *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

(define bitvector-list
  (lambda (hashfunctionlist dict)
     (cond ((null? dict) '())
	   ((= (length dict) 1) (append (bitvector hashfunctionlist (car dict)) '()))
	   (else (append (bitvector hashfunctionlist (car dict)) (bitvector-list hashfunctionlist (cdr dict)))))
))

(define bitvector
  (lambda (hashfunctionlist w)
     (cond ((null? hashfunctionlist) '())
           ((= (length hashfunctionlist) 1) (cons ((car hashfunctionlist) w) '()))
     	   (else (cons ((car hashfunctionlist) w) (bitvector (cdr hashfunctionlist) w))))
))

(define member?
   (lambda (x y)	
      (cond ((null? y) #f)
	    ((= x (car y)) #t)
            (else (member? x (cdr y))))))

(define keyhelper
   (lambda (n k)
      (cond ((= k 5187) (+ (ctv n) (* 5187 29)))
	    (else (+ (ctv n) (* k 29))))))

(define gen-checker-helper
  (lambda (hashfunctionlist bitlist w)
     (cond ((null? hashfunctionlist) #t)
	   ((eq? (member? ((car hashfunctionlist) w) bitlist) #f) #f)
	   (else (gen-checker-helper (cdr hashfunctionlist) bitlist w)))
))

;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
	(reduce keyhelper w 5187)
))

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 106402241991
;;   (key '(m a y))           = 126526810
;;   (key '(t r e e f r o g)) = 2594908189083745

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (lst)
	(modulo (key lst) size))
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (lst)
	(floor (* size (- (* (key lst) A) (floor (* (key lst) A))))))
))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 7224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))       ==> 35616
;;  (hash-1 '(m a y))           ==> 46566
;;  (hash-1 '(t r e e f r o g)) ==> 48238
;;
;;  (hash-2 '(h e l l o))       ==> 48849
;;  (hash-2 '(m a y))           ==> 81025
;;  (hash-2 '(t r e e f r o g)) ==> 16708
;;
;;  (hash-3 '(h e l l o))       ==> 6331.0
;;  (hash-3 '(m a y))           ==> 2456.0
;;  (hash-3 '(t r e e f r o g)) ==> 1806.0
;;
;;  (hash-4 '(h e l l o))       ==> 788.0
;;  (hash-4 '(m a y))           ==> 306.0
;;  (hash-4 '(t r e e f r o g)) ==> 225.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
     (define lst (bitvector-list hashfunctionlist dict))
     (lambda (w)
	(cond ((null? lst) #f)
	      ((null? hashfunctionlist) #f)
	      (else (gen-checker-helper hashfunctionlist lst w))))                              
))


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ;;==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #t  // false positive
