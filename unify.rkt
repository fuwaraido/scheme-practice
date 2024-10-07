#lang racket
;;
;; This is an implementaton of unification described in
;;  https://cs.brown.edu/courses/cs173/2002/Lectures/2002-11-13.pdf
;;
;; That document is claerest explanation of unification algorithm
;; available on the web I think. Expecially the person like me who
;; doesn't speak "math" very well. It's very "algorithmic" explanation.
;; Sounds good.
;;
;; This implementation does *not* intend to practical use. It's
;; just a reference or temporal code for small project.
;;
;; The unify routine is the main procedure of this small piece.
;; It was written especially for type reconstruction of some static
;; typed language. It feeds list of dot pair describes the equation
;; and solve it. Each equation must be correspond to the constraints
;; of expression types in the program.
;;
;; This code doesn't do any algebra but It's reasonable to solve
;; any small simultaneous eqations I suppose. You have to feed the
;; data like following example, you'll get the result (Probably).
;;
;; Now, we have a simultaneous eqations like:
;; t0 = int
;; t1 = t2
;; t6 = t4 -> int
;; t4 = int
;; (Okay this is terrible example...)
;;
;; To feed this you convert them into some list like:
;; (define eqls ((?t0 . int) (?t1 . ?t2) (?t6 . (?t4 ?t5)) (?t4 int)))
;;
;; Then feed it to "unify" routine, you'll get:
;; ((?t4 . int) (?t6 . (int int)) (?t1 . ?t2) (?t0 . int))
;;
;; It means the solution of above simultaneous eqations is:
;; t4 = int
;; t6 = int -> int
;; t1 = t2
;; t0 = int
;;
;; t1, t2 is not determined because there is not substitite.
;; (It means it's arbitary. It's "generic typed". you might
;; seen something like 'a -> ('a -> 'a) in the console of
;; your favorite functional language...that "'a" is like
;; t1, t2 term in above equation. It's could be any type)
;;
;; Every symbol supposed to represent a temporal typename
;; of each sub expresson. For example, assigning t0 to integer
;; literal 5, assigning t6 to function like (lambda (x) x) and so on,
;; you can get the right type of each variable and function (Probably).
;;
;; You can also use this function to get the solution of
;; simple simultaneous eqations but you have a lot better way
;; to do that.
;;

;;
;; variable must starts with ?
;;
(define (is-variable? x)
  (eq? (string-ref (symbol->string x) 0) #\?))

;;
;; composite equation is the list of symbols
;;
(define (is-composite? x)
  (list? x))

;;
;; sub function of subst
;;
;; if elt is a list, this function
;; apply itself to every elements of elt
;;
(define (subst-elt x t elt)
  (cond [(is-composite? elt)
         (map (lambda (e) (subst-elt x t e)) elt)]
        [(eq? elt x) t]
        [else elt]))

;;
;; substitute x by t in every car/cdr elements
;; in constraints.
;;
(define (subst x t constraints)
  (map (lambda (e)
         (cons (subst-elt x t (car e))
               (subst-elt x t (cdr e))))       
       constraints))

;;
;; Occur-check. If symbol a appeard in expression b,
;; return #t (and halt unify)
;;
(define (occur-check a b)
  (if (and (list? b) (not (empty? b)))
      (if (occur-check a (car b))
          #t
          (occur-check a (cdr b)))
      (eq? a b)))

;; Turn off occur check to see some "clever" but
;; incorrect solution for the equation containing
;; self recursion.
;;(define (occur-check a b) #f)

;:
;; unify algorithm main procedure.
;;
(define (unify constraints substitution)
  ;; (display constraints) (display " $ ") (display substitution) (newline) ;; <<<<<<< comment out this to see what's going on.
  ;; unify until the list of constraints gets empty!
  (if (empty? constraints)
      substitution ;; returns the result. ex.) ((t0 . int) (t1 . int -> int)....)
      (let ((e (car constraints)))
        (cond ;; symbols are equals ex.) t0 = t0 so it solved!
              [(eq? (car e) (cdr e))
               (unify (cdr constraints) substitution)]
              
              ;; for composite equation. ex.) (s0 s1 s2) = (t0 t1 t2)...
              ;;     produces the sequence like: s0 = t0, s1 = t1, s2 = t2.
              [(and (is-composite? (car e)) (is-composite? (cdr e)))
               (unify (append (map cons (car e) (cdr e)) (cdr constraints))
                      substitution)]
              
              ;; car part of the constraints is a variable.
              ;;  ex.) (?x . (s0 s1 s2)), (?x . int) or so on.
              [(is-variable? (car e))
               (if (occur-check (car e) (cdr e))
                   '()
                   (unify (subst (car e) (cdr e) (cdr constraints))
                          (cons e (subst (car e) (cdr e) substitution))))]
              
              ;; cdr part of the constraints is a variable.
              [(is-variable? (cdr e))
               (if (occur-check (cdr e) (car e))
                   '()
                   (unify (subst (cdr e) (car e) (cdr constraints))
                          (cons e (subst (cdr e) (car e) substitution))))]
              
              ;; unify fails! (no consistent solution)
              [else '()]))))

;; unifier test
(define constraints
  '((?t0 . int) (?t1 . ?t2) (?t6 . (?t4 int)) (?t4 . int)))
(unify constraints '())

;;
;; Following example fails because of occur-check.
;;
(define constraints2
  '((?t0 . ?t1) (?t1 . ?t2) (?t3 . (?t1 ?t3)) (?t4 . ?t3)))
(unify constraints2 '())
;;
;; But removing "occur-check" you'll get funny result like this:
;;
;; ((?t4 ?t2 ?t3) (?t3 ?t2 ?t3) (?t1 . ?t2) (?t0 . ?t2))
;;
;; This means the right hand side of the third element is not
;; properly evaluated. It sould be replaced by t3 and cause
;; infinite recursion but this algorithm evaluate it like:
;;
;; t3 = (t1 t3)     (3rd equation in "constraints")
;; t1 = t2          (2nd equation in "constraints")
;;
;; Therefore:
;; t3 = (t2 t3)
;;
;; Replacing t3 on the left side by (t2 t3) yields:
;; (t2 t3) = (t2 t3)
;; states SOLVED.
;;
;; But in fact, it sould be like this:
;; (t2 (t2 (t2 ... t3))))).. = (t2 (t2 (t2 (t2 ... t3)))) ..(INFINITE + 1 paren here!)
;;
;; Because mathematically "Inifinite = Infinite + 1" so the equation above is
;; somehow "valid" (and this is the correct answer by definition).
;;
;; Uhhhhhhh....I really don't know this could be a bug or???
;; Anyway this case wouldn't happen if occur-checks are
;; enabled. But interesting flaw and actually, it "solves"
;; the problem in some unexpectable way.
;;
;; I think this happends because this "unify" implementation
;; solve equations one by one. it proceed like, left side
;; is the symbol then evaluate the right side, right side of
;; the symbol ... an goes on and on. If all variable must be
;; detemined "at once" (not sequentially), it supposed
;; to cause infinite recursion on (t3 . (t2 t3))
;; (without occur-check). Proably in haskell, it does. 
;; But because of default lazy evaluation it wait for
;; infinite recursion instead of going into infinite loop.
;;
