#lang racket
;;
;; Parser combinator library
;;
;; This is a perser combinator and there are a
;; four primitives for PEG like notations.
;;
;; selection: p1 | p2
;; sequence: p1 p2
;; negative look-ahead: !p1
;; repetition: p1*
;;
;; User can create complex perser combining these
;; primitive functions. Each primitive parser
;; returns OK, EPSN, FAIL, ERROR.
;; When perser detect ERROR state, it stops.
;; This is based on four-value scheme (google it!).
;; It assume that target syntax is basically LL(1)
;; and emit ERROR in one of following case:
;;
;; 1. The second token in the "sequence" rule fails
;; 2. All alternative in "selection" rule fails.
;; 3. Whenever parser function returns ERROR.
;;
;; Each code means:
;; OK : successfully parsed and consumed token.
;; EPSN: successfully parsed without consuming token.
;;  (This case occurs on repeat and not parser)
;; FAIL: parser failed without consuming token.
;;  (This means "try alternative!")
;; ERROR: parser failed to try all alternative or
;; LL(1) assumption denied on sequence. ERROR
;; propagates and stop entire process at the
;; position of the error occured.
;;
;; Without this mechanism, perser works on LL(k)
;; grammar but it's difficult to detect error under
;; those circumstances...
;;
;; Read following paper for detailed explaination. This is not
;; an original paper of four-value scheme... I couldn't find original
;; on the web. :(
;;
;; "Exception Handling for Error Reporting in Parsing Expression Grammars". 
;; http://www.inf.puc-rio.br/~roberto/docs/sblp2013-1.pdf
;;
;; Don't worry. I said this perser assume LL(1)
;; but you can use PEG notation and look-ahead
;; operator! Basically you won't have any trouble
;; with complex grammar I guess. Besides if you manually
;; insert look-ahead opearator, you can parse LL(?)
;; grammar by these combinators!
;;
;; p.s ? depends on your effort :P
;;


;;
;; string utility
;;
(define (trim-right-regexp whitespace str)
  (let ((r (regexp-match-positions whitespace str)))
    (substring str 0 (caar r))))

(define (trim-left-regexp whitespace str)
  (let ((r (regexp-match-positions whitespace str)))
    (substring str (cdar r) (string-length str))))

(define (trim str)
  (trim-left-regexp "^[ \t]*"
   (trim-right-regexp "[ \t]*$" str)))

;;
;; parser result type definition & utilities
;;
;; (input value status cause)
;;

;;
;; getter and setter for parse result
;;
(define (error-cause r)
  (cadddr r))

(define (parse-input r)
  (car r))

(define (parse-value r)
  (cadr r))

(define (parse-status r)
  (caddr r))

;;
;; constructor for parse result
;;
(define (parse-result input value status error)
  `(,input ,value ,status ,error))

(define (parse-ok input pos)
  `(,(substring input (cdr pos))
    ,(trim (substring input (car pos) (cdr pos)))
    OK
    ()))

(define (parse-epsn input)
  `(,input () EPSN ()))

(define (parse-fail input cause)
  `(,input () FAIL ,cause))

(define (parse-error input cause)
  `(,input () ERROR ,cause))


;;
;; Helper function for combine-result
;; I know equivalant function already exists
;; somewhere in the library...but I just don't
;; know which one is!
;;
(define (make-list r1 r2)
  (cond ((and (list? r1) (list? r2)
              (append r1 r2)))
        ((list? r1) (append r1 (list r2)))
        ((list? r2) (cons r1 r2))
        (else (cons r1 (list r2)))))
     
;;
;; This is special constructor for sequence
;; parser. 
;;
(define (combine-result r1 r2)
  (parse-result
   (parse-input r2)   
   (make-list (parse-value r1) (parse-value r2)) ;; uncomment this to get faltted list as parse result
   ;;(cons (parse-value r1) (parse-value r2)) ;; uncomment this to get binary tree as parse result
   (parse-status r2)
   '()))

;;
;; some predicates
;;

(define (parse-success? r)
  (or (eq? (parse-status r) 'OK)
      (eq? (parse-status r) 'EPSN)))

(define (parse-failed? r)
  (or (eq? (parse-status r) 'FAIL)
      (eq? (parse-status r) 'ERROR)))

(define (parse-error? r)
  (eq? (parse-status r) 'ERROR))

;;
;; In four-value scheme, both OK and ERROR
;; means parser consumed some tokens.
;; When parser consume more than 1 token
;; and fails, it makes entire parser fail
;; because of LL(1) restriction.
;; (it treated as an ERROR)
;;
(define (parse-consumed? r)
  (or (eq? (parse-status r) 'OK)
      (eq? (parse-status r) 'ERROR)))

;;
;; On the other hand EPSN and FAIL
;; means perser failed/succeeded *without*
;; consuming any token. So getting FAIL
;; from one parser doesn't make entire
;; parser fail. FAIL means parser should
;; try alternatives when it's possible.
;; (And if there is no alternative,
;; parser returns ERROR)
;;
(define (parse-not-consumed? r)
  (or (eq? (parse-status r) 'EPSN)
      (eq? (parse-status r) 'FAIL)))

;;(parse-ok "foo bar" '(0 . 3))
;;(parse-error "foo bar" '(0 . 3) #f)
;;(parse-fail "foo bar" '(0 . 3) #f)
;;(parse-epsn "foo bar")

;;
;; regex parser
;;
;; Use this as a clude lexer. you can combine regex-parser
;; with parser combinator too. it's very useful.
;;
(define (regex-parser regex)
  (lambda (input)
    (let ((result (regexp-match-positions regex input)))
      (if (eq? result #f)
          ;;(parse-error input `(,regex ,(string-length input)))
          (parse-fail input `(,regex ,(string-length input)))
          (parse-ok input (car result))))))

;;
;; parser combinators
;;

;;
;; parse p1 | p2
;;
;; This operator parse selection.
;; The rule of fail/error of this operator
;; is a bit complicated...I won't explain in detail.
;;
(define (or-pair p1 p2)
  (lambda (input)
    (let ((r1 (p1 input)))
      (if (parse-consumed? r1)
          r1 ;; OK or ERROR
          (let ((r2 (p2 input)))
            (match (cons (parse-status r1) (parse-status r2))
              ((cons 'EPSN  'FAIL) (parse-epsn input))
              ((cons 'EPSN  'EPSN) (parse-epsn input))
              ((cons 'EPSN  _) r2)
              ((cons 'FAIL  'FAIL) (choose-error r1 r2))
              ((cons 'FAIL  _) r2)))))))

;;
;; support function for or-pair
;;
;; This is a heuristic. When or-pair perser detects
;; an ERROR state, it compares the length of the
;; text left by two alternatives. And grant shoter
;; one as longest much input...and it *probably*
;; represents right cause of errors...
;;
(define (choose-error r1 r2)
  (if (< (string-length (parse-input r1))
         (string-length (parse-input r2)))
      (parse-error (parse-input r1) (error-cause r1))
      (parse-error (parse-input r2) (error-cause r2))))
              
;;
;; parse p1 * p2
;; 
;; This operator parses sequence of p1 p2.
;; The rule of fail/error of this operator
;; is a bit complicated...
;;
(define (and-pair p1 p2)
  (lambda (input)
    (let ((r1 (p1 input)))
      (if (parse-failed? r1)
          r1 ;; FAIL or ERROR
          (let ((r2 (p2 (parse-input r1))))
            (match (cons (parse-status r1) (parse-status r2))
              ((cons 'EPSN _) r2)
              ((cons 'OK 'ERROR) (parse-error input (error-cause r2)))
              ((cons 'OK 'FAIL) (parse-error input (error-cause r2)))
              ((cons 'OK _) (combine-result r1 r2))))))))

;;
;; parse !p
;;
;; This operator doesn't consume any input, and never
;; returns an error (return EPSN/FAIL because this is
;; a look-ahead operator).
;;
(define (not p)
  (lambda (input)
    (let ((r (p input)))
      (if (parse-failed? r)
          (parse-epsn input)
          (parse-fail input (error-cause r))))))

;;
;; parse p* (many or none)
;;
;; This operator tries to match p until p
;; returns fail or error.
;;
(define (repeat p)
  (lambda (input)
    (letrec ((iter (lambda (input)
                     (let ((r (p input)))                       
                           (if (parse-failed? r)
                               (parse-result (parse-input r) '() 'OK '())
                               (combine-result r (iter (parse-input r))))))))
      (iter input))))


;;
;; apply the function specified by "action" parameter to
;; all successful result of parser "p"
;;
(define (apply-action p action)
  (lambda (input)
    (let ((r (p input)))
      (parse-result (parse-input r) (action (parse-value r))
                    (parse-status r) (error-cause r)))))

;;
;; add "error-tag" to a parser
;; when parsing fails, error-tag combinator add error infos
;; to error-cause in parse-result
;;
(define (extend-error-cause r error-infos)
  (parse-result
   (parse-input r)
   (parse-value r)
   (parse-status r)
   (cons (error-cause r) error-infos)))

(define (error-tag p error-infos)
  (lambda (input)
    (let ((r (p input)))
      (if (parse-failed? r)
          (extend-error-cause r error-infos)
          r))))

;;
;; utility function to make sequence
;;
;;(define (or list)
;;  (foldr (lambda (x y) (or-pair x y)) (last list) (drop-right list 1)))

;;(define (and list)
;;  (foldr (lambda (x y) (and-pair x y)) (last list) (drop-right list 1)))


;;
;; macro for sequence/selection construction
;;
(define-syntax OR
  (syntax-rules ()
    ((_ e) e)
    ((_ e1 e2 ...)
     (or-pair e1 (OR e2 ...)))))

(define-syntax AND
  (syntax-rules ()
    ((_ e) e)
    ((_ e1 e2 ...)
     (and-pair e1 (AND e2 ...)))))

;;
;; clude test code
;;
(define aaa (error-tag (regex-parser #rx"^aaa") '("expecting aaa")))
(define bbb (error-tag (regex-parser #rx"^bbb") '("expecting bbb")))
(define rep_aaa (repeat aaa))
(define test (AND (OR aaa bbb) (OR rep_aaa bbb) bbb))
(test "aaaaaaaaaaaaccc") ;; fail at 3 chars from end of the string, expecting bbb
(test "aaacccaaabbb") ;; fail at 9 chars from end of the string, expecting bbb
(test "aaaaaaaaacccbbb") ;; fail at 6 chars from end of the string, expecting bbb
(test "bbbaaaaaacccbbb") ;; fail at 6 chars from end of the string, expecting bbb
(test "aaacccaaaaaa") ;; fail at 9 chars from EOS, expecting bbb
(test "aaaaaacccaaa") ;; fail at 6 chars from EOS, expecting bbb
(test "bbbbbbbbb") ;;
((OR bbb aaa) "aaa") ;; OK
(test "aaaaaaaaabbb") ;; OK
(test "bbbaaaaaabbb") ;; OK