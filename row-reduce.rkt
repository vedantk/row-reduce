#lang racket

;; row-reduce.rkt
;;
;; Vedant Kumar <vsk@berkeley.edu>
;;
;; This row reduction algorithm attempts to bring matrices to reduced row 
;; echelon form. The implementation is interesting because it's completely
;; symbolic. Supply your matrices as vectors of vectors of symbols.

(require racket/stream)

(define (row-reduce! mat)
  (define rows (vector-length mat))
  (define columns (vector-length (vector-ref mat 0)))
  
  (define (reduce-submatrix! i j)
    (when (and (< i rows) (< j columns))
      (if (not (contains-nonzero-row? i j))
          (reduce-submatrix! i (+ j 1))
          (reduce! i j))))
  
  (define (reduce! i j)
    ;; A[i][j] -> 1
    (swap-rows! i (first-nonzero-row i j))
    (let ([pivot (mat-get! i j)])
      (when (not (eq? pivot 1))
        (scale-row! i `(/ 1 ,pivot))
        (mat-set! i j 1)))
    
    ;; A[k != i][j] -> 0.
    (for ([k (in-range rows)])
      (when (and (not (= i k))
                 (not (eq? (mat-get! k j) 0)))
        (add-multiple! i `(- ,(mat-get! k j)) k)
        (mat-set! k i 0)))
    (reduce-submatrix! (+ i 1) (+ j 1)))
  
  (define (contains-nonzero-row? i j)
    (>= (first-nonzero-row i j) 0))
  
  (define (first-nonzero-row i j)
    (if (not (eq? (mat-get! i j) 0))
        i
        (if (< i rows)
            (first-nonzero-row (+ i 1) j)
            -1)))
  
  (define (swap-rows! a b)
    (when (not (= a b))
      (let ([tmp (vector-ref mat b)])
        (vector-set! mat b (vector-ref mat a))
        (vector-set! mat a tmp))))
  
  (define (scale-row! i multiplier)
    (for ([j (in-range columns)])
      (mat-set! i j `(* ,multiplier ,(mat-get! i j)))))
  
  (define (add-multiple! row-0 multiplier row-f)
    (for ([j (in-range columns)])
      (mat-set! row-f j
                `(+ ,(mat-get! row-f j)
                    (* ,multiplier ,(mat-get! row-0 j))))))
  
  (define (mat-get! i j)
    (vector-ref (vector-ref mat i) j))
  
  (define (mat-set! i j v)
    (vector-set! (vector-ref mat i) j (persistent-simplify v)))
  
  (reduce-submatrix! 0 0))

(define op car)
(define op-lhs cadr)
(define op-rhs caddr)
(define op-args cdr)
(define op-atom? (or/c number? symbol?))

(define (simplify expr)  
  (define (simplify:+ x)
    (match (op-args x)
      [(list lhs 0) (simplify lhs)]
      [(list 0 rhs) (simplify rhs)]
      [(list val val) (simplify:* `(* 2 ,val))]
      [(list (list '+ a b) (list '- b)) (simplify a)]
      [(list (list '+ a b) (list '- a)) (simplify b)]
      [(list val (list '- k)) (simplify:- `(- ,val ,k))]
      [(list (list '- k) val) (simplify:- `(- ,val ,k))]
      [(list lhs (list '* k x))
       (if (and (number? k) (< k 0))
           (simplify:- `(- ,lhs (* ,(- k) ,x)))
           `(+ ,(simplify lhs) (* ,(simplify k) ,(simplify x))))]
      [(list lhs rhs) 
       (cond
         [(and (number? rhs) (< rhs 0)) (simplify:- `(- ,lhs ,(- rhs)))]
         [(and (number? lhs) (< lhs 0)) (simplify:- `(- ,rhs ,(- lhs)))]
         [else `(+ ,(simplify lhs) ,(simplify rhs))])]))
  
  (define (simplify:- x)
    (match (op-args x)
      [(list (list '* a b))
       (if (number? a)
           `(* ,(- a) ,(simplify b))
           `(- (* ,(simplify a) ,(simplify b))))]
      [(list val) `(- ,(simplify val))]
      [(list lhs 0) (simplify lhs)]
      [(list 0 rhs) `(- ,(simplify rhs))]
      [(list val (list '- k)) (simplify:+ `(+ ,val ,k))]
      [(list val val) 0]
      [(list lhs rhs) 
       (cond
         [(and (number? rhs) (< rhs 0)) (simplify:+ `(+ ,lhs ,(- rhs)))]
         [else `(- ,(simplify lhs) ,(simplify rhs))])]))
  
  (define (simplify:* x)        
    (match (op-args x)
      [(list lhs 0) 0]
      [(list 0 rhs) 0]
      [(list 1 rhs) (simplify rhs)]
      [(list lhs 1) (simplify lhs)]
      [(list -1 rhs) `(- ,(simplify rhs))]
      [(list lhs -1) `(- ,(simplify lhs))]
      [(list val (list '/ k val)) (simplify k)]
      [(list (list '/ k val) val) (simplify k)]
      [(list lhs rhs) 
       (if (and (number? rhs) (not (number? lhs)))
           `(* ,rhs ,(simplify lhs))
           `(* ,(simplify lhs) ,(simplify rhs)))]))
  
  (define (simplify:/ x)
    (match (op-args x)
      [(list 0 rhs) 0]
      [(list val val) 1]
      [(list (list '* k x) x) (simplify k)]
      [(list (list '* k x) k) (simplify x)]
      [(list (list '/' a b) a) `(/ 1 ,(simplify b))]
      [(list lhs rhs) `(/ ,(simplify lhs) ,(simplify rhs))]))
  
  (define (simplify-op x) 
    (match (op x)
      ['+ (simplify:+ x)]
      ['- (simplify:- x)]
      ['* (simplify:* x)]
      ['/ (simplify:/ x)]
      [else x]))
  
  (define (eval-if-possible x)
    (cond
      [(op-atom? x) x]
      [else
       (let ([result (simplify-op x)])
         (if (op-atom? result)
             result
             (let ([args (map eval-if-possible (op-args result))])
               (if (= (length args)
                      (length (filter number? args)))
                   (apply (eval (op result)) args)
                   (cons (op result) args)))))]))
  
  (eval-if-possible expr))

(define (persistent-simplify expr)
  (let* ([result (simplify expr)]
         [next-result (simplify result)])
    (if (equal? result next-result)
        result
        (persistent-simplify next-result))))

(define (format-infix expr)
  (define (wrap x)
    (cond
      [(or (op-atom? x)
           (memq (op expr) '(+ -)))
       (format-infix x)]
      [else (format "(~a)" (format-infix x))]))
  
  (cond
    [(number? expr) (number->string expr)]
    [(symbol? expr) (symbol->string expr)]
    [(= (length (op-args expr)) 1)
     (format "~a~a" (op expr) (wrap (op-lhs expr)))]
    [else
     (if (eq? (op expr) '*)
         (format "~a~a" (wrap (op-lhs expr)) (wrap (op-rhs expr)))
         (format "~a~a~a"
            (wrap (op-lhs expr)) (op expr) (wrap (op-rhs expr))))]))

(define (print-matrix mat)
  (for ([row mat])
    (printf "[")
    (for ([j (in-range (vector-length row))])
      (printf "~a" (format-infix (vector-ref row j)))
      (when (< j (- (vector-length row) 1))
        (printf ", ")))
    (printf "]~n")))

(define (test)
  (define m1
    (vector (vector 1 0 0)
            (vector 0 1 0)
            (vector 0 0 1)))
  
  (define m2
    (vector (vector 1 0 0)
            (vector 1 1 0)
            (vector 1 1 1)))
  
  (define m3
    (vector (vector 1 0 0 'x)
            (vector 1 1 0 '(+ x 1))
            (vector 2 3 1 '(/ x 2))))
  
  (define m4
    (vector (vector 'a '(+ 2 b) 'c)
            (vector 'b '(+ 4 b) 'd)))
  
  (define m5
    (vector (vector 'a '(* 3 b) '(/ c 3) 5)
            (vector '(* 3 b) '(+ a 1) 'c 7)
            (vector '(* 2 c) '(/ a c) 'b 13)))
  
  (define (test! m)
    (displayln "Before:")
    (print-matrix m)
    (displayln "After:")
    (row-reduce! m)
    (print-matrix m)
    
    (newline))
  
  (map test! (list m1 m2 m3 m4 m5)))