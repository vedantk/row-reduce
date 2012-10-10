#lang racket

;; row-reduce.rkt
;;
;; Vedant Kumar <vsk@berkeley.edu>
;;
;; This row reduction algorithm attempts to bring matrices to reduced row 
;; echelon form. The implementation is interesting because it's completely
;; symbolic. Supply your matrices as vectors of vectors of symbols.

(require racket/stream)
(require racket/pretty)

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

(define (simplify expr)
  (define op car)
  (define op-args cdr)
  (define op-lhs cadr)
  (define op-rhs caddr)
  
  (define (simplify:+ x)
    (match (op-args x)
      [(list lhs 0) (simplify lhs)]
      [(list 0 rhs) (simplify rhs)]
      [(list val val) (simplify:* `(* 2 ,val))]
      [(list (list '+ a b) (list '- b)) (simplify a)]
      [(list (list '+ a b) (list '- a)) (simplify b)]
      [(list lhs rhs) `(+ ,(simplify lhs) ,(simplify rhs))]))
  
  (define (simplify:- x)
    (match (op-args x)
      [(list val) `(- ,(simplify val))]
      [(list lhs 0) (simplify lhs)]
      [(list 0 rhs) `(- ,(simplify rhs))]
      [(list val val) 0]
      [(list lhs rhs) `(- ,(simplify lhs) ,(simplify rhs))]))
  
  (define (simplify:* x)
    (match (op-args x)
      [(list lhs 0) 0]
      [(list 0 rhs) 0]
      [(list 1 rhs) (simplify rhs)]
      [(list lhs 1) (simplify lhs)]
      [(list val (list '/ 1 val)) 1]
      [(list (list '/ 1 val) val) 1]
      [(list -1 rhs) `(- ,(simplify rhs))]
      [(list lhs -1) `(- ,(simplify lhs))]
      [(list lhs rhs) `(* ,(simplify lhs) ,(simplify rhs))]))
  
  (define (simplify:/ x)
    (match (op-args x)
      [(list 0 rhs) 0]
      [(list val val) 1]
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
      [(number? x) x]
      [(symbol? x) x]
      [else
       (let ([result (simplify-op x)])
         (if (or (number? result) (symbol? result))
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
    (pretty-print m)
    (displayln "After:")
    (row-reduce! m)
    (pretty-print m)
    (newline))
  
  (map test! (list m1 m2 m3 m4 m5)))

(test)
