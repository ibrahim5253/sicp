(define (integers-starting-from n)
 (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (mul-streams a b)
 (cons-stream (* (stream-car a) (stream-car b))
              (mul-streams (stream-cdr a) (stream-cdr b))))

(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

(define (add-streams a b)
 (cons-stream (+ (stream-car a) (stream-car b))
              (add-streams (stream-cdr a) (stream-cdr b))))

(define s (cons-stream 1 (add-streams s s)))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s) (stream-cdr s))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

; Exercise 3.59
(define (integrate-series s)
  (let ((inverse-ints (stream-map (lambda (x) (/ 1 x)) integers)))
    (mul-streams s inverse-ints)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; Exercise 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                                         (scale-stream (stream-cdr s1) (stream-car s2)))
                            (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))
;
;(define foo (add-streams (mul-series sine-series sine-series)
;                         (mul-series cosine-series cosine-series)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define pairs-int (pairs integers integers))

; 3.67
(define (all-pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave (stream-map (lambda (x) (list (stream-car s) x))
                              (stream-cdr t))
                  (stream-map (lambda (x) (list x (stream-car t)))
                              (stream-cdr s)))
      (all-pairs (stream-cdr s) (stream-cdr t)))))

(define all-pairs-int (all-pairs integers integers))

(define (pairs-1 s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs-1 (stream-cdr s) (stream-cdr t))))

; 3.69
(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
                    (stream-cdr u))
        (stream-map (lambda (x) (cons (stream-car s) x))
                    (pairs (stream-cdr t) (stream-cdr u))))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define py-triples (stream-filter (lambda (t)
                                    (let ((i (car t))
                                          (j (cadr t))
                                          (k (caddr t)))
                                      (= (+ (* i i) (* j j)) (* k k))))
                                  (triples integers integers integers)))

; 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2))
               (w1 (weight (stream-car s1)))
               (w2 (weight (stream-car s2))))
           (if (< w1 w2)
               (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
               (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
     weight)))

(define ordered-int-pairs
  (weighted-pairs integers integers
                  (lambda (p) (+ (car p) (cadr p)))))

(define weird-pairs
  (let ((f (lambda (x) (not (or (= (remainder x 2) 0)
                                (= (remainder x 3) 0)
                                (= (remainder x 5) 0))))))
    (weighted-pairs (stream-filter f integers)
                    (stream-filter f integers)
                    (lambda (p) (let ((i (car p))
                                      (j (cadr p)))
                                  (+ (* 2 i) (* 3 j) (* 5 (* i j))))))))

; 3.71
(define (foo s weight)
  (let ((a (stream-car s))
        (b (stream-car (stream-cdr s))))
    (if (= (weight a) (weight b))
        (cons-stream (list (weight a) a b)
                     (foo (stream-cdr s) weight))
        (foo (stream-cdr s) weight))))

(define ramanujan
  (let ((weight (lambda (p) (let ((i (car p))
                                  (j (cadr p)))
                              (+ (* i i i)
                                 (* j j j))))))
    (let ((s (weighted-pairs integers integers weight)))
      (foo s weight))))

; 3.72
(define (bar s weight)
  (let ((a (stream-car s))
        (b (stream-car (stream-cdr s)))
        (c (stream-car (stream-cdr (stream-cdr s)))))
    (if (and (= (weight a) (weight b))
             (= (weight b) (weight c)))
        (cons-stream (list (weight a) a b c)
                     (bar (stream-cdr s) weight))
        (bar (stream-cdr s) weight))))

(define three-sum-of-squares
  (let ((weight (lambda (p) (let ((i (car p))
                                  (j (cadr p)))
                              (+ (* i i)
                                 (* j j))))))
    (let ((s (weighted-pairs integers integers weight)))
      (bar s weight))))
