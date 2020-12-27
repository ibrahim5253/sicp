(define (poly? p)
 (if (pair? p)
   (eq? (car p) 'poly)
   false))

(define (contents p)
  (cdr p))

(define (variable c)
  (car c))

(define (term-list c)
  (cadr c))

(define (coeff term)
  (cadr term))
     
(define (order term)
  (car term))

(define (poly-y->x p) ; convert a polynomial in var 'x' to 'y'
  (define (parse-terms-for-x terms)
  (define (extract-terms term)
    (let ((c (coeff term)))
      (if (poly? c)
        (if (eq? (variable (contents c)) 'x)
          (parse-terms-for-x (term-list (contents c)))
          '())
        '())))
  (define (transform-term-list terms)
    (if (null? terms)
      '()
      (add-terms (extract-terms (car terms)) (transform-term-list (cdr terms)))))
  (let ((new-terms (transform-term-list (term-list p))))
    (make-poly 'x new-terms)))
