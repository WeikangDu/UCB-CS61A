(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s))
)

(define (caddr s)
  (car (cddr s))
)

(define (sign x)
  (cond ((> x 0) 1)
        ((= x 0) 0)
        ((< x 0) -1))
)

(define (square x) (* x x))

(define (pow b n)
  (cond ((= n 0) 1)
        ((even? n) (square(pow b (quotient n 2))))
        ((odd? n) (* b (pow b (- n 1))))
  )
)

(define (ordered? s)
  (cond ((null? (cdr s)) #t)
        ((> (car s) (cadr s)) #f)
        (else (ordered? (cdr s)))
  )
)

(define (empty? s) (null? s))

(define (add s v)
    (cond ((empty? s) (list v))
          ((= (car s) v) s)
          ((> (car s) v) (cons v s))
          ((< (car s) v) (cons (car s) (add (cdr s) v)))
    )
    )

; Sets as sorted lists
(define (contains? s v)
    (cond ((null? s) #f)
          ((> (car s) v) #f)
          ((< (car s) v) (contains? (cdr s) v))
          ((= (car s) v) #t)
    )
    )

; Equivalent Python code, for your reference:
;
; def empty(s):
;     return s is Link.empty
;
; def contains(s, v):
;     if empty(s):
;         return False
;     elif s.first > v:
;         return False
;     elif s.first == v:
;         return True
;     else:
;         return contains(s.rest, v)

(define (intersect s t)
    (cond ((or (empty? s) (empty? t)) list())
          ((= (car s) (car t)) (cons (car s) (intersect (cdr s) (cdr t))))
          ((< (car s) (car t)) (intersect (cdr s) t))
          ((> (car s) (car t)) (intersect s (cdr t)))
    )
    )

; Equivalent Python code, for your reference:
;
; def intersect(set1, set2):
;     if empty(set1) or empty(set2):
;         return Link.empty
;     else:
;         e1, e2 = set1.first, set2.first
;         if e1 == e2:
;             return Link(e1, intersect(set1.rest, set2.rest))
;         elif e1 < e2:
;             return intersect(set1.rest, set2)
;         elif e2 < e1:
;             return intersect(set1, set2.rest)

(define (union s t)
    (cond ((and (empty? s) (empty? t)) list())
          ((empty? s) t)
          ((empty? t) s)
          ((= (car s) (car t)) (cons (car s) (union (cdr s) (cdr t))))
          ((< (car s) (car t)) (cons (car s) (union (cdr s) t)))
          ((> (car s) (car t)) (cons (car t) (union s (cdr t))))

    )
    )
