;<header>
#lang r5rs
(#%require r5rs/init)
(#%provide (all-defined))
(#%require "simulator.ss")
;</header>

; sorting
(define (sort list compare)
  (cond
    ((eq? list '()) '())
    (else (append (sort (list< (car list) (cdr list) compare) compare)
        (cons (car list) '()) 
        (sort (list>= (car list) (cdr list) compare) compare)))))

(define (list< a b compare)
  (cond
    ((or (eq? a '()) (eq? b '())) '())
    ((compare a (car b)) (list< a (cdr b) compare))
    (else (cons (car b) (list< a (cdr b) compare)))))

(define (list>= a b compare)
  (cond
    ((or (eq? a '()) (eq? b '())) '())
    ((not (compare a (car b))) (list>= a (cdr b) compare))
    (else (cons (car b) (list>= a (cdr b) compare)))))

; end sorting

(define (get-manhattan-distance-row row1 row2)
    (cond ((eq? row1 '()) 0)
        ((eq? 'w (car row1)) (get-manhattan-distance-row (cdr row1) (cdr row2)))
        (else (+ (abs (- (car row1) (car row2))) (get-manhattan-distance-row (cdr row1) (cdr row2))))))

(define (get-manhattan-distance board1 board2) 
    (cond ((eq? board1 '()) 0)
        (else (+ (get-manhattan-distance-row (car board1) (car board2)) (get-manhattan-distance (cdr board1) (cdr board2))))))

(define (get-position-distance position1 position2)
    (+ (if (equal? (car (cdr position1)) (car (cdr position2))) 0 1)
        (abs (- (car (car position1)) (car (car position2))))
        (abs (- (car (cdr (car position1))) (car (cdr (car position2)))))
    ))

(define (get-program-length program)
    (cond ((eq? program '()) 0)
        ((equal? (car program) 'procedure) (+ 1 (get-program-length (cddr program))))
        ((equal? (car program) 'if) (+ 1 (get-program-length (cddr program))))
        ((list? (car program)) (+ (get-program-length (car program)) (get-program-length (cdr program))))
        (else (+ 1 (get-program-length (cdr program))))))


(define (sum-limits l1 l2)
    (if (eq? l1 '())
        '()
        (cons (+ (car l1) (car l2)) (sum-limits (cdr l1) (cdr l2)))
    )
)


(define (simulate-single program pair stack_size command-limit)
    (let ((result (simulate (car pair) 'start program stack_size command-limit)) (compare (car (cdr pair))))
        (if (eq? result '()) `(0 0 ,(+ command-limit 1)) (list
            (get-manhattan-distance (car (car (cdr result))) (car compare))
            (get-position-distance (cdr (car (cdr result))) (cdr compare))
            (length (car result))
        ))))

(define (treshold-in-limit? current_state treshold)
    (if (eq? current_state '()) 
        #t
        (and (<= (car current_state) (car treshold)) (treshold-in-limit? (cdr current_state) (cdr treshold)))
    )
)


(define (evaluate-program program pairs treshold current_state stack_size)
    (define (get-max-program-length) (car (cddr treshold)))
    (cond 
        ((not (treshold-in-limit? current_state treshold)) '())
        ((eq? pairs '()) current_state)        
        (else (let ((new-limit (sum-limits current_state
            (simulate-single program (car pairs) stack_size (get-max-program-length)))))
            (if (treshold-in-limit? new-limit treshold)
                (evaluate-program program (cdr pairs) treshold new-limit stack_size)
                '() ;exceeded limit
            )))))

(define (insert-at list index value)
    (cond 
        ((eq? list '()) '())
        ((eq? 0 index) (cons value list))
        (else (cons (car list) (insert-at (cdr list) (- index 1) value)))
    )
)

(define (compare-tresholds t1 t2)
    (cond
        ((eq? t1 '()) #f)
        ((eq? (car t1) (car t2)) (compare-tresholds (cdr t1) (cdr t2)))
        (else (< (car t1) (car t2)))
    )
)

(define (compare-results r1 r2)
    (compare-tresholds (car r1) (car r2)))

(define (evaluate-helper prgs pairs treshold stack_size)
    (let (
        (program-length-treshold (car (cddr treshold)))
        (inner-program-treshold (list (car treshold) (car (cdr treshold)) (car (cdddr treshold))))
    )
    (cond ((eq? prgs '()) '())
        (else (let ((program (car prgs)))
            (let ((program-length (get-program-length program)))
                (if (> program-length program-length-treshold) 
                    (evaluate-helper (cdr prgs) pairs treshold stack_size)
                    (let ((eval-result (evaluate-program program pairs inner-program-treshold '(0 0 0) stack_size)))
                        (if (eq? eval-result '()) 
                            (evaluate-helper (cdr prgs) pairs treshold stack_size)
                            (cons 
                                (list (insert-at eval-result 2 program-length) program) 
                                (evaluate-helper (cdr prgs) pairs treshold stack_size))
                        )
                    )
                )
            )
        ))
    )))

(define (evaluate prgs pairs treshold stack_size)
   (sort (evaluate-helper prgs pairs treshold stack_size) compare-results))

