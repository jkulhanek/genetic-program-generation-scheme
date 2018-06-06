;<header>
#lang r5rs
(#%require r5rs/init)
(#%provide (all-defined))
(#%require "evaluator.ss" "programs.ss")
;</header>

; random 
(define (congruential-rng seed)
  (let ((a 16807 #|(expt 7 5)|#)
        (m 2147483647 #|(- (expt 2 31) 1)|#))
    (let ((m-1 (- m 1)))
      (let ((seed (+ (remainder seed m-1) 1)))
        (lambda (b)
          (let ((n (remainder (* a seed) m)))
            (set! seed n)
            (quotient (* (- n 1) b) m-1)))))))
(define random (congruential-rng 12345))
(define (set-random! new-random) (set! random new-random))

;constants
(define generation-size 600)
(define not-modified-gen-size 50)
(define empty-node-priority 2)
(define instruction-node-priority 1)
(define conditional-node-priority 1)
(define max-mutation-ops 1)
(define procedure-call-priority 2)

(define new-nodes '(
  (1 ())
  (3 (turn-left))
  (1 ((if wall? () ())))
  (1 ((if mark? () ())))
  (1 ((if north? () ())))
  (2 (put-mark))
  (2 (get-mark))
  (6 (step))  
))


(define (sample-element prgs get-probab)
  (let ()
    (define (sample-element-recursive prgs acc)
      (if (eq? prgs '()) 
        (random acc) ; sample
        (let ((prg-stat (get-probab (car prgs))))
          (let ((val (sample-element-recursive (cdr prgs) (+ acc prg-stat))))
            (if (number? val)
            ; check if current program was selected
              (if (>= val acc)
                (car (cdr (car prgs)))
                val
              )
            val)))))

    (sample-element-recursive prgs 0)
  ))

(define (sample-program prgs)
  (let ()
    (define (calculate-index val total)
      (- total (floor (/ (- (sqrt (+ 1 (* 8 val))) 1) 2)) 1)
    )
    (define (sample-element-recursive prgs index)
      (if (eq? prgs '()) 
        ; calculate backward index
        
        (calculate-index (random (/ (* index (+ index 1)) 2)) index) ; sample
        (let ((val (sample-element-recursive (cdr prgs) (+ index 1))))
          (if (and (number? val) (= val index))
          ; check if current program was selected
              (car (cdr (car prgs)))
          val))))

    (sample-element-recursive prgs 0)
  ))

(define (sample-single elements)
  (sample-element elements (lambda (x) (car x)))
)


(define (sample-generation prgs generation-size)
  (let ()
    (define (sample-generation-recursive prgs index)
      (if (eq? index 0) '()
        (cons (sample-program prgs) (sample-generation-recursive prgs (- index 1)))
    ))
    (sample-generation-recursive prgs generation-size)
  ))

(define (mutate-node node new-nodes)
  (let ((new-node (sample-single new-nodes)))
    (let ((rest (if (eq? node '()) node (if (> (random 10) 7) node (cdr node)))
        ))
    (append new-node rest)
  )))

(define (get-mutation-op prg)
  (let ()
    (define (get-prg-ops prg)
      (if (eq? '() prg) '()
        (cons (list procedure-call-priority (list (car (cdr (car prg))))) (get-prg-ops (cdr prg)))
      ))
    (let ((new-nodes (append new-nodes (get-prg-ops prg))))
      (lambda (node)
        (mutate-node node new-nodes)
      ))
  )
)

(define (get-node-mutation-priority node)
  (if (or (eq? node '()) (and (list? node) (eq? (car node) '()))) empty-node-priority
    (let ((command (car node)))
      (cond 
        ((and (list? command) (eq? (car command) 'if)) conditional-node-priority)
        ((and (list? command) (eq? (car command) 'procedure)) 0)
        (else instruction-node-priority)
    ))))


(define (get-mutation-program-len program)
  (let ()
    (define (get-mutation-program-len-i program)
      (let ((c (get-node-mutation-priority program)))
      (cond ((or (eq? program '()) (and (list? program) (eq? (car program) '()))) empty-node-priority)
      ((list? (car program))
        (cond 
          ((equal? (car (car program)) 'procedure)
            (+ c 
              (get-mutation-program-len-i (car (cddr (car program))))
              (get-mutation-program-len-i (cdr program))))

          ((equal? (car (car program)) 'if)
          (+ c
            (get-mutation-program-len-i (car (cddr (car program))))
            (get-mutation-program-len-i (car (cdddr (car program))))
            (get-mutation-program-len-i (cdr program))))
          
          (else 
            (+ (get-mutation-program-len-i (car program))
             (get-mutation-program-len-i (cdr program))))))

      (else (+ c (get-mutation-program-len-i (cdr program)))))))
   (let ((len (get-mutation-program-len-i program)))
     (if (and (not (eq? program '())) (list? program) (not (eq? (car program) '())) (list? (car program)) (eq? (car (car program)) 'procedure))
         (- len 2) len)
     )))

(define (apply-change program index change)
  (let ()
  (define (apply-change-r program index acc change)    
    (let ((c-node-cost (get-node-mutation-priority program)))
      (if (and (>= index acc) (< index (+ acc c-node-cost)))
        ; mutate
        (cons (change program) (+ acc (get-mutation-program-len program)))
        
        ; enumerate next
        (cond ((eq? '() program) (cons program (+ acc c-node-cost)))
          ((not (list? (car program))) 
            ; ordinary command
            (let ((sub-r (apply-change-r (cdr program) index (+ acc c-node-cost) change)))
              (cons (cons (car program) (car sub-r)) (cdr sub-r))
            ))

          ((eq? (car (car program)) 'if)
            ; procedure
            (let ((sub-l (apply-change-r (car (cddr (car program))) index (+ acc c-node-cost) change))) ;l branch
              (let ((sub-r (apply-change-r (car (cdddr (car program))) index (cdr sub-l) change))) ;r branch
                (let ((sub-c (apply-change-r (cdr program) index (cdr sub-r) change))) ;continuation
                    (cons (cons (list (car (car program)) (car (cdr (car program)))
                    ;lbranch
                    (car sub-l)
                    ;rbranch
                    (car sub-r))
                    ;continuation
                    (car sub-c))
                    ;cost
                    (cdr sub-c)
                  )
              ))))

          ((eq? (car (car program)) 'procedure)
            ; procedure
            (let ((sub-b (apply-change-r (car (cddr (car program))) index (+ acc c-node-cost) change))) ;branch
                (let ((sub-c (apply-change-r (cdr program) index (cdr sub-b) change))) ;continuation
                    (cons (cons (list (car (car program)) (car (cdr (car program)))
                    ;bbranch
                    (car sub-b))
                    ;continuation
                    (car sub-c))
                    ;cost
                    (cdr sub-c)
                  )
              )))
        ))
    ))
  (car (apply-change-r program index 0 change))
))

(define (get-free-procedure-name program)
  (let ()
    (define (get-free-procedure-name-r program i)
      (if (eq? program '()) i
        (get-free-procedure-name-r (cdr program)
          (if (eq? (car (cdr (car program))) i) (+ i 1) i)
        )
      )
    )

    (get-free-procedure-name-r program 1)
  )
)

(define (apply-create-new-proc prg position)
  (let ((procedure-name (get-free-procedure-name prg)))
    (let ((program (apply-change prg position
      (lambda (rest) (cons procedure-name rest)))))
      (cons (list 'procedure procedure-name '()) program))))

(define (mutate-program prg)
  (let ((mutation-ops (+ 1 (random max-mutation-ops))))
    (define (mutate-program-single program)
      (let ((program-len (- (get-mutation-program-len program) empty-node-priority)))
        (if (= (random 16) 7)
          ; add new procedure
          (apply-create-new-proc program (random program-len))
          ; modify program simple
          (apply-change program (random program-len) (get-mutation-op program))
        )))
        

    (define (mutate-program-r program ops)
      (mutate-program-single (if (eq? ops 1) program (mutate-program-r program (- ops 1))))
    )
    
    (mutate-program-r prg mutation-ops)
    
    
  ))

;(define (get-next-generation old-generation)
;  (let ()
;    (define (sample-generation-recursive prgs index)
;      (if (eq? index 0) (sample-generation prgs not-modified-gen-size)
;        (cons (mutate-program (sample-program prgs)) (sample-generation-recursive prgs (- index 1)))
;    ));
;
;    (sample-generation-recursive old-generation (- generation-size not-modified-gen-size))
;  ))
(define (stream-generation generation)
  (let ()
    (define (stream-g gen-o)
      (if (eq? gen-o '()) (stream-g generation)
        (cons (car gen-o) (lambda () (stream-g (cdr gen-o))))))
    (stream-g generation)
    ))

(define (get-next-generation old-generation)
  (let ((gs (stream-generation old-generation)))
    (define (copy-p prgs index acc)
      (if (eq? index 0) acc (cons (car (cdr (car prgs))) (copy-p ((cdr prgs)) (- index 1) acc))))

    (define (mutate-p prgs index acc)
      (if (eq? index 0) acc (cons (mutate-program (car (cdr (car prgs)))) (mutate-p ((cdr prgs)) (- index 1) acc))))
    
    (mutate-p gs (- generation-size not-modified-gen-size) (copy-p gs not-modified-gen-size '()))
))



(define (evolve-next-generation prgs pairs treshold stack_size)
  (let ((old-generation (evaluate prgs pairs treshold stack_size)))
    (display (car old-generation))
    (newline)
    (if (eq? old-generation '()) initial-generation (get-next-generation old-generation))
  ))

(define (evolve pairs treshold stack_size)
  (let ()
    (define (evolve-r gen)
      (let ((n-gen (evolve-next-generation gen pairs treshold stack_size)))
        (evolve-r n-gen)))
    (evolve-r initial-generation)))
