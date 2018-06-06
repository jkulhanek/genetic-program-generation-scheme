;<header>
#lang r5rs
(#%require r5rs/init)
(#%provide (all-defined))

;</header>


(define (is-mark value)

  (if (equal? value 'w)

    #f

    (> value 0)

  )

)



(define (get-orientation state)

  (car (cdddr state))

)



(define (get-position state)

  (car (cddr state))

)



(define (get-index array index)

  (if (equal? index 0)

    (car array)

    (get-index (cdr array) (- index 1))

  )

)



(define (apply-at map index delegate)

  (cond

    ((null? map) '())

    ((equal? index 0) (cons (delegate (car map)) (cdr map)))

    (else (cons (car map) (apply-at (cdr map) (- index 1) delegate)))

  )

)



(define (apply-map map position delegate)

  (apply-at map (car (cdr position)) (lambda (inner-map)

    (apply-at inner-map (car position) delegate)                      

  ))

)



(define (apply-position-move position direction)

  (cond 

    ((equal? direction 'south)

      (cons 

        (car position)

        (cons (+ (car (cdr position)) 1)

          '()

        )

      )

    )

    ((equal? direction 'east)

    (cons 

        (+ (car position) 1)

        (cons (car (cdr position))

          '()

        )

      )

    )

    ((equal? direction 'north)

      (cons 

        (car position)

        (cons (- (car (cdr position)) 1)

          '()

        )

      )

    )

    ((equal? direction 'west)

      (cons 

        (- (car position) 1)

        (cons (car (cdr position))

          '()

        )

      )

    )

    (else position)

  )

)


(define (get-field state position)

  (get-index

      (get-index (car (cdr state)) (car (cdr position)))

      (car position)

  )

)



(define (try-step state)

  (cons (append (car state) '(step))

    (cons (car (cdr state))

      (cons 

        (apply-position-move (get-position state) (get-orientation state))

        (cdddr state)

      )

    )

  )

)



(define (step state)
  (if (validate-position (try-step state))
    (try-step state)
    (cons 'error state)
  )
)





(define (get-mark state)
  (if (is-mark (get-field state (get-position state)))
    (cons (append (car state) '(get-mark))
      (cons 
        (apply-map (car (cdr state)) (get-position state)
          (lambda (x) (- x 1))
        )
        (cddr state)
      )
    )
    (cons 'error state)
  )
)





(define (put-mark state)
  (cons (append (car state) '(put-mark))
    (cons 
      (apply-map (car (cdr state)) (get-position state)
        (lambda (x) (+ 1 x))
      )
      (cddr state)
    )
  )
)

(define (get-length array)

  (if (null? array)

    0

    (+ 1 (get-length (cdr array)))

  )

)



(define (get-width state)

  (get-length (car (car (cdr state))))

)



(define (get-height state)

  (get-length (car (cdr state)))

)



(define (validate-position state)

  (cond

    ((< (car (get-position state)) 0) #f)

    ((>= (car (get-position state)) (get-width state)) #f)

    ((< (car (cdr (get-position state))) 0) #f)

    ((>= (car (cdr (get-position state))) (get-height state)) #f)

    ((equal? 'w (get-field state (get-position state))) #f)

    (else #t)

  )

)





(define (eval-if state condition)

    (cond

        ((equal? condition 'north?)

          (equal? (get-orientation state) 'north)

        )

        ((equal? condition 'wall?)

          (not (validate-position (try-step state)))

        )

        ((equal? condition 'mark?)

          (is-mark (get-field state (get-position state)))

        )

        (else #f)

    )

)



(define (turn-left state)

  (cons (append (car state) '(turn-left))

    (cons (car (cdr state))

      (cons (car (cddr state))

        (cons (cond 

          ((equal? (get-orientation state) 'south) 'east)

          ((equal? (get-orientation state) 'east) 'north)

          ((equal? (get-orientation state) 'north) 'west)

          ((equal? (get-orientation state) 'west) 'south)

          (else (get-orientation state))

        )

        '())

      )

    )

  )

)



(define (find_procedure program name)

  (if (equal? (car (cdr (car program))) name)

      (car (cddr (car program)))

      (find_procedure (cdr program) name)

  )

)

(define (get-command-header command)

  (if (not (list? command))

    command

    (car command)

  )

)



(define (is-procedure-call command)

  (cond 

    ((equal? (get-command-header command) 'turn-left) #f)

    ((equal? (get-command-header command) 'step) #f)

    ((equal? (get-command-header command) 'put-mark) #f)

    ((equal? (get-command-header command) 'get-mark) #f)

    ((equal? (get-command-header command) 'if) #f)

    (else #t)

  )

)


(define (sim-evaluate state command program limit command-count-limit)
  (cond 
    ((equal? state 'overflow) 'overflow)
    ((eq? (car state) 'error) state)
    ; add overflow case
    ((> (length (car state)) command-count-limit) 'overflow)
      (else (if (> limit 0)

          (if (list? command)

              (cond
                ((eq? command '()) state)
                ((equal? (get-command-header command) 'if)                     

                  (if (eval-if state (car (cdr command)))

                      (sim-evaluate state (car (cddr command)) program limit command-count-limit)

                      (sim-evaluate state (car (cdddr command)) program limit command-count-limit)

                      ))

               (else (if (null? (cdr command))

                      (sim-evaluate state (car command) program limit command-count-limit)

                      (sim-evaluate (sim-evaluate state (car command) program limit command-count-limit) (cdr command) program limit command-count-limit)

                  ))

               )

              (cond

                ((equal? (get-command-header command) 'turn-left) (turn-left state))

                ((equal? (get-command-header command) 'step) (step state))

                ((equal? (get-command-header command) 'put-mark) (put-mark state))

                ((equal? (get-command-header command) 'get-mark) (get-mark state))

                (else (sim-evaluate state (find_procedure program (get-command-header command)) program (- limit 1) command-count-limit))

                )

              )

          (cons 'error state)

          )
      )
  )
)

(define (remove-error state)
  (cond 
    ((equal? state 'overflow) 'overflow)
    ((eq? (car state) 'error) (cdr state))
      (else state)
  )
)

(define (export-result result)
  (if (equal? result 'overflow) '()
    (list (car result) (cdr result))
  )
)



(define (simulate base-state command program limit command-count-limit)
  (let ((result (sim-evaluate (cons '() base-state) command program (+ limit 1) command-count-limit)))
    (export-result (remove-error result))
  )
)

















