;<header>
#lang r5rs
(#%require r5rs/init)
(#%provide (all-defined))

;</header>

(define initial-generation
    (list
      '(
        (procedure start ())
        )
            '(
              (procedure start
                (put-mark)
              )
            )
             
          
            '(
              (procedure start
                ((if wall? (put-mark) (step)))
              )
             )
          
            '(
              (procedure start
                ((if wall? (put-mark) (step start)))
              )
            )

            '(
              (procedure start
                ((if wall? (put-mark) (step start turn-left turn-left step turn-left turn-left)))
              )
             )

             '( 
                (procedure start
                   (turn-right (if wall? (turn-left 
                       (if wall? (turn-left (if wall? (turn-left) (step))) (step))) (step))
                           put-mark start )
                )   
                (procedure turn-right (turn-left turn-left turn-left))
            )
            '(
                (procedure start  (put-mark (if wall? (turn-left) (step)) start))
            )
            '(
                (procedure start (step step step put-mark))
            )

                '(
                  (procedure start
                    ((if mark?
                        (get-mark step start turn-180 step turn-180)
                        (put-mark)
                    ))
                   )
                  (procedure turn-180
                    (turn-left turn-left)
                  )
                  )

                '(
                  (procedure start
                    (put-mark)
                  )
                 )

                '(
                  (procedure start
                    ((if wall? (put-mark) (step)))
                  )
                 )

                '(
                  (procedure start
                    ((if wall? (put-mark) (step start)))
                  )
                 )

                '(
                  (procedure start
                    ((if wall? (put-mark) (step start turn-left turn-left step turn-left turn-left)))
                  )
                 )

                '(
                  (procedure start
                    ((if wall? (turn-left start turn-left turn-left turn-left) (go-and-return)))
                  )
                  (procedure go-and-return
                    ((if wall? (put-mark) (step go-and-return turn-left turn-left step turn-left turn-left)))
                  )
                 )

                '(
                  (procedure turn-right (turn-left turn-left turn-left))
                  (procedure start
                    ((if wall? 
                      (turn-left 
                        (if wall? 
                          (turn-left 
                            (if wall? 
                              (turn-left go-and-return turn-right) 
                              (go-and-return)) turn-right) (go-and-return)) turn-right) (go-and-return)))
                  )
                  (procedure go-and-return
                    ((if wall? (put-mark) (step go-and-return turn-left turn-left step turn-left turn-left)))
                  )
                 )

                '(
                  (procedure start (fill-maze))
                  (procedure fill-maze
                    ((if mark?
                        ()
                        ( put-mark
                          (if wall? () (step fill-maze step-back))
                          turn-left
                          (if wall? () (step fill-maze step-back))
                          turn-left
                          turn-left
                          (if wall? () (step fill-maze step-back))
                          turn-left
                        ) 
                   )))
                  (procedure step-back
                    (turn-left turn-left step turn-left turn-left)
                  )
                  )

                '(
                  (procedure start (add-mark-to-maze))
                  (procedure add-mark-to-maze
                    ((if mark?
                     (get-mark
                      (if mark?
                        (put-mark)
                        ( put-mark put-mark
                          (if wall? () (step add-mark-to-maze step-back))
                          turn-left
                          (if wall? () (step add-mark-to-maze step-back))
                          turn-left
                          turn-left
                          (if wall? () (step add-mark-to-maze step-back))
                          turn-left get-mark
                        ))
                     ) (put-mark add-mark-to-maze get-mark)
                   )))
                  (procedure step-back
                    (turn-left turn-left step turn-left turn-left)
                  )
                  )

                '(
                   (procedure start () )
                 )

                '(
                  (procedure start (go-north go))
                  (procedure go
                    ((if wall?
                        (turn-left go)
                        (step go-north go)
                    ))
                   )
                  (procedure go-north
                    ((if north? () (turn-left go-north)))
                   )
                  )

                '(
                  (procedure start (turn-north go))
                  (procedure go
                    ((if wall?
                        ()
                        (step go)
                    ))
                   )
                  (procedure turn-north
                    ((if north? () (turn-left turn-north)))
                   )
                  )

                  '(
                    (procedure start
                      (put-mark)
                    )
                   )
                
                
                
                  '(
                    (procedure start
                      ((if wall? (put-mark) (step start)))
                    )
                  )
                
                  '(
                    (procedure start
                      ((if wall? (put-mark) (step start turn-left turn-left step turn-left turn-left)))
                    )
                  )

                  '((procedure
                    start
                    (turn-right
                    (if wall?
                      (turn-left
                        (if wall?
                          (turn-left (if wall? (turn-left) (step)))
                          (step)))
                      (step))
                    put-mark
                    start))
                  (procedure
                    turn-right
                    (turn-left turn-left turn-left)))

  '(
      (procedure start (step step step put-mark))
  )

 

    ))