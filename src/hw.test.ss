(#%require r5rs/init)
(#%require schemeunit)

(#%require "hw.ss" "evaluator.ss")

(define file-tests
    (let ()
    (test-suite
        "Tests for hw3"
        (let ()
            (test-case
                "Check sample-program works"
                (let ()
                    (define prgs '(((1 1 2) a) ((0 2) b) ((3) c)))
                    (set-random! (lambda (x) 0))
                    (check-equal? (sample-program prgs) 'c)

                    (set-random! (lambda (x) 3))
                    (check-equal? (sample-program prgs) 'a)

                    (set-random! (lambda (x) 1))
                    (check-equal? (sample-program prgs) 'b)
            ))

            (test-case
                "Check sample-generation works"
                (let ()
                    (define prgs '(((4 0 0 0) a) ((0 2 0 0) b) ((3 0 0 0) c)))
                    (set-random! (lambda (x) 0))
                    (check-equal? (length (sample-generation prgs 20)) 20)))
            (test-case
                "Check mutate-node works"
                (let ()
                    (set-random! (lambda (x) (- x 1)))
                    (check-equal? (mutate-node '(get-mark) new-nodes) '(step get-mark))))
                
            (test-case
                "get-mutation-op works"
                (let ()
                    (set-random! (lambda (x) (- x 1)))
                    (check-equal? ((get-mutation-op '((procedure start ()))) '()) '(start)) 
                ))
            
            (test-case
                "Get node priority works"
                (let ()
                    (check-equal? (get-node-mutation-priority '(get-mark put-mark)) instruction-node-priority)
                    (check-equal? (get-node-mutation-priority '((if wall? '() '()) get-mark put-mark)) conditional-node-priority)
                    (check-equal? (get-node-mutation-priority '()) empty-node-priority)
                    (check-equal? (get-node-mutation-priority '((procedure start ()))) 0)
                ))
            (test-case
                "Get mutation program len works"
                (let ((a '(procedure start (if wall? () ()))))
                    ;(check-equal? (get-mutation-program-len a) 7)
                    (check-equal? (get-mutation-program-len '()) 2)
                    (check-equal? (get-mutation-program-len '(step step get-mark put-mark)) 6)
                    (check-equal? (get-mutation-program-len '(step)) 3)
                    (check-equal? (get-mutation-program-len '((if a () ()))) 7)
                    (check-equal? (get-mutation-program-len '((if a (step) (get-mark)) step)) 10)
                    (check-equal? (get-mutation-program-len '((procedure start ()))) 2)
                ))

            (test-case
                "Apply-change works"
                (let ()
                    (check-equal? (apply-change '(step step get-mark put-mark) 3
                        (lambda (x) (cons 'test x))) '(step step get-mark test put-mark) "Append in the middle")

                    (check-equal? (apply-change '(step step get-mark put-mark) 4
                        (lambda (x) (cons 'test x))) '(step step get-mark put-mark test) "Append to end")

                    (check-equal? (apply-change '() 1
                        (lambda (x) (cons 'test x))) '(test) "Append to end 2")

                    (check-equal? (apply-change '((if a (step) (get-mark)) put-mark) 0
                        (lambda (x) (cons 'test x))) '(test (if a (step) (get-mark)) put-mark) "Append before if")
                    
                    (check-equal? (apply-change '((if a (step) (get-mark)) put-mark) 1
                        (lambda (x) (cons 'test x))) '((if a (test step) (get-mark)) put-mark) "Append to if 1")

                    (check-equal? (apply-change '((if a (step) (get-mark)) put-mark) 4
                        (lambda (x) (cons 'test x))) '((if a (step) (test get-mark)) put-mark) "Append to else")
                        
                    (check-equal? (apply-change '((if a (step) (get-mark)) put-mark) 7
                        (lambda (x) (cons 'test x))) '((if a (step) (get-mark)) test put-mark) "Append after if")

                    (check-equal? (apply-change '((if a (step) (get-mark)) put-mark) 7
                        (lambda (x) (cons 'test x))) '((if a (step) (get-mark)) test put-mark) "Append after if")

                    (check-equal? (apply-change '((procedure a (step step)) (procedure b (get-mark))) 1
                        (lambda (x) (let () (display x) (cons 'test x)))) '((procedure a (step test step)) (procedure b (get-mark))) "Procedure change")

                    (check-equal? (apply-change '((procedure a (step step)) (procedure b (get-mark))) 4
                        (lambda (x) (cons 'test x))) '((procedure a (step step)) (procedure b (test get-mark))) "Second procedure change")
                ))

                (test-case
                    "Mutate program"
                    (let ()
                        (set-random! (lambda (x) 0))
                        (check-equal? (mutate-program '((procedure start (step)))) '((procedure start ())))

                        (set-random! (lambda (x) (- x 1)))
                        (check-equal? (mutate-program '((procedure start (get-mark)))) '((procedure start (get-mark start start start))))
                    ))

                (test-case
                    "Evolve single program"
                    (let ()

                     (mutate-program '((procedure start ())))
                            
                    ))

                (test-case
                    "sort generation"
                    (let ()
                        (check-equal? (sort '((0 0 2 0) (0 0 1 0)) compare-tresholds) '((0 0 1 0) (0 0 2 0)))
                            
                    ))
            
        ))))

(#%require schemeunit/text-ui)
(run-tests file-tests)
