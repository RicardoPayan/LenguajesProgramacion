
#lang racket

(require rackunit
         rackunit/text-ui
         "explicit-refs.rkt")

(define-test-suite test
  (test-case "const-exp"
             (check-equal? (run `7)
                           (result (num-val 7) '())))
  (test-case "diff-exp"
             (check-equal? (run `(- 10 5))
                           (result (num-val 5) '())))
  (test-case "let"
             (check-equal? (run `(let [x 5] (- x 2)))
                           (result (num-val 3) '())))
  ()
  )


  

(run-tests test 'verbose)