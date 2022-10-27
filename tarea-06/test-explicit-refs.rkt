
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

  (test-case "newref"
             (check-equal? (run `(newref 5))
                           (result (ref-val 0) (list (num-val 5)))))
  (test-case "deref"
             (check-equal? (run `(let (x (newref 5)) (- (deref x) 7)))
                           (result (num-val -2) (list (num-val 5)))))
  
 
  )


  

(run-tests test 'verbose)