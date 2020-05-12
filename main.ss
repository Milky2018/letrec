(import (datatype (1))
        (report (1)))

(include "letrec.ss")

;; parse-and-put : list -> IO
(define parse-and-put
  (lambda (s-list)
    (show-result (value-of-program (parse-program s-list)))))

;; parse-program : list -> program
(define parse-program
  (lambda (s-list)
    (program-a-program
      (parse-expression s-list))))

;; parse-expression : s-exp -> expression
(define parse-expression
  (lambda (s-exp)
    (cond
      [(number? s-exp) (expression-const-exp s-exp)]
      [(symbol? s-exp)
         (expression-var-exp s-exp)]
      [(eqv? (car s-exp) '-)
         (expression-diff-exp 
           (parse-expression (cadr s-exp))
           (parse-expression (caddr s-exp)))]
      [(eqv? (car s-exp) 'zero?)
         (expression-zero?-exp (parse-expression (cadr s-exp)))]
      [(eqv? (car s-exp) 'if)
         (expression-if-exp (parse-expression (cadr s-exp))
                            (parse-expression (caddr s-exp))
                            (parse-expression (cadddr s-exp)))]
      [(eqv? (car s-exp) 'let)
         (expression-let-exp (cadr s-exp)
                             (parse-expression (caddr s-exp))
                             (parse-expression (cadddr s-exp)))]
      [(eqv? (car s-exp) 'lambda)
         (expression-proc-exp (cadr s-exp)
                              (parse-expression (caddr s-exp)))]
      [(eqv? (car s-exp) 'letrec)
         (expression-letrec-exp (cadr s-exp)
                                (caddr s-exp)
                                (parse-expression (cadddr s-exp))
                                (parse-expression (list-ref s-exp 4)))]
      [else 
        (expression-call-exp (parse-expression (car s-exp))
                             (parse-expression (cadr s-exp)))])))

;; driver-loop : input -> output
(define driver-loop 
  (lambda ()
    (begin
      (newline)
      (display "$ ")
      (let ([input (read)])
        (if (or 
              (eq? input 'exit)
              (eq? input #!eof))
            (exit)
            (begin 
              (parse-and-put input) 
              (driver-loop)))))))

(driver-loop)
