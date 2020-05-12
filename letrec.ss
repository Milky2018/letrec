(import (datatype (1))
        (report (1)))

(define-datatype program
  [a-program expression])

(define-datatype expval 
  [num-val number]
  [bool-val bool]
  [proc-val proc])

(define-datatype proc 
  [procedure identifier expression environment])

;; expval->num : expval -> int
(define expval->num 
  (lambda (val) 
    (expval-case val 
      [num-val (num) num]
      [else (report-expval-extractor-error 'num val)])))

;; expval->bool : expval -> bool
(define expval->bool 
  (lambda (val)
    (expval-case val 
      [bool-val (bool) bool]
      [else (report-expval-extractor-error 'bool val)])))

;; expval->bool : expval -> bool
(define expval->proc
  (lambda (val)
    (expval-case val 
      [proc-val (proc) proc]
      [else (report-expval-extractor-error 'proc val)])))

(define-datatype expression
  [const-exp number]
  [diff-exp exp1 exp2]
  [zero?-exp exp]
  [if-exp exp1 exp2 exp3]
  [var-exp identifier]
  [let-exp identifier exp1 exp2]
  [proc-exp var body]
  [call-exp rator rand]
  [letrec-exp p-name b-var p-body letrec-body])

(define-datatype environment 
  [empty-env]
  [extend-env identifier expval environment]
  [extend-env-rec p-name b-var body env])

; apply-procedure : proc x expval -> expval
(define apply-procedure 
  (lambda (proc val)
    (proc-case proc 
      [procedure (var body saved-env)
        (value-of body (environment-extend-env var val saved-env))])))

;; apply-env : environment x var -> expval 
(define apply-env 
  (lambda (env search-var)
    (environment-case env 
      [empty-env () 
        (report-no-binding-found search-var)]
      [extend-env (saved-var saved-val saved-env)
        (if (eqv? saved-var search-var)
          saved-val 
          (apply-env saved-env search-var))]
      [extend-env-rec (p-name b-var p-body saved-env)
        (if (eqv? search-var p-name)
          (expval-proc-val (proc-procedure b-var p-body env))
          (apply-env saved-env search-var))])))

;; value-of-program : program -> expval
(define value-of-program
  (lambda (pgm)
    (program-case pgm 
      [a-program (expr) (value-of expr (environment-empty-env))])))

;; value-of : expression x env -> expval
(define value-of
  (lambda (expr env)
    (expression-case expr 
      [const-exp (num) (expval-num-val num)]
      [var-exp (var) (apply-env env var)]
      [diff-exp (exp1 exp2) 
        (let ([val1 (value-of exp1 env)]
              [val2 (value-of exp2 env)])
          (let ([num1 (expval->num val1)]
                [num2 (expval->num val2)])
            (expval-num-val
              (- num1 num2))))]
      [zero?-exp (exp1)
        (let ([val (value-of exp1 env)])
          (let ([num (expval->num val)])
            (if (zero? num)
              (expval-bool-val #t)
              (expval-bool-val #f))))]
      [if-exp (exp1 exp2 exp3)
        (let ([val (value-of exp1 env)])
          (if (expval->bool val)
            (value-of exp2 env)
            (value-of exp3 env)))]
      [let-exp (var exp1 body)
        (let ([val (value-of exp1 env)])
          (value-of body 
            (environment-extend-env var val env)))]
      [proc-exp (var body)
        (expval-proc-val (proc-procedure var body env))]
      [call-exp (rator rand)
        (let ([proc (expval->proc (value-of rator env))]
              [arg (value-of rand env)])
          (apply-procedure proc arg))]
      [letrec-exp (p-name b-var p-body letrec-body)
        (value-of 
          letrec-body
          (environment-extend-env-rec p-name b-var p-body env))])))

;; show-result : expval -> IO
(define show-result
  (lambda (val)
    (display (expval-case val 
      [num-val (num) num]
      [bool-val (bool) bool]
      [proc-val (proc) "procedure"]))))
