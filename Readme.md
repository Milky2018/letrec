# Language LETREC

The language LETREC is a language represented with S-expression with only following valid forms:

+ (zero? <exp>)
+ (if <exp> <exp> <exp>)
+ (- <exp> <exp>)
+ (let <id> <exp> <exp>)
+ (letrec <id> <id> <exp> <exp>)
+ (lambda <id> <exp>)
+ (<exp> <exp>)
+ <id>
+ <num>

The example of LETREC is shown in the file example.letrec:

```
(letrec double x
  (if (zero? x)
      0
      (- (double (- x 1)) -2)) 
  (double 10))
```

This simple LETREC program is expected to give a final answer 20.