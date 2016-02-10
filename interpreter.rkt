(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    ((interpret_start (parser filename)))))

(define mvalexp
  (lambda (exp)
    (cond
      ((number? exp) exp)
      ((eq? (operator exp) '+) (+ (mvalexp (operand1 exp)) (mvalexp (operand2 exp))))
      ((eq? (operator exp) '-) (- (mvalexp (operand1 exp)) (mvalexp (operand2 exp))))
      ((eq? (operator exp) '*) (* (mvalexp (operand1 exp)) (mvalexp (operand2 exp))))
      ((eq? (operator exp) '/) (quotient (mvalexp (operand1 exp)) (mvalexp (operand2 exp))))
      ((eq? (operator exp) '%) (remainder (mvalexp (operand1 exp)) (mvalexp (operand2 exp))))
      ((list? (car exp)) (mvalexp (car exp)))
      ((number? (car exp)) (car exp))
      (else (error "bad stuff")))))

(define operator car)

(define operand1 cdr)

(define operand2 cddr)

(define mstatewhile
  (lambda (cond statement state)
    (cond
      (if (mboolean cond state)
          (mstatewhile cond statement (mstatestatement statement state))
          state))))