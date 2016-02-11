(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    ((interpret_start (parser filename) '(()())))))

(define M_state_statement
  (lambda (parse_tree state)
    (cond
      ((eq? (first_symbol parse_tree) 'var) (M_state_statement (M_state_init state (rest_of_statement parse_tree)) (cdr parse_tree)))
      ((eq? (first_symbol parse_tree) '=) (M_state_statement (M_state_assign state (rest_of_statement parse_tree)) (cdr parse_tree)))
      ((eq? (first_symbol parse_tree) 'return) (M_val_expression state (rest_of-statement))))
      ((eq? (first_symbol parse_tree) 'if) (M_state_statement (M_state_if state (rest_of_statement parse_tree)) (cdr parse_tree)))
      ((eq? (first_symbol parse_tree) 'while) (M_state_statement (M_state_while state (first_statement parse_tree)) (cdr parse_tree)))))

(define M_state_init
  (lambda (stmt state)
    (M_state_assign (initialize_variable state (symbol stmt)) (rest_of_statement stmt))))
   

(define first_statement car)
(define first_var caar)
(define first_var_val cadr)
(define first_symbol caar)
(define rest_of_statement cdar)
(define symbol car)
              
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