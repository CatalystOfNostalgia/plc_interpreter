(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    ((interpret_start (parser filename) '(()())))))

(define M_state_statement
  (lambda (parse_tree state)
    (cond
      ((null? (cdr state)) (car state))
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

; Gets the value of a variable from a state 
(define get_val
  (lambda (state variable)
    (cond
      ((null? state) (error "No state was defined"))
      ((null? (variables_from_state state)) (error "Variable not declared"))
      ((and (eq? (next_var state) variable) (null? (next_val state))) (error "Variable not initialized"))
      ((eq? (next_var state) variable) (next_val state))
      (else (get_val (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable))))) 

(define variables_from_state car)
(define values_from_state cadr)

(define next_var caar)
(define next_val caadr) 
; Creates a state from a list of vars and vals 
(define create_state
  (lambda (vars vals)
    (cons vars (cons vals '()))))

(define add_to_state
  (lambda (state var val)
    (create_state (append (variables_from_state state) (cons var ())) (append (values_from_state state) (cons val ())))))
     
(define initialize_variable
  (lambda (state variable)
    (cond
      ((null? state) (error "No state was defined"))
      ((null? (variables_from_state state)) (create_state (cons variable '()) '(()) ))
      ((eq? (next_var state) variable) (error "Variable is already initialized"))
      (else (add_to_state (initialize_variable (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable) (next_var state) (next_val state))))))

(define assign
  (lambda (state variable value)
    (cond 
      ((null? state) (error "No state wut?"))
      ((null? (variables_from_state state)) (error "Variable not declared"))
      ((eq? (next_var state) variable) (add_to_state (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable value))
      (else (add_to_state (assign (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable value) (next_var state) (next_val state))))))
  
