(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    (M_state_statement '(()()) (parser filename))))

(define M_state_statement
  (lambda (state parse_tree)
    (cond
      ((null? parse_tree) state)
      ((done? state) (return_val state))
      ((equal? (first_symbol parse_tree) 'return) (M_bool state (return_exp parse_tree)))
      ((eq? (first_symbol parse_tree) 'var) (M_state_statement (M_state_init state (rest_of_statement parse_tree)) (next_stmt parse_tree)))
      ((eq? (first_symbol parse_tree) '=) (M_state_statement (M_state_assign state (rest_of_statement parse_tree)) (next_stmt parse_tree)))
      ((eq? (first_symbol parse_tree) 'if) (M_state_statement (M_state_if state (rest_of_statement parse_tree)) (next_stmt parse_tree)))
      ((eq? (first_symbol parse_tree) 'while) (M_state_statement (M_state_while state (first_statement parse_tree)) (next_stmt parse_tree)))
    )))

(define done?
  (lambda (state)
    (null? (cdr state))))

(define get_sanitized_result
  (lambda (state exp)
    (sanitize state (M_bool state exp))))

(define sanitize
  (lambda (val)
    (cond
      ((eq? val #t) 'true)
      ((eq? val #f) 'false)
      (else val))))

(define M_state_init
  (lambda (state stmt)
    (M_state_assign (initialize_variable state (symbol stmt)) stmt)))

(define M_state_assign
  (lambda (state stmt)
    (assign state (symbol stmt) (M_bool state (assign_exp stmt)))))

(define M_state_if
  (lambda (state stmt)
    (cond
      ((M_bool state (conditional stmt)) (M_state_statement state (cons (then_statement stmt) '())))
      ((has_optional stmt) (M_state_statement state (cons (optional_statement stmt) '())))
      (else state))))

(define M_state_while
  (lambda (state stmt)
    0))
    
(define conditional car)
(define then_statement cadr)
(define optional_statement caddr)
(define has_optional
  (lambda (l)
    (not (null? (cddr l)))))

(define return_val car)
(define first_statement car)
(define first_symbol caar)
(define rest_of_statement cdar)
(define next_stmt cdr)
(define symbol car)
(define assign_exp cdr)
(define return_exp cadar)
              
(define M_val_expression
  (lambda (state exp)
    (cond
      ((null? exp) '())
      ((number? exp) exp)
      ((eq? (operator exp) '+) (+ (M_val_expression state (operand1 exp)) (M_val_expression state (operand2 exp))))
      ((eq? (operator exp) '-) (if (unary? exp)
                                   (- 0 (M_val_expression state (operand1 exp)))
                                   (- (M_val_expression state (operand1 exp)) (M_val_expression state (operand2 exp)))))
      ((eq? (operator exp) '*) (* (M_val_expression state (operand1 exp)) (M_val_expression state (operand2 exp))))
      ((eq? (operator exp) '/) (quotient (M_val_expression state (operand1 exp)) (M_val_expression state (operand2 exp))))
      ((eq? (operator exp) '%) (remainder (M_val_expression state (operand1 exp)) (M_val_expression state (operand2 exp))))
      ((list? (first_part_of_exp exp)) (M_val_expression state (first_part_of_exp exp)))
      ((number? (first_part_of_exp exp)) (first_part_of_exp exp))
      (else (get_val state (first_part_of_exp exp))))))

(define unary?
  (lambda (exp)
    (null? (cddr exp))))
; M_bool can also do mathematical expressions.
; The reason for this is because of == and !=
; Our implementation allows <bool> == <bool> or <math> == <math>
(define M_bool
  (lambda (state exp)
    (cond
      ((null? exp) '())
      ((number? exp) exp)
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((not (list? exp)) (get_val state exp))
      ((eq? (operator exp) '==) (eq? (M_bool state (first_part_of_bool exp)) (M_bool state (second_part_of_bool exp))))
      ((eq? (operator exp) '!=) (not (eq? (M_bool state (first_part_of_bool exp)) (M_bool state (second_part_of_bool exp)))))
      ((eq? (operator exp) '<) (< (M_bool state (first_part_of_bool exp)) (M_bool state (second_part_of_bool exp))))
      ((eq? (operator exp) '>) (> (M_bool state (first_part_of_bool exp)) (M_bool state (second_part_of_bool exp))))
      ((eq? (operator exp) '<=) (<= (M_bool state (first_part_of_bool exp)) (M_bool state (second_part_of_bool exp))))
      ((eq? (operator exp) '>=) (>= (M_bool state (first_part_of_bool exp)) (M_bool state (second_part_of_bool exp))))
      ((eq? (operator exp) '||) (or (M_bool state (first_part_of_bool exp)) (M_bool state (second_part_of_bool exp))))
      ((eq? (operator exp) '&&) (and (M_bool state (first_part_of_bool exp)) (M_bool state (second_part_of_bool exp))))
      ((eq? (operator exp) '!) (not (M_bool state (first_part_of_bool exp))))
      (else (M_val_expression state exp)))))

(define first_part_of_bool cadr)
(define second_part_of_bool caddr)

(define first_part_of_exp car)

(define operator car)

(define operand1 cdr)

(define operand2 cddr)

;parser gives us atom (while (<operator> <variable1> <variable2>) (statement))
;after reading that atom#1 is a while, atom #2 and #3 along with the state get fed to this method
;if the (<operator> <variable1> <variable2>) evaluates to true then we execute the statement on the state and call the while again passing in the new state
;except <variable1> and <variable2> could actually be expressions themselves (such as * x x)
;state will look like this ((<variable symbols>) (<variable values>))
(define M_State_While 
  (lambda (state condtn statement)
    ;condtn is (<operator> <variable1> <variable2>)
    (cond
    ;  (if (mboolean cond state)
    ;      (mstatewhile cond statement (mstatestatement statement state))
    ;      state))))
      )))

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
  
