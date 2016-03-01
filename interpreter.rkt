; PLC Project 2
; Eric Luan
; Steven Wendling
; William Ordiway 
(load "simpleParser.scm")

; Interpret a file. 
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (M_state_statement '(()()) (parser filename) return '() '())))))

; The general M_state function. Handles return/var/=/if/while.  
(define M_state_statement
  (lambda (state parse_tree return break continue)
    (cond
      ((null? parse_tree) state)
      ((equal? (first_symbol parse_tree) 'return) (get_sanitized_result state (return_exp parse_tree)))
      ((eq? (first_symbol parse_tree) 'break) (break state))
      ((eq? (first_symbol parse_tree) 'continue) (continue state))
      ((eq? (first_symbol parse_tree) 'begin) (M_state_statement
                                               (M_state_statement
                                                state
                                                (strip_symbol parse_tree)
                                                return break continue)
                                              (next_stmt parse_tree)
                                              return break continue))
      ((eq? (first_symbol parse_tree) 'var) (M_state_statement (M_state_init state (rest_of_statement parse_tree)) (next_stmt parse_tree) return break continue))
      ((eq? (first_symbol parse_tree) '=) (M_state_statement (M_state_assign state (rest_of_statement parse_tree)) (next_stmt parse_tree) return break continue))
      ((eq? (first_symbol parse_tree) 'if) (M_state_statement (M_state_if state (rest_of_statement parse_tree) return break continue) (next_stmt parse_tree) return break continue))
      ((eq? (first_symbol parse_tree) 'while) (M_state_statement (M_state_while state (rest_of_statement parse_tree) return break continue) (next_stmt parse_tree) return break continue))
    )))

; Handles M_state of an init statement 
(define M_state_init
  (lambda (state stmt)
    (M_state_assign (initialize_variable state (symbol stmt)) stmt)))

; Handles M_state of an assign statement 
(define M_state_assign
  (lambda (state stmt)
    (if (null? (assign_exp stmt))
        (assign state (symbol stmt) '())
        (assign state (symbol stmt) (M_bool state (cadr stmt))))))

; Handles M_state of an if statement 
(define M_state_if
  (lambda (state stmt return break continue)
    (cond
      ((M_bool state (conditional stmt)) (M_state_statement state (cons (then_statement stmt) '()) return break continue))
      ((has_optional stmt) (M_state_statement state (cons (optional_statement stmt) '()) return break continue))
      (else state))))

; the statement is the car of the parse tree cleansed of the leading "while" designator
; meaning ((<bool_operator> <expression1> <expression2>) (<operation>))
(define M_state_while 
  (lambda (state statement return break continue)
    (cond
      ((null? (conditional statement)) (error "No boolean expression was defined"))
      ((number? (M_bool state (conditional statement))) (error "while statement evaluating a number instead of boolean expression. OOPS")) ; M_bool MAY return a number as part of its operation, but shouldn't unless we made a mistake on our part 
      ; do a check on break continuation, if the break was flagged we simply return the state and don't do this iteration
      ((break) state) ; the while statement encountered a break in its last iteration meaning that no future looping should occur, pass up the state
      ; this ^ state should have done all the operations up until the break and therefor is accurate to return
      ((M_bool state (conditional statement)) ;if the while boolean operation (<bool_operator> <expression1> <expression2>) is true
       (M_state_while (M_state_statement state (cons (then_statement statement) '()) return break continue) statement return break continue) return break continue) ; we need tail-recurse on a state changed by the statement
              ; statement may now consist of a code block now, e.x. (begin (= x (- x 1)) (break) (= x (+ x 100)))
              ; only operations leading up to the break should be executed, at the break the rest of the operations and while cease to matter
      (else state) ; the M_bool returned false so we don't apply the statement to the state we simply pass up the state
      )))

;########################################################
;test2/test10.txt parser output, for debugging reference
;((var x 0)
; (var y x)
; (var z y)
; (while
;  (== 1 1)
;  (begin
;    (= y (- y x))
;    (while (== 2 2) (begin (= z (- z y)) (while (== 3 3) (begin (= z (+ z 1)) (if (> z 8) (break) (continue)))) (= y (+ y 1)) (if (<= y 7) (continue) (break))))
;    (= x (+ x 1))
;    (if (> x 6) (break) (continue))))
; (return (+ (+ (* x 100) (* y 10)) z)))
;########################################################

; Handles M_state for a return 
; Sanitizes #t and #f to true/false respectively. 
(define get_sanitized_result
  (lambda (state exp)
    (sanitize (M_bool state exp))))

; Handles returning and special return for #t and #f 
(define sanitize
  (lambda (val)
    (cond
      ((eq? val #t) 'true)
      ((eq? val #f) 'false)
      (else val))))

(define strip_symbol
  (lambda (parse_tree)
    (cdar parse_tree)))

; Handles M_value. 
; Does +/-/*/"/"/%
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

; M_bool handles returning booleans. It can also evaluate mathematical expressions. 
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

; Determines whether "if" has an optional or not 
(define has_optional
  (lambda (l)
    (not (null? (cddr l)))))

; Checks between unary - vs operational - 
(define unary?
  (lambda (exp)
    (null? (cddr exp))))

(define conditional car)
(define then_statement cadr)
(define optional_statement caddr)
(define return_val car)
(define first_statement car)
(define first_symbol caar)
(define rest_of_statement cdar)
(define next_stmt cdr)
(define symbol car)
(define assign_exp cdr)
(define return_exp cadar)
(define first_part_of_bool cadr)
(define second_part_of_bool caddr)
(define first_part_of_exp car)
(define operator car)
(define operand1 cdr)
(define operand2 cddr)

; State operations below 

; Gets the value of a variable from a state 
(define get_val
  (lambda (state variable)
    (cond
      ((null? state) (error "No state was defined"))
      ((null? (variables_from_state state)) (error "Variable not declared"))
      ((and (eq? (next_var state) variable) (null? (next_val state))) (error "Variable not initialized"))
      ((eq? (next_var state) variable) (next_val state))
      (else (get_val (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable)))))

; Creates a state from a list of vars and vals 
(define create_state
  (lambda (vars vals)
    (cons vars (cons vals '()))))

; Adds a var/val pair to a state and returns the new state 
(define add_to_state
  (lambda (state var val)
    (create_state (append (variables_from_state state) (cons var ())) (append (values_from_state state) (cons val ())))))
     
; Initializes a variable in the state and returns the new state 
(define initialize_variable
  (lambda (state variable)
    (cond
      ((null? state) (error "No state was defined"))
      ((null? (variables_from_state state)) (create_state (cons variable '()) '(()) ))
      ((eq? (next_var state) variable) (error "Variable is already declared"))
      (else (add_to_state (initialize_variable (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable) (next_var state) (next_val state))))))

; Assigns a value to a variable in a state and returns the new state 
(define assign
  (lambda (state variable value)
    (cond 
      ((null? state) (error "No state wut?"))
      ((null? (variables_from_state state)) (error "Variable not declared"))
      ((eq? (next_var state) variable) (add_to_state (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable value))
      (else (add_to_state (assign (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable value) (next_var state) (next_val state))))))

(define variables_from_state car)
(define values_from_state cadr)
(define next_var caar)
(define next_val caadr) 


