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
       (M_state_statement new_state (parser filename) return (lambda (v) (error "Continue outside of loop")) (lambda (v) (error "Break outside of loop")) (lambda (v) (error "Break or continue outside of loop")) default_catch 'none)))))

;need to coordinate with 
(define default_catch
  (lambda (val)
    (error val)))

; The general M_state function. Handles return/var/=/if/while.  
(define M_state_statement
  (lambda (state parse_tree return continue break break-return catch catch_body)
    (cond
      ((null? parse_tree) state)
      ((equal? (first_symbol parse_tree) 'return) (return (get_sanitized_result state (return_exp parse_tree))))
      ;((equal? (first_symbol parse_tree) 'return) (return state))
      ((eq? (first_symbol parse_tree) 'break) (break (break-return state)))
      ((eq? (first_symbol parse_tree) 'continue) (continue (break-return state)))
      ((eq? (first_symbol parse_tree) 'throw) (M_state_catch (throw_val parse_tree) state return break catch catch_body))
      ((eq? (first_symbol parse_tree) 'begin) (M_state_statement
                                               (pop_last_state (M_state_statement
                                                (push_state empty_state state)
                                                (strip_symbol parse_tree)
                                                return continue break (lambda (v)
                                                               (if (null? (pop_last_state v))
                                                                   (error "Break or continue out of loop")       
                                                                   (break-return (pop_last_state v))
                                                               )) catch catch_body))
                                              (next_stmt parse_tree)
                                              return continue break break-return catch catch_body)) 
      ((eq? (first_symbol parse_tree) 'var) (M_state_statement (M_state_init state (rest_of_statement parse_tree)) (next_stmt parse_tree) return continue break break-return catch catch_body))
      ((eq? (first_symbol parse_tree) '=) (M_state_statement (M_state_assign state (rest_of_statement parse_tree)) (next_stmt parse_tree) return continue break break-return catch catch_body))
      ((eq? (first_symbol parse_tree) 'if) (M_state_statement (M_state_if state (rest_of_statement parse_tree) return continue break break-return catch catch_body) (next_stmt parse_tree) return continue break break-return catch catch_body))
      ((eq? (first_symbol parse_tree) 'while) (M_state_statement (M_state_while state (rest_of_statement parse_tree) return catch catch_body) (next_stmt parse_tree) return continue break break-return catch catch_body))
      ((eq? (first_symbol parse_tree) 'try) (M_state_statement (M_state_try state (rest_of_statement parse_tree) return break catch catch_body) (next_stmt parse_tree) return continue break break-return catch catch_body))
    )))

;TODO have try push/pop a state, the pushed state needs to be given to M_state_statement, needs to pop on the return of M_state_statement
;     also needs to pop a state when throw is called, maybe create an M_state_throw to abstract some out of M_state_statement
(define M_state_try
  (lambda (state stmt return break catch catch_body)
    (M_state_finally (call/cc
                      (lambda (new_catch)
                        (M_state_statement state (try_block stmt) return break new_catch (catch_block stmt))))
                     (finally_block stmt)
                     return break catch catch_body)))


;TODO, needs to separate out (catch (e), assign e, etc
;TODO have catch push/pop a state
(define M_state_catch
  (lambda (val state stmt return break catch catch_body)
    0))

; Handles the executiong of the finally statement after try( and catch?) have run
;TODO make finally push/pop a state
(define M_state_finally
  (lambda (state stmt return break catch catch_body)
    (M_state_statement state stmt return break catch catch_body)))

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
  (lambda (state stmt return continue break break-return catch catch_body)
    (cond
      ((M_bool state (conditional stmt)) (M_state_statement state (cons (then_statement stmt) '()) return continue break break-return catch catch_body))
      ((has_optional stmt) (M_state_statement state (cons (optional_statement stmt) '()) return continue break break-return catch catch_body))
      (else state))))

; the statement is the car of the parse tree cleansed of the leading "while" designator
; meaning ((<bool_operator> <expression1> <expression2>) (<operation>))
(define M_state_while 
  (lambda (state statement return catch catch_body)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (state statement return catch catch_body) 
                         (cond
                           ((null? (conditional statement)) (error "No boolean expression was defined"))
                           ((number? (M_bool state (conditional statement))) (error "While condition evaluted to a number")) ; M_bool MAY return a number as part of its operation, but shouldn't unless we made a mistake on our part 
                           ((M_bool state (conditional statement)) ;if the while boolean operation (<bool_operator> <expression1> <expression2>) is true
                            (loop
                             (call/cc
                              (lambda (continue)
                                (M_state_statement state (cons (then_statement statement) '()) return continue break (lambda (v) v) catch catch_body))) statement return catch catch_body)) ; we need recurse on a state changed by the statement
                           (else state) ; the M_bool returned false so we don't apply the statement to the state we simply pass up the state
      ))))
         (loop state statement return catch catch_body))))))
         

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
(define throw_val cdar)
(define try_block car)
(define other_stmts cdr)
(define catch_var caadar)
(define catch_block cadr)
(define finally_block
  (lambda (stmt)
    (car (cdaddr stmt))))
    

; State operations below
; General naming convention: "states" refers to all of the layers "state" refers to a single layer 

; Removes the top state layer and returns the rest of the states 
(define pop_last_state
  (lambda (states)
    (cond
      ((null? states) (error "No state was given"))
      (else (rest_of_states states)))))

; Add a new state layer with this list of variables and list of values 
(define push_new_state
  (lambda (vars vals states)
    (cons (create_state vars vals) states)))

; Adds an existing state as the next layer on the states
(define push_state
  (lambda (state states)
    (cons state states)))

; Get val takes the list of states and finds the variable in it 
(define get_val
  (lambda (states variable)
    (cond
      ((null? states) (error "Variable not declared"))
      ((check_var_initialized_in_state variable (first_layer states)) (get_val_state (first_layer states) variable))
      (else (get_val (rest_of_states states) variable)))))


; Gets the value of a variable from a state state 
(define get_val_state
  (lambda (state variable)
    (cond
      ((null? state) (error "Variable not declared"))
      ((null? (variables_from_state state)) (error "Variable not declared"))
      ((and (eq? (next_var state) variable) (null? (next_val state))) (error "Variable not initialized"))
      ((eq? (next_var state) variable) (next_val state))
      (else (get_val_state (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable)))))

; Creates a state from a list of vars and vals 
(define create_state
  (lambda (vars vals)
    (cons vars (cons vals '()))))
 
; Adds a var/val pair to  state and returns the new state 
(define add_to_state
  (lambda (state var val)
    (create_state (append (variables_from_state state) (cons var ())) (append (values_from_state state) (cons val ())))))
     
; Initializes a variable in one of the layer 
(define initialize_variable_in_state
  (lambda (state variable)
    (cond
      ((null? state) (error "No state was defined"))
      ((null? (variables_from_state state)) (create_state (cons variable '()) '(()) ))
      ((eq? (next_var state) variable) (error "Variable is already declared"))
      (else (add_to_state (initialize_variable_in_state (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable) (next_var state) (next_val state))))))

; Initializes a variable in the first layer of all of the states 
(define initialize_variable
  (lambda (states variable)
    (if (check_var_initialized variable states)
        (error "Variable already declared")
        (push_state (initialize_variable_in_state (first_layer states) variable) (rest_of_states states)))))

; Assign
(define assign
  (lambda (states variable value)
    (assign_cps states variable value (lambda (v) v))))

; Assigns a value to a variable in the states
(define assign_state
  (lambda (state variable value)
    (cond 
      ((null? state) (error "No state wut?"))
      ((null? (variables_from_state state)) (error "Variable not declared"))
      ((eq? (next_var state) variable) (add_to_state (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable value))
      (else (add_to_state (assign_state (create_state (cdr (variables_from_state state)) (cdr (values_from_state state))) variable value) (next_var state) (next_val state))))))

; Assigns a value to a variable in the appropriate state 
(define assign_cps
  (lambda (states variable value return)
    (cond
      ((null? states) (error "Variable not declared"))
      ((check_var_initialized_in_state variable (first_layer states)) (return (cons (assign_state (first_layer states) variable value) (rest_of_states states))))
      (else (assign_cps (rest_of_states states) variable value (lambda (v)
                                                                 (return (cons (first_layer states) v))))))))
                                                             
; Returns true if the variable has already been initialized in any layer, otherwise false. 
(define check_var_initialized
  (lambda (var states)
    (cond
      ((null? states) #f)
      ((null? rest_of_states) (check_var_initialized_in_state var (first_layer states)))
      (else (or (check_var_initialized var (rest_of_states states)) (check_var_initialized_in_state var (first_layer states)))))))

; Returns true if the variable is initialized in this state 
(define check_var_initialized_in_state
  (lambda (var state)
    (cond
      ((null? (variables_from_state state)) #f)
      ((eq? (next_var state) var) #t)
      (else (check_var_initialized_in_state var (remove_first_from_state state))))))
                                            
; Remove the first var/val pair from a state and returns that state                                                                              
(define remove_first_from_state
  (lambda (state)
    (cons (rest_of_variables state) (cons (rest_of_values state) '()))))

(define rest_of_variables cdar)
(define rest_of_values cdadr)
(define variables_from_state car)
(define values_from_state cadr)
(define next_var caar)
(define next_val caadr)
(define new_state '((()(()))))
(define empty_state '(()()))
(define rest_of_states cdr)
(define first_layer car)