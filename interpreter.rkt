; PLC Project 2
; Eric Luan
; Steven Wendling
; William Ordiway 
(load "functionParser.scm")

; interpret <filename>
; <filename> = "<path/><testname.txt>"
; runs simpleParse.scm on file to obtain parse-tree which is passed to M_state_statement
; along with error statements for continue/break/break-return and empty lists for catch/catch_body/catch-return.
; Effectively interprets and executes a very simple Java/C-ish language.
(define interpret
  (lambda (filename)
    (do_func (parse_globals (add_empty_layer ()) (parser filename)) 'main ())))

; Calls a function, returns the return value of the function.
(define do_func
  (lambda (state name param_vals)
    (call/cc
     (lambda (return)
       (M_state_statement (get_func_state state name param_vals) (get_func_body state name) return (lambda (v) (error "Continue outside of loop")) (lambda (v) (error "Break outside of loop")) (lambda (v) (error "Break or continue outside of loop")) '() '() '()))))) ; TODO: create function to do this but return state instead of val from function

; returns the body of the function
(define get_func_body
  (lambda (state name)
    (function_body (get_from_environment state name))))

;returns the initial state/environment for the function being called
(define get_func_state
  (lambda (state name param_vals)
    (add_state_layer (get_stored_state state name) (assign_func_vars (get_stored_param_names state name) (resolve_input state param_vals) new_state))));DELETE/CHANGE push_new_state

(define assign_func_vars
  (lambda (vars vals state)
    (cond
      ((and (null? vars) (null? vals)) state)
      ((or (null? vars) (null? vals)) (error "Input parameters mismatch with formal parameter count."))
      (else (assign_func_vars (cdr vars) (cdr vals) (assign (initialize state (car val)) (car var) (car val)))))))

(define get_stored_state
  (lambda (state name)
    (env_func_state (get_from_environment state name))))

(define get_stored_param_names
  (lambda (state name)
    (env_func_vars (get_from_environment state name))))

(define resolve_input
  (lambda (state vals)
    (resolve_input_cps state vals (lambda (v) v))))

(define resolve_input_cps
  (lambda (state vals return)
    (cond
      ((null? vals) (return vals))
      (else (resolve_input_cps state (cdr vals) (lambda (v) (return (cons (M_bool state (car vals)) v))))))))

; high level function that creates global environment from source code
(define parse_globals
  (lambda (state parse_tree)
    (M_state_global state parse_tree)))

; Parses global portion of source code and returns the global environment
(define M_state_global
  (lambda (state parse_tree)
    (cond
      ((null? parse_tree) state)
      ((eq? (first_symbol parse_tree) 'var) (M_state_global (M_state_init state (rest_of_statement parse_tree)) (next_stmt parse_tree)))
      ((eq? (first_symbol parse_tree) 'function) (M_state_global (M_state_funcdef state (rest_of_statement parse_tree)) (next_stmt parse_tree)))
      (else (error "Non-declarative statement outside of function.")))))

; Returns the given environment with a new function defined.
(define M_state_funcdef
  (lambda (state parse_tree)
    (set_value_in_environment (initialize_in_environment state (symbol parse_tree))
            (symbol parse_tree)
            (create_closure (function_vars parse_tree) state (function_body parse_tree)))))

(define create_closure
  (lambda (vars state body)
    (cons vars (cons state (cons body ())))))

; M_state_statement <state> <parse_tree> <return> <continue> <break> <break-return> <catch> <catch_body> <catch-return>
;<state> The state is a list of one or more pairings of variables and values where atoms of pairings signify levels of scope in increasing order, ex: '( ((a)(1)) ((x y) (3 2)) ), could signify x=3; y =2; if(x>y){a=1; ....}  
;<parse_tree> The return of simpleParse.scm on <filename>, signifying the order to evaluate logic, operations, and assignments ex: (while (== 3 3) (begin (= z (+ z 1)) (if (> z 8) (break) (continue))))
;<return> , <continue> permit ability to continue and re-evaluate loop conditional or deliver a return to the commandline
;<break> permit ability to exit the innermost loop
;<break-return> permits errors for breaks and continues that occur outside code blocks
;<catch>, <catch_body>, <catch-return> continuations for try-catch-finally statements
; The general M_state function. Handles return/var/=/if/while.
(define M_state_statement
  (lambda (state parse_tree return continue break break-return catch catch_body catch-return)
    (cond
      ((null? parse_tree) state)
      ((equal? (first_symbol parse_tree) 'return) (return (get_sanitized_result state (return_exp parse_tree))))
      ;((equal? (first_symbol parse_tree) 'return) (return state)) ; Useful code for debugging. Comment this in/commont out above line to print out the state instead of return value 
      ((eq? (first_symbol parse_tree) 'break) (break (break-return state)))
      ((eq? (first_symbol parse_tree) 'continue) (continue (break-return state)))
      ((eq? (first_symbol parse_tree) 'throw) (M_state_catch (throw_val parse_tree) state return continue break break-return catch catch_body catch-return))
      ((eq? (first_symbol parse_tree) 'begin) (M_state_statement
                                               (pop_last_state (M_state_statement
                                                (push_state empty_state state)
                                                (strip_symbol parse_tree)
                                                return continue break (lambda (v)
                                                               (if (null? (pop_last_state v))
                                                                   (error "Break or continue out of loop")       
                                                                   (break-return (pop_last_state v))
                                                               )) catch catch_body (lambda (v)
                                                               (if (null? (pop_last_state v))
                                                                   (error "Break or continue out of loop")       
                                                                   (catch-return (pop_last_state v))
                                                               ))))
                                              (next_stmt parse_tree)
                                              return continue break break-return catch catch_body catch-return)) 
      ((eq? (first_symbol parse_tree) 'var) (M_state_statement (M_state_init state (rest_of_statement parse_tree)) (next_stmt parse_tree) return continue break break-return catch catch_body catch-return))
      ((eq? (first_symbol parse_tree) '=) (M_state_statement (M_state_assign state (rest_of_statement parse_tree)) (next_stmt parse_tree) return continue break break-return catch catch_body catch-return))
      ((eq? (first_symbol parse_tree) 'if) (M_state_statement (M_state_if state (rest_of_statement parse_tree) return continue break break-return catch catch_body catch-return) (next_stmt parse_tree) return continue break break-return catch catch_body catch-return))
      ((eq? (first_symbol parse_tree) 'while) (M_state_statement (M_state_while state (rest_of_statement parse_tree) return catch catch_body catch-return) (next_stmt parse_tree) return continue break break-return catch catch_body catch-return))
      ((eq? (first_symbol parse_tree) 'try) (M_state_statement (M_state_try state (rest_of_statement parse_tree) return continue break break-return catch catch_body catch-return)
                                                               (next_stmt parse_tree)
                                                               return continue break
                                                               (lambda (v)
                                                               (if (null? (pop_last_state v))
                                                                   (error "Break or continue out of loop")       
                                                                   (break-return (pop_last_state v))
                                                               )) catch catch_body (lambda (v)
                                                               (if (null? (pop_last_state v))
                                                                   (error "Break or continue out of loop")       
                                                                   (break-return (pop_last_state v))
                                                               ))))
      ((eq? (first_symbol parse_tree) 'funcall) (M_state_statement (do_func state (func_name parse_tree) (func_input parse_tree)) return continue break break-return catch catch_body catch-return))
    )))

; Handles a "try" block of a piece of code
; Runs the try block of code, storing the new catch continuation and the respective block of code.
; When wither catch or try finish, that state is passed to the finally block
(define M_state_try
  (lambda (state stmt return continue break break-return catch catch_body catch-return)
    (M_state_finally (pop_last_state (call/cc
                                      (lambda (new_catch)
                                        (M_state_statement (push_state empty_state state) (try_block stmt) return continue break break-return (push_new_catch catch new_catch) (push_new_cb catch_body (catch_block stmt)) (lambda (v) v)))))
                     (finally_block stmt)
                     return continue break break-return catch catch_body catch-return)))

;--------- Try Abstractions ---------;
(define push_new_cb
 (lambda (bodies body)
   (cons body bodies)))

(define push_new_catch
 (lambda (catches catch)
   (cons catch catches)))

; Returns the state from a catch block
(define M_state_catch
  (lambda (val state return continue break break-return catch catch_body catch-return)
    (if (null? catch_body)
        (error "No catch for throw")
        ((this_catch catch) (catch-return (M_state_statement (create_catch_state state (this_body catch_body) val) (strip_catch_prefix (this_body catch_body)) return continue break break-return (pop_catch catch) (pop_body catch_body) catch-return))))))

; Creates a new catch state, intializing the catch variable as the value given to throw
(define create_catch_state
  (lambda (try_state catch_body val)
    (set_value_in_environment (initialize_in_environment (push_state empty_state (pop_last_state try_state)) (catch_var catch_body)) (catch_var catch_body) (M_bool try_state val))))
         
; Handles the executiong of the finally statement after try( and catch?) have run
(define M_state_finally
  (lambda (state stmt return continue break break-return catch catch_body catch-return)
    (if (null? stmt)
        state
        (pop_last_state (M_state_statement (push_state empty_state state) (strip_finally_prefix stmt) return continue break break-return catch catch_body catch-return)))))

; Handles M_state of an init statement 
(define M_state_init
  (lambda (state stmt)
    (M_state_assign (initialize_in_environment state (symbol stmt)) stmt)))

; Handles M_state of an assign statement 
(define M_state_assign
  (lambda (state stmt)
    (if (null? (assign_exp stmt))
        (set_value_in_environment state (symbol stmt) '())
        (set_value_in_environment state (symbol stmt) (M_bool state (cadr stmt))))))

; Handles M_state of an if statement 
(define M_state_if
  (lambda (state stmt return continue break break-return catch catch_body catch-return)
    (cond
      ((M_bool state (conditional stmt)) (M_state_statement state (wrap (then_statement stmt)) return continue break break-return catch catch_body catch-return))
      ((has_optional stmt) (M_state_statement state (wrap (optional_statement stmt)) return continue break break-return catch catch_body catch-return))
      (else state))))

; the statement is the car of the parse tree cleansed of the leading "while" designator
; meaning ((<bool_operator> <expression1> <expression2>) (<operation>))
(define M_state_while 
  (lambda (state statement return catch catch_body catch-return)
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
                                (M_state_statement state (wrap (then_statement statement)) return continue break (lambda (v) v) catch catch_body catch-return))) statement return catch catch_body)) ; we need recurse on a state changed by the statement
                           (else state) ; the M_bool returned false so we don't apply the statement to the state we simply pass up the state
      ))))
         (loop state statement return catch catch_body))))))

; Wraps a statement within an empty list for parsing purpsoes 
(define wrap
  (lambda (stmt)
    (cons stmt '())))

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

; Removes the symbol from a parse tree and returns the rest of the tree 
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
      ((eq? (operator exp) 'funcall) (do_func state (eval_func_name exp) (eval_func_input exp))) ; TODO verify this
      ((eq? (operator exp) '+) (+ (M_val_expression state (operand1 exp)) (M_val_expression state (operand2 exp))))
      ((eq? (operator exp) '-) (if (unary? exp)
                                   (- 0 (M_val_expression state (operand1 exp)))
                                   (- (M_val_expression state (operand1 exp)) (M_val_expression state (operand2 exp)))))
      ((eq? (operator exp) '*) (* (M_val_expression state (operand1 exp)) (M_val_expression state (operand2 exp))))
      ((eq? (operator exp) '/) (quotient (M_val_expression state (operand1 exp)) (M_val_expression state (operand2 exp))))
      ((eq? (operator exp) '%) (remainder (M_val_expression state (operand1 exp)) (M_val_expression state (operand2 exp))))
      ((list? (first_part_of_exp exp)) (M_val_expression state (first_part_of_exp exp)))
      ((number? (first_part_of_exp exp)) (first_part_of_exp exp))
      (else (get_from_environment state (first_part_of_exp exp))))))

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
      ((not (list? exp)) (get_from_environment state exp))
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

(define conditional car)           ;used to take the first atom from a statement given to M_state_if and M_state_while, which is the boolean conditional
(define then_statement cadr)       ;used to take the atom, which may be a block of code or may be a single line, following the boolean conditional
(define optional_statement caddr)  ;used in checks for and subsequent execution of operation(s), which may be a block of code or may be a single line, to be executed in the event of a false boolean conditional
;(define return_val car) ;remenant of Project1, no longer used
;(define first_statement car);remenant of Project1, no longer used
(define first_symbol caar)         ;used by M_state_statement to determine what procedure to apply to a atom of the parsetree
(define rest_of_statement cdar)    ;used by M_state_statement such that after identifying the procedure, the variables operations and values of that procedure may be passed to the appropriate M_state 
(define next_stmt cdr)             ;used by M_state_statement to interate through the remainder of the parsetree after identifying and executing the procedure at the front of the parsetree
(define symbol car)                ;used by M_state_init and M_state_assign to obtain the variable reference, ex: 'x' or 'y'
(define assign_exp cdr)            ;used in M_state_assign to signify the expression on the right of the '=' operator. If null, the variables will not hold a value.
(define return_exp cadar)          ;used to pass the numerical or boolean value from the parsetree in M_state_statement to get_sanitized_result such that "true" may be returned instead of "#t", etc...
(define first_part_of_bool cadr)   ;used by M_state_bool to obtain the first arguement in the boolean evaluation
(define second_part_of_bool caddr) ;used by M_state_bool to obtain the second arguement in the boolean evaluation
(define first_part_of_exp car)     ;used by M_state_expression to define, look up, and expand into  operator
(define operator car)              ;used by M_state_expression to define what operation should occur between operand1 & operand2
(define operand1 cdr)              ;refers to the value, or value represented by a variable or nested expression, that the operation should be applied to
(define operand2 cddr)             ;refers to the value, or value represented by a variable or nested expression, that affects operand1 by the operator
(define throw_val cdar)            ;used to extract the errorcode to throw from the parsetree should M_state_statement detect throwing is necessary
(define try_block car)             ;used by M_state_try to abstract out the try block from the statement
;(define other_stmts cdr) was used in development of Project2, no longer used
(define catch_var caadr)           ;used by create_catch_state to ____________________
(define this_body car)             ;used in M_state_catch to _________________
(define pop_body cdr)              ;used in M_state_catch to _____________
(define this_catch car)            ;used in M_state_catch to __________
(define pop_catch cdr)             ;used in M_state_catch to __________
(define catch_block cadr)          ;used in M_state_try to ____________
(define strip_catch_prefix caddr)  ;used in M_state_catch to __________
(define finally_block caddr)       ;used in M_state_try to ____________
(define strip_finally_prefix cadr) ;used in M_state_finally to _________
(define function_vars cadr)
(define env_func_vars car)
(define env_func_state cadr)
(define function_body caddr)
(define function_state_func cadr)
(define func_input cddar)
(define func_name cadar)
(define eval_func_name cdar)
(define eval_func_input cddr)

; Environment operations. An environment is a linked list of states
; Enter block
(define enter_block
  (lambda (environment)
    (cond
      ((null? environment) (error "No environment???!?!?"))
      (else (add_state_layer (rest_of_environments environment) (push_state empty (top_layer environment)))))))

; Exit block 
(define exit_block
  (lambda (environment)
    (cond
      ((null? environment?) (error "No environment"))
      (else ((add_state_layer (rest_of_environments environment) (pop_last_state (top_layer environment))))))))

; Returns the top state (really the states)
(define get_top_state
  (lambda (environment)
    ((null? (environment)) (error "Error"))
    (else (car environment))))

; Add an empty layer to the environment 
(define add_empty_layer
  (lambda (environment)
      (cons new_state environment)))

; Adds a layer to an environment
(define add_state_layer
  (lambda (environment states)
    (cons states environment)))
    
; Retrieves the environment for a function  
(define get_environment
  (lambda (environment function)
    (cond
      ((null? environment) (error "Function not found"))
      ((check_var_initialized function (top_layer environment)) environment)
      (else (get_environment (rest_of_environments environment) function)))))

; Gets a function from an environment...can also be used to get values 
(define get_from_environment
  (lambda (environment function)
    (cond
      ((null? environment) (error "Function not found"))
      ((check_var_initialized function (top_layer environment)) (get_val (top_layer environment) function))
      (else (get_from_environment (rest_of_environments environment) function)))))

; Checks if the namespace for a var is taken in the environment 
(define var_exists_in_environment?
  (lambda (environment var)
    (cond 
      ((null? environment) #f)
      ((check_var_initialized var (top_layer environment)) #t)
      (else (var_exists_in_environment? (rest_of_environments environment) var)))))
  
; Initialize a variable in the top environment 
(define initialize_in_environment
  (lambda (environment var)
    (cond
      ((null? environment) (error "No environment???"))
      ((not (var_exists_in_environment? environment var)) (add_state_layer (rest_of_environments environment) (initialize_variable (top_layer environment) var)))
      (else (error "Variable already initialized")))))

(define set_value_in_environment
  (lambda (environment var val)
    (cond
      ((null? environment) (error "Variable or function not declared"))
      ((check_var_initialized var (top_layer environment)) (add_state_layer (rest_of_environments environment) (assign (top_layer environment) var val)))
      (else (add_state_layer (set_value_in_environment (rest_of_environments environment) var val) (top_layer environment))))))
    

(define top_layer car)
(define rest_of_environments cdr)
; State operations below
; General naming convention: "states" refers to all of the layers and "state" refers to a single layer 

; Removes the top state layer and returns the rest of the states 
(define pop_last_state
  (lambda (states)
    (cond
      ((null? states) (error "No state was given"))
      (else (rest_of_states states)))))

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
      ((eq? (next_var state) variable) (unbox (next_val state)))
      (else (get_val_state (create_state (remove_first_variable (variables_from_state state)) (remove_first_value (values_from_state state))) variable)))))

; Creates a state from a list of vars and vals 
(define create_state
  (lambda (vars vals)
    (cons vars (cons vals '()))))
 
; Adds a var/val pair to  state and returns the new state 
(define add_to_state
  (lambda (state var val)
    (create_state (append (variables_from_state state) (cons var ())) (append (values_from_state state) (cons val ())))))
     
; Initializes a variable in one of the layers
(define initialize_variable_in_state
  (lambda (state variable)
    (cond
      ((null? state) (error "No state was defined"))
      ((null? (variables_from_state state)) (create_state (cons variable '()) (cons (box '()) '())))
      ((eq? (next_var state) variable) (error "Variable is already declared"))
      (else (add_to_state (initialize_variable_in_state (create_state (remove_first_variable (variables_from_state state)) (remove_first_value (values_from_state state))) variable) (next_var state) (next_val state))))))

; Initializes a variable in the first layer of all of the states 
(define initialize_variable
  (lambda (states variable)
    (if (check_var_initialized variable states)
        (error "Variable already declared")
        (push_state (initialize_variable_in_state (first_layer states) variable) (rest_of_states states)))))

; Use this to assign a value to a variable in the layer of states
; It will do it tail recursively using assign_cps 
(define assign
  (lambda (states variable value)
    (assign_cps states variable value (lambda (v) v))))

; Assigns a value to a variable in the states
(define assign_state
  (lambda (state variable value)
    (cond 
      ((null? state) (error "No state wut?"))
      ((null? (variables_from_state state)) (error "Variable not declared"))
      ((eq? (next_var state) variable) (begin (set-box! (next_val state) value) state));(add_to_state (create_state (remove_first_variable (variables_from_state state)) (remove_first_value (values_from_state state))) variable value))
      (else (add_to_state (assign_state (create_state (remove_first_variable (variables_from_state state)) (remove_first_value (values_from_state state))) variable value) (next_var state) (next_val state))))))

; Assigns a value to a variable in the appropriate state 
(define assign_cps
  (lambda (states variable value return)
    (cond
      ((null? states) (error "Variable not declared"))
      ((check_var_initialized_in_state variable (first_layer states)) (return (append_state (assign_state (first_layer states) variable value) (rest_of_states states))))
      (else (assign_cps (rest_of_states states) variable value (lambda (v)
                                                                 (return (add_layer (first_layer states) v))))))))
                                                             
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

;--------- State structure abstractions ---------;
(define rest_of_variables cdar)   ;used to obtain the the list of variables of all layers of a state except the top layer
(define rest_of_values cdadr)     ;used to obtain the the list of values of all layers of a state except the top layer
(define variables_from_state car) ;used to obtain the list of variables of a layer of a state
(define values_from_state cadr)   ;used to obtain the list of values of a layer of a state
(define next_var caar)            ;used to obtain a single variable from value list in state
(define next_val caadr)           ;used to obtain a single value from value list in state
(define new_state '((()())))    ;used in top level 'interpreter' call to initialize nested null lists for storing variables and values
(define empty_state '(()()))      ;definition of an empty state as a list containing two null lists
(define rest_of_states cdr)       ;used to grab the list of states besides the top state
(define first_layer car)          ;used to grab the top state for the list of states
(define append_state cons)        ;abstraction used to clarify operation of appending a state
(define null_variable '(()))      ;abstraction used to clarify variable status
(define add_layer cons)           ;abstraction used to clarify operation 
(define remove_first_variable cdr)
(define remove_first_value cdr)