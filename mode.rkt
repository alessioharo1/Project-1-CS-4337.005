#lang racket

; Define interactive mode based on command line arguments
(define interactive-mode?
  (let ([args (current-command-line-arguments)])
    (cond
      [(= (vector-length args) 0) #t]  ; If no arguments, run in interactive mode
      [(or (string=? (vector-ref args 0) "-b") ; If -b, run in batch mode
           (string=? (vector-ref args 0) "--batch")) #f]  ; If --batch, run in batch mode
      [else #t])))  ; Default to interactive mode if arguments don't match batch flags

; Display input prompt in interactive mode
(define (show-prompt)
  (when interactive-mode?
    (display "Enter a prefix expression: ")))

; Display a result with corresponding History ID 
(define (show-result id result) ; Parameters: ID and result
  (display id)  ; Display History ID
  (display ": ")
  (display result)  ; Display value
  (newline))

; Skip whitespace characters at the beginning of input
(define (skip-spaces chars) ; Parameters: Expression
  (cond
    [(null? chars) chars]  ; Empty: return empty
    [(char-whitespace? (car chars)) (skip-spaces (cdr chars))]  ; Whitespace: Skip
    [else chars]))  ; Return remaining characters

; Read digit from input
(define (read-number chars history) ; Parameters: History and Expression
  (let ([chars (skip-spaces chars)])  ; Skip leading spaces
    (if (and (not (null? chars)) (char-numeric? (car chars)))  ; Check if next char == digit
        (list (string->number (string (car chars))) (cdr chars))  ; Convert to number and return with remaining chars
        (error "Invalid Expression"))))  ; If not digit: Error

; Read reference to previous result
(define (read-history-ref chars history) ; Parameters: expression and history
  (let ([chars (skip-spaces (cdr chars))])  ; Skip $ and spaces
    (if (and (not (null? chars)) (char-numeric? (car chars)))  ; Check if next char is digit
        (let ([n (string->number (string (car chars)))])  ; Convert to number
          (if (or (< n 1) (> n (length history)))  ; Check if reference is valid
              (error "Invalid Expression")
              (list (list-ref history (- (length history) n)) (cdr chars))))  ; Return referenced value
        (error "Invalid Expression"))))  ; Error if reference is invalid

; Read number or history reference
(define (read-value chars history) ; Paramaters: expression and history
  (let ([chars (skip-spaces chars)])  ; Skip leading spaces
    (cond
      [(null? chars) (error "Invalid Expression")]  ; Error if empty
      [(char=? (car chars) #\$) (read-history-ref chars history)]  ; Handle $ reference
      [(char-numeric? (car chars)) (read-number chars history)]  ; Handle digit
      [else (error "Invalid Expression")])))  ; Error if neither

; Handle negative numbers (unary minus)
(define (read-negative chars history) ; Parameters: Expression and history
  (let ([chars (skip-spaces (cdr chars))])  ; Skip - and spaces
    (let ([result (read-expr chars history)])  ; Read expression to negate
      (if (null? (cdr result))  ; Check if valid expression
          (error "Invalid Expression")
          (list (- (car result)) (cadr result))))))  ; Return negated value

; Handle binary operations (+, *, /)
(define (read-operation op chars history)
  (let ([chars (skip-spaces (cdr chars))])  ; Skip operator and spaces
    (let ([first (read-expr chars history)])  ; Read first operand
      (if (null? (cdr first))  ; Check if valid
          (error "Invalid Expression")
          (let ([second (read-expr (cadr first) history)])  ; Read second operand
            (if (null? (cdr second))  ; Check if valid
                (error "Invalid Expression")
                (list (cond  ; Perform the operation
                        [(char=? op #\+) (+ (car first) (car second))]
                        [(char=? op #\*) (* (car first) (car second))]
                        [(char=? op #\/) (if (= (car second) 0) ; Return error if zero
                                           (error "Error: Division by zero") 
                                           (quotient (car first) (car second)))])
                      (cadr second))))))))  ; Return result and remaining chars

; Main expression reader - handles numbers, operations, and references
(define (read-expr chars history)
  (let ([chars (skip-spaces chars)])  ; Skip leading spaces
    (cond
      [(null? chars) (error "Invalid Expression")]  ; Error if empty
      [(char=? (car chars) #\-) (read-negative chars history)]  ; Handle negative
      [(or (char=? (car chars) #\+)  ; Handle operations
           (char=? (car chars) #\*)
           (char=? (car chars) #\/)) (read-operation (car chars) chars history)]
      [else (read-value chars history)])))  ; Handle numbers/references

; Evaluate an expression string
(define (evaluate-expr str history)
  (let ([chars (string->list str)])  ; Convert string to char list
    (let ([result (read-expr chars history)])  ; Parse expression
      (let ([remaining (skip-spaces (cadr result))])  ; Check for remaining chars
        (if (not (null? remaining))  ; If anything left, it's invalid
            (error "Invalid Expression")
            (car result))))))  ; Return the result

; Main interactive loop
(define (interactive-loop)
  (displayln "Calculator started. Press Enter to exit.")  ; Welcome message
  (let loop ([history '()])  ; Start with empty history
    (show-prompt)  ; Display prompt
    (let ([input (read-line)])  ; Read user input
      (cond
        [(or (eof-object? input) (string=? input "")) (void)]  ; Exit on empty input
        [else  ; Process input
         (with-handlers ([exn:fail? (lambda (e)  ; Error handling
                                     (displayln 
                                      (if (string-prefix? (exn-message e) "Error:")
                                          (exn-message e)
                                          "Error: Invalid Expression"))
                                     (loop history))])
           (let ([result (evaluate-expr input history)])  ; Evaluate expression
             (let ([new-history (cons result history)])  ; Add to history
               (show-result (length new-history) result)  ; Show result
               (loop new-history))))]))))  ; Continue loop with updated history

; Batch mode processing (similar to interactive but without prompts)
(define (batch-process)
  (displayln "Processing expressions in batch mode.")
  (let loop ([history '()])
    (let ([input (read-line)])  ; Read from stdin
      (cond
        [(or (eof-object? input) (string=? input "")) (void)]  ; End on empty line
        [else
         (with-handlers ([exn:fail? (lambda (e)  ; Error handling
                                     (displayln 
                                      (if (string-prefix? (exn-message e) "Error:")
                                          (exn-message e)
                                          "Error: Invalid Expression"))
                                     (loop history))])
           (let ([result (evaluate-expr input history)])  ; Evaluate
             (let ([new-history (cons result history)])  ; Update history
               (show-result (length new-history) result)  ; Show result
               (loop new-history))))]))))  ; Continue

; Start the program in the appropriate mode
(if interactive-mode?
    (interactive-loop)  ; Start interactive mode
    (batch-process))  ; Start batch mode