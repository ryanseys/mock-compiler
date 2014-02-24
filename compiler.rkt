#|
# Compiler for (op expr expr) written in Scheme.
# Author: Ryan Seys
|#

; PARSE takes in a list, initial register value and string.
; It outputs a string which has appended instructions
(define (PARSE val reg str)
  (cond
    ((list? val)
      ; check if the list given has a length of 3 (1 operator and 2 operands)
      (if (not (eq? 3 (length val)))
        (error "Invalid input: Must only have 3 items per arithmetic expression.")
      )
      ; check to ensure that the first member of the list is an operator
      (if (not (member (first val) '(+ - * /)))
        (error "Invalid input: operator must be first argument.")
      )
      ; the second argument of the list must be one of the operators (i.e. a number)
      (if (and (not (list? (second val))) (not (number? (second val))))
        (error "Invalid input: second argument must be list or number.")
      )
      ; the third argument of the list must also be one of the operators (i.e. a number)
      (if (and (not (list? (first (reverse val)))) (not (number? (first (reverse val)))))
        (error "Invalid input: last argument must be list or number.")
      )
      ; if all of these checks pass, continue parsing recursively
      ; we first parse the second value, then the third value and finally the
      ; first value in the list, passing the string result from one to the next.
      (PARSE (first val) (+ 2 reg) (PARSE (first (reverse val)) (+ 1 reg) (PARSE (second val) reg str)))
    )
    ((number? val) ; if its a number, store it in a register
      (string-append str (format "move ~a register-~a~n" val reg))
    )
    ((eq? val '+) ; + means addition
      (string-append str (format "add register-~a register-~a~n" (- reg 2) (- reg 1)))
    )
    ((eq? val '-) ; - means subtraction
      (string-append str (format "subtract register-~a register-~a~n" (- reg 2) (- reg 1)))
    )
    ((eq? val '*) ; * means multiplication
      (string-append str (format "times register-~a register-~a~n" (- reg 2) (- reg 1)))
    )
    ((eq? val '/) ; / means division
      (string-append str (format "divide register-~a register-~a~n" (- reg 2) (- reg 1)))
    )
    (else ; If it's none of the above, its just very wrong syntax.
      (error "Incorrect syntax. Must be in the form (op expr expr)")
    )
  )
)

(define (Compile)
  (printf (PARSE (read) 1 "")) ; begin reading and parsing the input
)

(Compile) ; begin compile
