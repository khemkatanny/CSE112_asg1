#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: mbir.scm,v 1.9 2021-01-12 11:57:59-08 - - $
;;
;; NAME
;;    mbir.scm filename.mbir
;;
;; SYNOPSIS
;;    mbir.scm - mini basic interper
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an mbir
;;    program, which is the executed.  Currently it is only printed.
;;

;; Jay Pandit (jpandit@ucsc.edu)
;; Tanisha Khemka (tkhemka@ucsc.edu)

;;given: definig constants
(define *DEBUG* #f)
(define *STDIN* (current-input-port))
(define *STDOUT* (current-output-port))
(define *STDERR* (current-error-port))
(define *ARG-LIST* (vector->list (current-command-line-arguments)))


;;given: defining Hash tables
(define *stmt-table*     (make-hash))
(define *function-table* (make-hash))
(define *var-table*      (make-hash))
(define *array-table*    (make-hash))
(define *label-table*    (make-hash))


;;given
(for-each (lambda (var) (hash-set! *var-table* (car var) (cadr var)))
   `(
        (e ,(exp 1.0))
        (eof 0.0)
        (nan ,(/ 0.0 0.0))
        (pi ,(acos -1.0))
    )
)


;;all of these are defined in the pdf
(for-each (lambda (var) 
    (hash-set! *function-table* (car var) (cadr var))) 
    `(
        (+ ,+)
        (- ,-)
        (* ,*)
        (/ ,/)
        (^ , expt)
        (< ,<)
        (> ,>)
        (=, =)
        (<= ,<=)
        (>= ,>=)
        (!= , (lambda (x y) (not (= x y))))
        (abs ,abs)
        (acos ,acos) 
        (asin ,asin) 
        (atan ,atan) 
        (ceil ,ceiling) 
        (cos ,cos)
        (exp ,exp) 
        (floor ,floor)
        (log ,log) 
        (round ,round)
        (sin ,sin)
        (sqrt ,sqrt)
        (tan ,tan)
        (trunc, truncate)
        (log10 ,(lambda (x) (/ (log x) (log 10.0)))) 
    )
) 


;;given: run file
(define *RUN-FILE*
    (let-values
        (
            (
                (dirname basename dir?)
                (split-path (find-system-path 'run-file))
            )
        )
        (path->string basename)
    )
)


;;given: if we get an error it exits
(define (die list)
    (for-each (lambda (item) (fprintf *STDERR* "~a " item)) list)
    (fprintf *STDERR* "~n")
    (when (not *DEBUG*) (exit 1))
)


;;given: for debugging
(define (dump . args)
    (when *DEBUG*
        (printf "DEBUG:")
        (for-each (lambda (arg) (printf " ~s" arg)) args)
        (printf "~n")
    )
)


;;given: exits with error msg
(define (usage-exit)
    (die `("Usage: " ,*RUN-FILE* " [-d] filename"))
)


;;given: Gets the line number
(define (line-number line)
    (car line)
)


;;given: Gets the label for the line
(define (line-label line)
    (let ((tail (cdr line)))
         (and (not (null? tail))
              (symbol? (car tail))
              (car tail))
    )
)


;;given: line statement
(define (line-stmt line)
    (let ((tail (cdr line)))
        (cond ((null? tail) #f)
              ((pair? (car tail)) (car tail))
              ((null? (cdr tail)) #f)
              (else (cadr tail))
        )
    )
)


;;given
(define (not-implemented function args . nl)
    (printf "(NOT-IMPLEMENTED: ~s ~s)" function args)
    (when (not (null? nl)) (printf "~n"))
)


;;Evauating the expression
;;used from Professor's website from evalexpr.scm file    ;;;FULLY DONE
(define (eval-expr expr) 
    ;;if expr is a number, we return a number
    (cond ((number? expr) (+ expr 0.0)) 
        ;;if expr is a symbol, we return symbol in variable table 
        ((symbol? expr) (hash-ref *var-table* expr 0.0))   
        ((and (pair? expr) (eq? `asub (car expr))) 
            (vector-ref (hash-ref *array-table* (cadr expr) #f) 
                (exact-round (eval-expr 
                    (cadr (cdr expr)))
                )
            )
        )
           
        ((pair? expr)
            (let ((func (hash-ref *function-table* (car expr) #f))
                    (opnds (map eval-expr (cdr expr))))
                    (if (not func) nan            
                       (apply func opnds)
                    )
            )
        )
        (else nan)
    )
)


;;default error for eval-expr    ;;;FULLY DONE
(define nan (/ 0.0 0.0))


;;creating a vector and adding to array table
;;Creates a vector and initialises an array
(define (interp-dim args continuation)   ;;;FULLY DONE
    (let ((variable (caddar args))
          (expression (cadar args))
         )

        (cond ((number? variable)
                (hash-set! *array-table* expression 
                    (make-vector (exact-round variable) 0.0)
                ))
              (else
                (hash-set! *array-table* expression 
                    (make-vector (exact-round 
                        (eval-expr variable)) 0.0
                    )
                ))
        )
    )
    (interp-program continuation)
)


;;let function -> assignment statement
;; = used to define temporary variables
(define (interp-let args continuation)          ;;;FULLY DONE
        ;;if expr1 is a pair, add the value to array table
        (cond ((pair? (car args))   
                    (vector-set! (hash-ref *array-table* (cadar args)) 
                        (exact-round (eval-expr (caddar args))) 
                            (eval-expr (cadr args))
                    )
              )
            ((symbol? (car args)) 
            (hash-set! *var-table* (car args) 
                ;;if expr1 is a symbol, add the value to variable table
                (eval-expr (cadr args))
            )) 
        )
    (interp-program continuation)
)


;;goto function -> helps with jumping from point to point
(define (interp-goto args continuation) ;;FULLY DONE
    ;;checking if label exists 
    (if (not(hash-ref *label-table* (car args)))
        ;;print error
        (printf "~a~n" "ERROR: Label does not exist")
        ;;otherwise, interpret
        (interp-program (hash-ref *label-table* (car args)))    
    )
)


;;if function
;;checking to see if comparison is true 
;; -> goto fn is called
(define (interp-if args continuation) 
    (if ((hash-ref *function-table* (car (car args))) 
        (eval-expr (cadar args)) (eval-expr (caddar args)))
        (interp-program (hash-ref *label-table* (cadr args) 0.0))
        (interp-program continuation)
    )
)


;;print function
(define (interp-print args continuation)
    (define (print item) ; void print( item)
        (if (string? item) ; if (item is a string)
            (printf "~a" item) ; print(item) without quotes in it
            (printf " ~a" (eval-expr item))
        )
    ) ; else {print the item}
    (for-each print args) 
    (printf "~n")
    (interp-program continuation)
)


;;helper function(derived from readnumber.scm file on website)
(define (readnumber)
    (let ((input (read)))  ;;using read to read input from user
        (cond ((eof-object? input) (hash-set! *var-table* 
                ;;checking if input exists in the variable table
                'eof 1.0)) 
            
                ;;checking if input is a number, 
                ;;if it is convert to float
                ((number? input) (+ input 0.0))   
                (else (begin (printf "Invalid number: ~a~n" input) 
                    (readnumber))
                )  ;;else print error
        )
    ) 
)


;;reads number from input (derived from readnumber.scm file on website)
(define (interp-input args continuation)
    (define (get_input x)   ;;user input x
        (let ((number (readnumber))) 
            ;;checking to see if x is a symbol or the 
            ;;input in variable table is identical to 0.0
            (if (or (symbol? x) 
                (eq? (hash-ref *var-table* 'eof #f) 0.0))  
                ;;if it is, set x and number to variable table 
                (hash-set! *var-table* x number)   
                (nan)
            )
        )
    )
    (for-each get_input args)
    (interp-program continuation)
)


;;hash table for stmt table
(for-each (lambda (fn) (hash-set! *stmt-table* (car fn) (cadr fn)))
   `(
        (dim   ,interp-dim)
        (let   ,interp-let)
        (goto  ,interp-goto)
        (if    ,interp-if)
        (print ,interp-print)
        (input ,interp-input)
    )
)


;;given
(define (interp-program program)
    (when (not (null? program))
            (let ((line (line-stmt (car program)))
                (continuation (cdr program)))
                (if line
                   (let ((func (hash-ref *stmt-table* (car line) #f)))
                        (func (cdr line) continuation))
                   (interp-program continuation)
                )
            )
    )
)



;;helper function for scan (derived from labelhash.scm file on website)
(define (get-label line)
        (and (not (null? line))
             (not (null? (cdr line)))
             (cadr line)
        )
)


;;scan function (derived from labelhash.scm file on website)
(define (scan-for-labels program) 
    (when (not (null? program))    ;;if program is not null
        (let ((label (get-label (car program))))
            (when (symbol? label)   ;;checking if label is a symbol
                ;;if it is, add to label table
                (hash-set! *label-table* label program)
            )
        )   
        (scan-for-labels (cdr program))
    )
)


;;given
(define (print-program label program)
    (printf "~s: [~n" label)
    (for-each (lambda (line) (printf "   ~s~n" line)) program)
    (printf "]~n")
)


;;given
(define (readlist filename)
    (let ((inputfile (open-input-file filename)))
        (if (not (input-port? inputfile))
            (die `(,*RUN-FILE* ": " ,filename ": open failed"))
            (let ((program (read inputfile)))
                (close-input-port inputfile) program
            )
        )
    )
)


;;given
(define (dump-program filename program)
    (define (dump-line line)
        (dump (line-number line) (line-label line) (line-stmt line))
    )
    (dump *RUN-FILE* *DEBUG* filename)
    (dump program)
    (for-each (lambda (line) (dump-line line)) program)
)


;;given
(define (main arglist)
    (cond ((null? arglist)
                (usage-exit))
          ((string=? (car arglist) "-d")
                (set! *DEBUG* #t)
                (printf "~a: ~s~n" *RUN-FILE* *ARG-LIST*)
                (main (cdr arglist)))
          ((not (null? (cdr  arglist)))
                (usage-exit))
          (else (let* ((mbprogfile (car arglist))
                       (program (readlist mbprogfile)))
                (begin (when *DEBUG* (dump-program mbprogfile program))
                       (scan-for-labels program)
                       (interp-program program)
                ))
          )
    )
)

(main *ARG-LIST*)
