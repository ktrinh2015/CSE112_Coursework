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

;;CSE112 - Assignment 1
;;Spring 2021
;;Andre Cutuli - atcutuli@ucsc.edu
;;Kevin Trinh  - kevatrin@ucsc.edu

(define *DEBUG* #f)
(define *STDIN* (current-input-port))
(define *STDOUT* (current-output-port))
(define *STDERR* (current-error-port))
(define *ARG-LIST* (vector->list (current-command-line-arguments)))

(define *stmt-table*     (make-hash))
(define *function-table* (make-hash))
(define *var-table*      (make-hash))
(define *array-table*    (make-hash))
(define *label-table*    (make-hash))

(for-each (lambda (symfun) (hash-set! *function-table* (car symfun) (cadr symfun)))
   `(
        (+    ,+)
        (-    ,-)
        (*    ,*)
        (/    ,/)
        (^    ,expt)
        (sqrt ,sqrt)
        (sqr  ,sqr)
	(log  ,log)
	(= ,=)
        (< ,<)
        (> ,>)
        (!= ,(lambda (x y) (not (equal? x y))))
        (>= ,>=)
        (<= ,<=)
        (sin ,sin)
        (cos ,cos)
        (tan ,tan)
        (asin ,asin)
        (acos ,acos)
        (atan ,atan)
        (abs ,abs)
        (ceil ,ceiling)
        (floor ,floor)
        (round ,round)
        (exp ,exp)
        (truncate , truncate)
        (log10       ,(lambda (x) (/ (log x) (log 10.0))))
        ;;(asub , (lambda (x y)  (vector-ref 
        ;;(hash-ref *array-table* x) y )))  ;;GOD I HATE THIS
        ;;X = Array Name,  Y = Array Index #

))

(for-each (lambda (var) (hash-set! *var-table* (car var) (cadr var)))
   `(
        (e     , (exp 1));;(exp 1.0))
        (eof   ,0.0)
        (nan   ,(/ 0.0 0.0))
        (pi    ,(acos -1.0))
    ))

(define NAN (/ 0.0 0.0))

(define *RUN-FILE*
    (let-values
        (((dirname basename dir?)
            (split-path (find-system-path 'run-file))))
        (path->string basename)))

;;Given - for when given an error, exit program
(define (die list)
    (for-each (lambda (item) (fprintf *STDERR* "~a " item)) list)
    (fprintf *STDERR* "~n")
    (when (not *DEBUG*) (exit 1)))

(define (dump . args)
    (when *DEBUG*
        (printf "DEBUG:")
        (for-each (lambda (arg) (printf " ~s" arg)) args)
        (printf "~n")))

;; Leaves program with error statement
(define (usage-exit)
    (die `("Usage: " ,*RUN-FILE* " [-d] filename")))

(define (line-number line)
    (car line))

(define (line-label line)
    (let ((tail (cdr line)))
         (and (not (null? tail))
              (symbol? (car tail))
              (car tail))))

(define (line-stmt line)
    (let ((tail (cdr line)))
         (cond ((null? tail) #f)
               ((pair? (car tail)) (car tail))
               ((null? (cdr tail)) #f)
               (else (cadr tail)))))

;;Here Convenience
(define (not-implemented function args . nl)
    (printf "(NOT-IMPLEMENTED: ~s ~s)" function args)
    (when (not (null? nl)) (printf "~n")))


;;(if (string=? (car expr) "asub") (don't do things) (map expr to args))\

(define (eval-expr expr)
    (cond ((number? expr) (+ expr 0.0))
            ;;if its symbol, return symbol
          ((symbol? expr) (hash-ref *var-table* expr NAN)) ;;0.0))
          ;;if its pair, do this BS
          ((pair? expr)
            (if (eq? (car expr) 'asub)
                (vector-ref (hash-ref *array-table* (cadr expr))
                    (exact-round (eval-expr(caddr expr))))
                (begin
                    (let ((func (hash-ref *function-table* (car expr) #f))
                        (opnds (map eval-expr (cdr expr))))
                        (if (not func) NAN
                            (apply func opnds))))))
            (else NAN)))

;; make vector at variable table location
(define (interp-dim args continuation)
    (define temp (car args))  ;;asub a size
    ;;(display (cadr temp))(newline)
    ;;(display (caddr temp))(newline)
    (if (symbol? (cadr temp))
       (hash-set! *array-table* 
            (cadr temp) (make-vector 
                (exact-round (eval-expr (caddr temp))) 0.0))
        (exit 1)
    )  
    ;;(not-implemented 'interp-dim args 'nl)
    (interp-program continuation))
    
;;set variable value in variable hashtable
;;evaluate expression and set value of variable key to expr
;;check if array ref is in table, 
;;evaluate expr at array location


;;(printf "Array-table is ~a~n" *array-table*)

;;caddar = max
;;cadar = a

;;args = mem   continuation = expression
(define (interp-let args continuation) 
    ;;(display args)(newline)
    ;;(display args)(newline)
    ;;If the argument is a pair
    (if (pair? (car args)) ;;COndition
        (if (and (eq? (caar args) 'asub)
            (not (null? (hash-ref *array-table* (cadar args)))))
            (begin
                ;;(display "I am here")(newline)
                ;;(display "HI")(newline)
                (vector-set! (hash-ref *array-table* (cadar args))  ;;vector 
                    (exact-round (eval-expr (caddar args)))            ;;Index
                    (eval-expr (cadr args)))
                ;;(display "I am gone")(newline)
                ;;(printf "Array-table is ~a~n" *array-table*)
            )                        ;;WHAT I FUCK WANT IN IT
            (begin
                (display "Invalid asub Expression")(newline)
                (exit 1)
            )
        )
    ;;Otherwise moves on to the standard Let Function
        (if (symbol? (car args)) 
            (hash-set! *var-table* (car args) (eval-expr (cadr args)))
            (if(and (symbol? (car args))
                (not (null? (hash-ref *array-table* (cdr args)))))
                    (vector-set! (hash-ref *array-table* (car args))
                        (exact-round
                            (eval-expr (car args))) (eval-expr (cdr args)))
                        (exit 1)
        )))
    ;;(display "WHY")(newline)
    (interp-program continuation))


;;Complete? Test Later

;;DO I EVEN NEED THE "CONTINUATION" FOR GOTO
(define (interp-goto args continuation) ;;args continuation
    ;;if Input is NULL
    (if (null? (car args)) ;;If no input do ERROR
        (die '("Error: Label is NULL."))
    ;;if key exists in label table  
    ;;then Do interpret-prog  ;; ELSE ERROR MESSAGE
        (if (hash-has-key? *label-table* (car args)) 
            (interp-program (hash-ref *label-table* (car args))) 
            (die '("Error: Undeclared Label in args.")))))

;;if the comparison is true, control transfers 
;;to the statement, as for the goto statement.
(define (interp-if args continuation)
    ;;Checking if RELOP IS THERE
    (define temp (car args))
    ;;if < > <= >= !=  exists do eval-expr    
    (if ((hash-ref *function-table* (car temp)) 
        (eval-expr (cadr temp)) (eval-expr (caddr temp)))
        ;;(printf "Var-table is ~a~n" *var-table*)
        (interp-goto (cdr args) continuation)  ;;IF
        (interp-program continuation))) ;;ELSE

(define (interp-print args continuation)
    (define (print item)
        (if (string? item)
            (printf "~a" item)
            (printf " ~a" (eval-expr item))))
    (for-each print args)
    (printf "~n");
    (interp-program continuation))

;;EOF DOESNT BECOME 1 IN INPUT FUNCT FOR 30-QUAD

;;INPUT Funct
;; USING readnums.scm AS A REFERENCE 
(define (interp-input args continuation)
    ;;(when (not (null? args))
        ;;Put Input into Number

    (if (null? args)
        (interp-program continuation) 
        ;; if args is null Move on
        (let ((number (readnumber args continuation))) 
            ;;if args not null
            (if (eof-object? number)
                (begin 
                (hash-set! *var-table* 'eof 1)
                (interp-program continuation))
                (begin 
                    (hash-set! *var-table* (car args) number)
                    ;;(printf "Var-table is ~a~n" *var-table*)
                    (interp-input (cdr args) continuation))))))

(define (readnumber args continuation)
    ;;(display "helloooo")(newline)
    (let ((object (read))
        (nan (/ 0.0 0.0)))
        ;;(printf "object: ~a~n" object)
        (cond [(eof-object? object) object]
            [(number? object) (+ object 0.0)];;(begin 
                ;;(display object)(newline))]
            [else nan])))

(for-each (lambda (fn) (hash-set! *stmt-table* (car fn) (cadr fn)))
   `(
        (dim   ,interp-dim)
        (let   ,interp-let)
        (goto  ,interp-goto)
        (if    ,interp-if)
        (print ,interp-print)
        (input ,interp-input)
    ))
    
(define (interp-program program)
    (when (not (null? program))
          (let ((line (line-stmt (car program)))
                (continuation (cdr program)))
               (if line
                   (let ((func (hash-ref *stmt-table* (car line) #f)))
                        (func (cdr line) continuation))
                   (interp-program continuation)))))


;;put-labels-in-hash function from labelhash.scm example code
;;Thanks perry
;;SHOULD BE COMPLETE
(define (scan-for-labels program)
    (define (get-label line)
        (and (not (null? line))
             (not (null? (cdr line)))
             (cadr line)))
    (when (not (null? program))
          (let ((label (get-label (car program))))  
          ;;label = get-label(car prog)
               (when (symbol? label)
                     (hash-set! *label-table* label program)))
          (scan-for-labels (cdr program))))
    ;;(not-implemented 'scan-for-labels '() 'nl))

(define (readlist filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*RUN-FILE* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-program filename program)
    (define (dump-line line)
        (dump (line-number line) (line-label line) (line-stmt line)))
    (dump *RUN-FILE* *DEBUG* filename)
    (dump program)
    (for-each (lambda (line) (dump-line line)) program))

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
                       (interp-program program))))))

(main *ARG-LIST*)

