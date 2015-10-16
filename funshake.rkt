#| Functional Shakespeare Interpreter

Read through the starter code carefully. In particular, look for:

- interpret: the main function used to drive the program.
  This is provided for you, and should not be changed.
- evaluate: this is the main function you'll need to change.
  Please put all helper functions you write below this one.
  Doing so will greatly help TAs when they are marking. :)
|#
#lang racket

; You are allowed to use all the string functions in this module.
; You may *not* import any other modules for this assignment.
(require racket/string)

; This exports the main driver function. Used for testing purposes.
; This is the only function you should export. Don't change this line!
(provide interpret)

#| Begin startercode: |#

;------------------------------------------------------------------------------
; Parsing constants
;------------------------------------------------------------------------------

; Sections dividers
(define personae "Dramatis personae")
(define settings "Settings")
(define finis "Finis")

; Comment lines
(define comments '("Act" "Scene"))

; List of all "bad words" in a definition
(define bad-words
  '("vile"
    "villainous"
    "wicked"
    "naughty"
    "blackhearted"
    "shameless"
    "scoundrelous"))

; Arithmetic
(define add " join'd with ")
(define mult " entranc'd by ")

; Self-reference keywords
(define self-refs
  '("I"
    "me"
    "Me"
    "myself"
    "Myself"))

; Function call
(define call "The song of ")
(define delim " and ")

; Function parameter name
(define param "Hamlet")

#| End startercode |#

;------------------------------------------------------------------------------
; Interpreter driver
;------------------------------------------------------------------------------

#|
(interpret filename)
  filename: a string representing the path to a FunShake file

  Returns a list of numbers produced when evaluating the FunShake file.
  You can complete this assignment without modifying this function at all,
  but you may change the implementation if you like. Please note that you may
  not change the interface, as this is the function that will be autotested.
|#
(define (interpret filename)
  (let* ([contents (port->string (open-input-file filename))]
         [lines (map normalize-line (string-split contents "\n"))]
         ; Ignore title, empty, and comment lines
         [body (remove-empty-and-comments (rest lines))])
    (evaluate body)))

#|
(normalize-line str)
  str: the line string to normalize

  Remove trailing period and whitespace.
|#
(define (normalize-line str)
  (string-trim (string-normalize-spaces (string-trim str)) "."))

#|
(remove-empty-and-comments strings)
  strings: a list of strings

  Removes all empty strings and FunShake comment strings from 'strings'.
|#
(define (remove-empty-and-comments strings)
  (filter (lambda (s)
            (and
             (< 0 (string-length s))
             (not (ormap (lambda (comment) (prefix? comment s))
                         comments))))
          strings))

#|
(prefix? s1 s2)
  s1, s2: strings

  Returns whether 's1' is a prefix of 's2'.
|#
(define (prefix? s1 s2)
  (and (<= (string-length s1) (string-length s2))
       (equal? s1 (substring s2 0 (string-length s1)))))

;------------------------------------------------------------------------------
; Main evaluation (YOUR WORK GOES HERE)
;------------------------------------------------------------------------------

#|
(evaluate body)
  body: a list of lines corresponding to the semantically meaningful text
  of a FunShake file.

  Returns a list of numbers produced when evaluating the FunShake file.
  This should be the main starting point of your work! Currently,
  it just outputs the semantically meaningful lines in the file.
|#
(define (evaluate body)
  (evalh body dividers '() '())      
  )

(define dialogue 'dialogue)
(define dividers (list personae settings))

#|
dividers is a list of divider phrases.
body is a list of lines, starting with one equal to finis when the
function is initially called.
Return a sublist of dividers that begins with the first
divider found in body. If none were found, return (list dialogue)
|#
(define (skip dividers body)
  (cond [(equal? body empty) (println "Error: Empty body (skip)")]
        [(equal? dividers empty) (list dialogue)]
        [(equal? (first dividers) (first body))
         dividers]
        [else (skip (rest dividers) body)]))

#|
Step through a list of lines of text (body)
All branches end in a call to evalh (if the line being considered partitions
the file (ex: "Settings", "Finis"), or to dispatch (if the line is a meaningful
part of the file to be interpreted).
dividers is a list the first element of which is the section the current
line belongs to, bindings are the current global bindings, and out is the
list of output accumulated from all previous lines that were interpreted
|#
(define (evalh body dividers bindings out)
  (cond [(equal? body '()) out]
        ;Finis ends a section
        [(equal? (first body) finis)
         (evalh (rest body) (skip dividers (rest body)) bindings out)]
        
        ;Move onto the next divider
        [(and (not (equal? (rest dividers) '()))
              (equal? (first body) (second dividers)))
         (println "Shouldn't happen")
         (evalh (rest body) (rest dividers) bindings out)]
        ;Skip ahead to the second divider (if no settings)
        [(and (not (equal? (rest dividers) '()))
              (not (equal? (rest (rest dividers)) '()))
              (equal? (second dividers) (rest body)))
         (println "SHouldn't happen")
         (evalh (rest body) (rest (rest dividers)) bindings out)]
        ;Handle the line(s)
        [else (dispatch body dividers bindings out)]
  ))

#|
Do something with the next line or two of lst, depending on which divider
is at the front of dividers. Out is the output of the program so far
and bindings the current global bindings.
All conditional branches end in a call to evalh, passing as a parameter
the back of lst
|#
(define (dispatch lst dividers bindings out)
  (cond
    ;A divider does not get evaluated
    [(equal? (first dividers) (first lst))
     (evalh (rest lst) dividers bindings out)]
    ;Evaluate the value of the persona, and attach it and the name
    ;to the front of the list of bindings passed down through evalh
    [(equal? (first dividers) personae)
     (let ([new-bindings (bind (first lst) bindings #t)])
       (if new-bindings
           (evalh (rest lst) dividers new-bindings out)
           (print "Error: in binding (persona)")))]
    ;Bind the expression in the setting to the name of the setting
    ; and attach this to the front of the list of bindings
    [(equal? (first dividers) settings)
     (let ([new-bindings (bind (first lst) bindings)])
       (if new-bindings
           (evalh (rest lst) dividers new-bindings out)
           (print "Error: in binding (setting)")))]
    ;Bind 'self to the name of the speaker, put this binding at the
    ;front of the list of bindings, and use this to evaluate the second
    ;line of lst. Append the value this produces to out and pass out
    ;and the original bindings to evalh.
    [(equal? (first dividers) dialogue)
     (let* ([selfbindings
             (cons (binding 'self
                            (string-trim (first lst) ":")
                            bindings)
                   bindings)]
            [outval (apply eval-expr (cons (second lst)
                                           selfbindings))])
       (evalh (rest (rest lst)) dividers bindings
              (append out (list outval))))]
    [else (println "Unrecognized divider")]))

; Name-value bindings:
#|
Create a binding of the value of val to name. Use bindings when evaluating
the value. If computed is not #f, use its value as the precomputed value
If computed is #f, but the expression is of type base or none (meaning it
contains no calls or operations, or it is empty), its value can be statically
evaluated so do so and store that in the binding.
If literal? is #t, the expression is a string literal (a description)
containing no bound identifiers.
|#
(define (binding name val bindings [computed #f] [literal? #f])
  (list name val
        (if literal?
            ;Simply evaluate the expression
            (eval-base val)
            ;If computed is not #f, use that value; if it is of type 'base
            ;or 'none, statically evaluate val and store that value as
            ;computed, otherwise leave computed as it was
            (if (and (or
                      (equal? (get-type keywords val) 'base)
                      (equal? (get-type keywords val) 'none))
                     (not computed))
                (apply eval-expr (cons val bindings))
                computed))))
;Access methods
(define (get-id binding) (first binding))
(define (get-val binding) (second binding))
(define (get-computed binding) (third binding))

#|
Search the list bindings and return the sublist starting with the first
binding of name, or #f if no such list exists. If name is in self-refs,
return the first binding of 'self
|#
(define (find-binding bindings name)
  (cond [(equal? bindings empty) #f]
        [(and (equal? (get-id (first bindings)) 'self)
              (among? name self-refs))
         bindings]
        [(equal? (get-id (first bindings)) name)
         bindings]
        [else (find-binding (rest bindings) name)]))

#|
Parse the line var and create a binding the name of which is what
precedes the comma, with the value being the expression following
the comma.
|#
(define (bind var bindings [literal? #f])
  (let* ([split (string-split var ", ")]
         [name (first split)]
         [val (foldr string-append ""
                      (rest split))])
        (cons (binding name val bindings #f literal?) bindings)))

;Closures:

#|
Create a closure. A closure is an expression to evaluate, and a
set of bindings. The expression may contain the name Hamlet, and the bindings
should have a binding of the name Hamlet.
|#
(define (closure function bindings)
 (cons function bindings))
;Access methods
(define (get-function closure)
  (car closure))
(define (get-bindings closure)
  (cdr closure))
;To evaluate a closure, use the bindings to evaluate the function expression
(define (eval-function closure)
  (apply eval-expr (cons (get-function closure) (get-bindings closure))))

(define (subeval-function closure)
  (let ([binding (find-binding (get-bindings closure) param)])
    (if binding
        (let* ([arg (get-val (first binding))]
               [split-on-param (string-split
                        (get-function closure) param #:trim? #f)]
               [substituted (rest (foldr (lambda (x y) (cons arg (cons x y)))
                       '()
                       split-on-param))]
               [val(foldr string-append "" substituted)])
          (apply eval-expr (cons val (rest binding))))
    (println "Error: no param"))))
;Expressions

;Return #t if expression is found in lst
(define (among? expression lst)
  (cond [(equal? empty lst) #f]
        [(equal? (first lst) expression) #t]
        [else (among? expression (rest lst))]))

#|
Return #f if the expression does not have keyword in it. Otherwise a list
consisting of the keyword and all the strings (in order) in expression that
were separated by keyword
|#
(define find-toplevel (lambda (keyword expression) 
                        (let ([split (string-split expression keyword)])
                          (cond [(equal? (first split) expression) #f]
                                [(and
                                  (equal? keyword call)
                                  (equal? (first
                                           (string-split (first split) delim))
                                          (first split))) #f]
                                 [else (cons keyword
                                       split)]))))
#|
Call find-toplevel with expression on each keyword. The first time this
does not return #f, return the result. If all keywords give #f, returns 'base
|#
(define get-type (lambda (keywords expression) 
                   (cond [(equal? keywords empty) 'base]
                         [(equal? (string-trim expression) "")
                          'none]
                         [(let ([top
                                 (find-toplevel (first keywords) expression)])
                            top top)]
                         [else (get-type (rest keywords) expression)])))
;Calls take precedence over arithmetic (closer to the toplevel)
(define keywords (list call add mult))

#|
Evaluate the expression that is the first element of arglist.
If there are succeeding elements of arglist, each one is a binding
corresponding to a global binding or an unbound identifier in expression
|#
(define eval-expr
  (lambda arglist
    (let* ([expression (first arglist)]
           [bindings (rest arglist)]
           [type (get-type keywords expression)])
      (cond
        ;'none are empty expressions
        [(equal? type 'none) 0]
        ;'base expressions contain no arithmetic or function calls
        [(equal? type 'base) 
         (apply eval-base arglist)]
        ;Arithmetic operations:
        [(equal? (car type) add)
         (eval-binary (cdr type) + bindings)]
        [(equal? (car type) mult)
         (eval-binary (cdr type) * bindings)]
        ;Calls:
        ;Find the binding of the function being called, and bind Hamlet
        ;to the argument, before evaluating the resulting closure
        [(equal? (car type) call)
         (let* ([split (string-split (second  type) delim)]
                [fname (first split)]
                [arg 
                 (string-trim
                  (foldr (lambda (x y) (string-append x " " y))
                         ""
                         (rest split)))]
                [funcb (find-binding bindings fname)]
                [fval (if funcb (get-val (first funcb)) "")] 
                [closure
                 (closure fval
                          (append
                           (list (binding param arg funcb)) bindings))])
           (eval-function closure))]))))

#|
Evaluate the first and second elements (expressions) in lst and return 
the result of calling func on them
|#
(define (eval-binary lst func bindings)
  (func (apply eval-expr (cons (first lst) bindings))
        (apply eval-expr (cons (second lst) bindings))))

#|
Evaluate an expression of type 'base. Arglist is as in eval-expr. 
|#
(define eval-base
  (lambda arglist
    (let*
        ([expression (first arglist)]
         [bindings (rest arglist)]
         ;If expression is in bindings, it may be shadowing another
         ;binding. (relevant in nested function calls when we have multiple
         ;"Hamlet"s that refer to the last one
         [innerbindings (find-binding bindings expression)])
      (if innerbindings
          (let ([binding (first innerbindings)])
            (if (get-computed binding)
                (get-computed binding)
                (apply eval-expr
                       (cons (get-val binding) (rest innerbindings)))))
          ;expression is not a name to which anything is bound, so treat it
          ;as a string literal
          (let*
              ([words (string-split expression " ")]
               [these-bad-words 
                (filter (lambda (word) 
                          (ormap (lambda (x) (equal? x word)) bad-words))
                        words)]
               [numbad (length these-bad-words)])
            (if (> numbad 0) 
                (* (- 0 1) (expt 2 numbad) (length words))
                (length words)))))))
