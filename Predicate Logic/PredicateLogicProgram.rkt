;;;Done by William Holder
#|
(start "PreloadData.txt")
|#

#lang racket

(define (debug)
  (start "PreloadData.txt")
  )

(define (start filename)
  (define in (open-input-file filename))
  (define textinfo (readfile in))
  (define groups (getListComponents (cdr (cdr textinfo))))
  (define facts (getListComponents (cdr (cdr (memq ': (cdr textinfo))))))
  (define is (getListComponents (cdr (cdr (memq ': (cdr (memq ': (cdr textinfo))))))))
  (define isnot (getListComponents (cdr (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr textinfo))))))))))
  (define can (getListComponents (cdr (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr textinfo))))))))))))
  (define cannot (getListComponents (cdr (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr textinfo))))))))))))))
  (display groups)(newline)(newline)
  (display facts)(newline)(newline)
  (display is)(newline)(newline)
  (display isnot)(newline)(newline)
  (display can)(newline)(newline)
  (display cannot)(newline)(newline)
  (define initFacts (createInitialFacts groups facts))
  (display initFacts)(newline)(newline)
  (display (addToCategory initFacts 'actor '((William_Holder) (Bob_Man) (Clint_Eastwood) (William_Holder))))(newline)(newline)
  'done
  )

(define (readfile in)
  (define inf (read in))
  (cond
    ((eof-object? inf) '())
    (else (cons inf (readfile in)))
    )
  )

(define (getListComponents lst)
  (cond
    ((eq? '- (car lst)) 
     (cond
       ((eq? ': (car (cdr lst))) '())
       (else (cons (getItems (cdr lst)) (getListComponents (memq '- (cdr lst)))))
        )
    )
    (else (error "(wdh)Invalid Syntax: All statments must have - preceding it (including :)"))
   )
  )

  
(define (getItems lst)
  (cond
    ((eq? '- (car lst)) '())
    ((eq? ': (car lst)) '())
    (else (cons (car lst) (getItems (cdr lst))))
   )
  )

(define (getFirst lst)
  (cond
    ((null? lst) '())
    (else (cons (list(car (car lst))) (getFirst (cdr lst))))
    )
  )

;;; ctgs = categories
(define (createInitialFacts ctgs rules)
  (cond
    ((null? ctgs) '())
     (else (cons (cons (car (car ctgs)) (makeListCategories (car(car ctgs)) rules)) (createInitialFacts (cdr ctgs) rules)))
    )
  )

(define (makeListCategories ctg rules)
  (cond
    ((null? rules) '())
    ((eq? ctg (car (car rules))) (cons (cdr (car rules)) (makeListCategories ctg (cdr rules))))
    (else (makeListCategories ctg (cdr rules)))
   )
  )

(define (addToCategory wlist ctg itms)
  (cond
    ((null? wlist) '())
    ((eq? (car (car wlist)) ctg) (cons (car (cons (car (car wlist)) (removeDups (append (cdr (car wlist)) itms)))) (addToCategory (cdr wlist) ctg itms)))
    (else (cons (car wlist) (addToCategory (cdr wlist) ctg itms)))
    )
  )

(define (addToList plist itms)
  (cond
   ((null? itms) '())
   ((memq (car itms) (cdr itms)) (addToList plist (cdr itms)))
   ((memq (car itms) plist) (addToList plist (cdr itms)))
   (else (append (addToList plist (cdr itms)) (list (list(car itms)))))
   )
  )

(define (removeDups olist)
  (cond
    ((null? olist) '())
    ((memq (car olist) (cdr olist)) (removeDups(olist)))
    (else (cons (car olist) (removeDups (cdr olist))))
   )
  )