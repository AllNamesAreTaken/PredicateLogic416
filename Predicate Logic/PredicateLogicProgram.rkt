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
  (define initGroups (getListComponents (cdr (cdr textinfo))))
  (define initFacts (getListComponents (cdr (cdr (memq ': (cdr textinfo))))))
  (define initIs (getListComponents (cdr (cdr (memq ': (cdr (memq ': (cdr textinfo))))))))
  (define initIsNot (getListComponents (cdr (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr textinfo))))))))))
  (define initCan (getListComponents (cdr (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr textinfo))))))))))))
  (define initCannot (getListComponents (cdr (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr (memq ': (cdr textinfo))))))))))))))
  (display initGroups)(newline)(newline)
  (display initFacts)(newline)(newline)
  (display initIs)(newline)(newline)
  (display initIsNot)(newline)(newline)
  (display initCan)(newline)(newline)
  (display initCannot)(newline)(newline)
  (define facts (createFacts initGroups initFacts))
  (display facts)(newline)(newline)
  (set! facts (addToCategory facts 'actor '((William_Holder) (Bob_Man) (Clint_Eastwood) (William_Holder))))
  (display facts)(newline)(newline)
  (set! facts (addToCategory facts 'monkey '((William) (Goku))))
  (display facts)(newline)(newline)
  (set! facts (addToCategory facts 'costars '((William Goku) (Test1 Test2) (Test1 Test3) (Test2 Test3) (William Goku))))
  (display facts)(newline)(newline)
  (set! facts (transferMultiParts initGroups facts))
  (printByList facts)(newline)(newline)
  'Finished
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

(define (printByList lst)
  (cond
   ((null? lst) (void))
   (else (display (car lst))(newline)(printByList (cdr lst)))
   )
  )

;;; ctgs = categories
(define (createFacts ctgs rules)
  (cond
    ((null? ctgs) '())
     (else 
      (cons 
       (cons 
        (car (car ctgs))
        (cons
         (car (cdr (car ctgs)))
         (makeListCategories (car(car ctgs)) rules))
        )
       (createFacts (cdr ctgs) rules))
      )
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
    ((eq? (car (car wlist)) ctg) 
     (append 
      (list (cons 
             (car (car wlist)) 
             (addAndRemoveDups (cdr (car wlist)) itms))) 
      (cdr wlist))
     )
    (else (cons (car wlist) (addToCategory (cdr wlist) ctg itms)))
    )
  )

(define (addAndRemoveDups nlist items)
  (removeListDups (append nlist items))
  )

(define (turnToSublists mlist)
  (cond
    ((null? mlist) '())
    (else (cons (list (car mlist)) (turnToSublists (cdr mlist))))
  )
  )

(define (removeDups olist num)
  (cond
    ((> num 1) (removeListDups olist))
    (else (removeAtomDups olist))
   )
  )

(define (removeAtomDups olist)
  (cond
    ((null? olist) '())
    ((memq (car olist) (cdr olist)) (removeAtomDups (cdr olist)))
    (else (cons (car olist) (removeAtomDups (cdr olist))))
    )
  )

(define (removeListDups olist)
  (cond
   ((null? olist) '())
   ((hasMatchingList? (cdr olist) (car olist)) (removeListDups (cdr olist)))
   (else (cons (car olist) (removeListDups (cdr olist))))
   )
  )

(define (hasMatchingList? olist lis)
  (cond
    ((null? olist) #f)
    ((doListMatch (car olist) lis) #t)
    (else (hasMatchingList? (cdr olist) lis))
   )
  )

(define (doListMatch lis1 lis2)
  (cond
   ((and (null? lis1) (null? lis2)) #t)
   ((or (null? lis1) (null? lis2)) #f)
   ((eq? lis1 lis2) #t)
   ((or (not (pair? lis1)) (not (pair? lis2))) #f)
   ((eq? (car lis1) (car lis2)) (doListMatch (cdr lis1) (cdr lis2)))
   (else #f)
   )
  )

(define (transferMultiParts mlist flist)
  (cond
    ((null? mlist) flist)
    ((> (car (cdr (car mlist))) 1) (transferMultiParts (cdr mlist) (updateMP (car mlist) flist (- (car(cdr(car mlist))) 1))))
    (else (transferMultiParts (cdr mlist) flist))
   )
  )

(define (updateMP mlist flist num)
  (cond
   ((< num 0) flist)
   (else (updateMP mlist (addToCategory flist (getIndex mlist (+ num 2)) (retreiveAllCategory flist (car mlist) num)) (- num 1)))
   )
  )

(define (retreiveAllCategory alist ctg num)
  (cond
   ((null? alist) '())
   ((eq? (car (car alist)) ctg) (getSectList (cdr (cdr (car alist))) num))
   (else (retreiveAllCategory (cdr alist) ctg num))
   )
  )

(define (getSectList alist num)
  (cond
   ((null? alist) '())
   (else (cons (list (getIndex (car alist) num)) (getSectList (cdr alist) num)))
   )
  )

(define (getIndex alist num)
  (cond
   ((null? alist) '())
   ((> num 0) (getIndex (cdr alist) (- num 1)))
   (else (car alist))
   )
  )

(define (setExpression wlist plist)
  (cond
   ((null? plist) wlist)
                      ;;;addToCategory facts (   category    )  factsToAdd
   (else (setExpression (addToCategory wlist (car (car plist)) (getExpressionParts wlist (cdr (car plist)))) (cdr plist)))
   )
  )
;;;Needs to return new elements to add [ ((test tes2) (test3 test4)) or maybe ((test) (test1)) may need number
;;;wlist = all facts
;;;parts = is statements
(define (getExpressionParts wlist parts)
  (cond
    ((null? parts) '())
    (else (cons (getElements wlist (car parts)) (getExpressionParts wlist (cdr parts))))
   )
  )

(define (simplifyListToLists wlist)
(cond
  ((null? wlist) '())
  ((pair? wlist)  
     (cond
       ((pair? (car wlist)) (append (simplifyListToLists (car wlist)) (simplifyListToLists (cdr wlist))))
       (else (list wlist))
     )
     )
  (else '())
 )
)

(define (simplifyListToAtoms wlist)
(cond
  ((null? wlist) '())
  ((pair? wlist)  
     (cond
       ((pair? (car wlist)) (append (simplifyListToAtoms (car wlist)) (simplifyListToAtoms (cdr wlist))))
       (else wlist)
     )
     )
  (else '())
 )
)

(define (getAllCombinations slist plist)
  (cond
    ((null? plist) slist)
    ((pair? plist) 
     (cond
       ((null? (car plist)) '())
       ;;;(else (evaluateExpression wlist part))
       (else (cons (getAllCombinations (append slist (list (car (car plist)))) (cdr plist)) (getAllCombinations slist (cons (cdr (car plist)) (cdr plist) ))))
     )
     )
    (else '())
   )
  )
;;;(getElements '((fact1 1 (William_Holder) (Bob_Guy)) (fact2 2 (Guy John) (Guy Bill) (Blah William_Holder))) '(fact1 0 and fact2 1))
(define (getElements wlist part)
  (cond
    ((null? part) '())
    ((pair? part) 
     (cond
       ((memq 'and part) (andOp (getElements wlist (append (list (car part)) (list (car (cdr part))))) (getElements wlist (cdr (cdr (cdr part))))))
       ((memq 'or part) (orOp (getElements wlist (append (list (car part)) (list (car (cdr part))))) (getElements wlist (cdr (cdr (cdr part))))))
       ((memq 'not part) (notOp (getElements wlist (append (list (car part)) (list (car (cdr part))))) (getElements wlist (cdr (cdr (cdr part))))))
       ((pair? (car part)) (cons (getElements wlist (car part)) (getElements wlist (cdr part))))
       ;;;(else (evaluateExpression wlist part))
       (else (simplifyListToAtoms (retreiveAllCategory wlist (car part) (car (cdr part)))))
     )
     )
    (else '());;;(retreiveAllCategory wlist (car part) (car (cdr part))))
   )
  )
  
(define (andOp lis1 lis2)
  (cond
   ((null? lis1) '())
   ((memq (car lis1) lis2) (cons (car lis1) (andOp (cdr lis1) lis2)))
   (else (andOp (cdr lis1) lis2))
   )
  )
  
(define (orOp lis1 lis2)
    (removeAtomDups (append lis1 lis2))
  )
  
(define (notOp lis1 lis2)
  (cond
   ((null? lis1) '())
   ((memq (car lis1) lis2) (notOp (cdr lis1) lis2))
   (else (cons (car lis1) (notOp (cdr lis1) lis2)))
   )
  )