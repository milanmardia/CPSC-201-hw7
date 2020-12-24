#lang racket
(provide
 isort
 msort
 qsort
 key<=?
 random-list
 lookup
 depth
 update
 update-table-tree
 binary-table-tree
 lookup-table-tree
 )

; ********************************************************************
; Name: Milan Mardia
; Email address: milan.mardia@yale.edu
; ********************************************************************

; CS 201a HW #7  DUE 5:00 pm Thursday, Dec 10, 2020
; end of Reading Period using the submit command.

; Here is a library  and some code that will allow you to
; time the execution of procedures.


; include library and procedure definitions for timings
(#%require srfi/19)

(define make-timer
  (lambda ()
    (let ((start-time (current-time)))
      (lambda ()
	(let ((elapsed (time-difference (current-time) start-time)))
	  (+ (exact->inexact (time-second elapsed))
	     (/ (exact->inexact (time-nanosecond elapsed))
		(* 1.0e3 (time-resolution)))))))))
; example of using make-timer to
; create a procedure for timing a sort of time testsort

(define time-sorter
  (lambda (sorter lst compare?)
    (let ((t1 (make-timer)))
      (sorter lst compare?)
      (t1))))



; Unless the problem specifies otherwise:
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.
; * Your procedures need *not* fail gracefully if their
; inputs do not satisfy the assumptions of the problem.
; * You may assume that the input is given as specified,
; you do not need to include tests in your code to verify
; that the input is valid.
; * Please use the specified names (and numbers of arguments)
; for the required procedures in a problem; this is to
; facilitate automatic testing.
; * Do NOT use MUTATORS (ie. set!)
; * Do NOT use "define" in the body of a procedure definition
; * Do NOT use take, drop, split-at procedures in your solutions.
; Use the procedures we have been using in class and on other
; assignments (i.e. if you can't find a procedure in class notes
; or another assignment, don't use it.)
; * You may and will need to use the predicates string<=? and string>=? as arguments for testing some of
; the sorting of unordered lists.

; ************************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 15)

; ************************************************************************
; ** problem 1 ** (16 points)
; Write two procedures for insertion sort:
; (insert item lst compare?)
; (isort lst compare?)
; where lst is a list and compare? is a procedure to
; compare two list elements and return either #t or #f.
; Insertion sort was described in lecture, and a version
; of the code for insert is in the pdf notes. You need to generalize
; that code for a general predicate compare? .


; (insert item lst compare?) returns a list equal to lst
; with item inserted in the correct order according to compare?,
; assuming that lst is already correctly ordered by compare?
; (isort lst compare?) insertion sorts lst using insert and
; compare? and returns the correctly sorted list.

; Examples:
; (insert "alonzo" '("algorithm" "program") string<=?) => '("algorithm" "alonzo" "program")
; (insert 7 '(2 3 5 11 13 17) <=) => '(2 3 5 7 11 13 17)
; (isort '(1 3 5 7 2 4 6 8) >=) => '(8 7 6 5 4 3 2 1)
; (isort '(1 3 5 7 2 4 6 8) <=) => '(1 2 3 4 5 6 7 8)
; ************************************************************************

(define insert
  (lambda (itm lst compare?)
    (cond
      ((null? lst) (cons itm '()))
      ((compare? itm (first lst)) (cons itm lst))
      (else (cons (first lst) (insert itm (rest lst) compare?))))))

(define isort-helper
  (lambda (orig-lst new-lst compare?)
    (cond
      ((null? orig-lst) new-lst)
      (else (isort-helper (rest orig-lst) (insert (first orig-lst) new-lst compare?) compare?)))))

(define isort
  (lambda (lst compare?)
    (isort-helper lst '() compare?)))




; ************************************************************************
; ** problem 2 ** (16 points)
; Write two procedures for merge sort:
; (merge lst1 lst2 compare?)
; (msort lst compare?)
; where lst, lst1 and lst2 are lists and
; compare? is a procedure that compares two list
; elements and returns either #t or #f.
; Merge sort was described in lecture, and code
; can be found the pdf notes. Again you need to
; generalize for the predicate compare? .

; (merge lst1 lst2 compare?)
; returns a list containing all the elements of lst1 and lst2
; correctly ordered according to compare?,
; assuming that lst1 and lst2 are each correctly ordered according
; to compare?
; (msort lst compare?) merge sorts lst using merge and compare? and
; returns the correctly sorted list.

; Examples:
; (merge '(1 14 55 124) '(2 3 150 155) <=) => '(1 2 3 14 55 124 150 155)
; (merge '(124 55 14 1) '(155 150 3 2) >=) => '(155 150 124 55 14 3 2 1)
; (merge '("that" "is" "best") '("where" "were" "they") string>=?) => '("where" "were" "they" "that" "is" "best")
; (merge '("what" "is" "correct") '("where" "was" "it") string>=?) => '("where" "what" "was" "it" "is" "correct")
; (msort '(14 15 2 99 33 100 16) >=) => '(100 99 33 16 15 14 2)
; (msort '(14 15 2 99 33 100 16) <=) => '(2 14 15 16 33 99 100)
; (msort '("is" "best" "where" "they" "that" "were") string<=?) => '("best" "is" "that" "they" "were" "where")
; ************************************************************************



(define merge
  (lambda (lst1 lst2 compare?)
    (cond ((null? lst1) lst2)
          ((null? lst2) lst1)
          ((compare? (first lst1) (first lst2)) (cons (first lst1) (merge (rest lst1) lst2 compare?)))
          (else (cons (first lst2) (merge lst1 (rest lst2) compare?))))))

(define split-beginning
  (lambda (lst index)
    (cond ((= index 0) '())
          (else (cons (first lst) (split-beginning (rest lst) (- index 1)))))))

(define split-end
  (lambda (lst index)
       (cond
         ((= index 0) lst)
         (else (split-end (rest lst) (- index 1))))))
(define msort
  (lambda (lst compare?)
    (cond ((null? lst) '())
          ((= (length lst) 1) (list (first lst)))
          (else (merge (msort (split-beginning lst (quotient (length lst) 2)) compare?) (msort (split-end lst (quotient (length lst) 2)) compare?) compare?))))) 


; ************************************************************************
; *** problem 3 ** 12 pts
; ************************************************************************

; Here is a procedure for quicksort, from 
; http://blog.matthewrathbone.com/
;
(define pHelper (lambda (all chk l m)
                  (cond ((null? all) (cons l (cons chk (cons m '()))))
                        (else
                        (let ((x (first all)))
                          (if (<= x chk) 
                              (pHelper (rest all) chk (cons x l) m)
                              (pHelper (rest all) chk l (cons x m))))))))

(define partition (lambda (l)
                      (pHelper (rest l) (first l) '() '())))



(define quicksort (lambda (l)
                    (cond ((null? l) l)
                          (else
                          (let ((lx (partition l)))
                            (append (quicksort (first lx)) (cons (first (rest lx)) (quicksort (first (rest (rest lx)))))))))))


; Modify the code for quicksort to write a procedure qsort that takes
; a comparison function compare? as a parameter.

; Examples:
; (qsort '("is" "best" "where" "they" "that" "were") string<=?) => '("best" "is" "that" "they" "were" "where")
; (qsort '(9 21 43 6 7 435) >=) => '(435 43 21 9 7 6)
; ************************************************************************


(define pHelper1
  (lambda (all chk l m compare?)
                  (cond ((null? all) (cons l (cons chk (cons m '()))))
                        (else
                        (let ((x (first all)))
                          (if (compare? x chk) 
                              (pHelper1 (rest all) chk (cons x l) m compare?)
                              (pHelper1 (rest all) chk l (cons x m) compare?)))))))

(define partition1 (lambda (l compare?)
                      (pHelper1 (rest l) (first l) '() '() compare?)))


(define qsort (lambda (l compare?)
                    (cond ((null? l) l)
                          (else
                          (let ((lx (partition1 l compare?)))
                            (append (qsort (first lx) compare?) (cons (first (rest lx)) (qsort (first (rest (rest lx))) compare?))))))))



; ************************************************************************
; Consider sorting a table of values given by a list of two items (key value) 
; to put the list in order of ascending key value. The keys will all be integers.
; Write a procedure (key<=? lst1 lst2) that will produce the following:
;
; (qsort '((234 "no") (3 "aaa") (324 "foo") (9 "cat") (5 "dog")) key<=?)
;    => '((3 "aaa") (5 "dog") (9 "cat") (234 "no") (324 "foo"))
; (qsort '((234 234) (37 2345) (23 99900) (1 444) (3 2466) (9 2341)) key<=?)
;    => '((1 444) (3 2466) (9 2341) (23 99900) (37 2345) (234 234))
; ************************************************************************

(require racket/trace)

(define key<=?
  (lambda (lst1 lst2)
    (if (<= (first lst1) (first lst2))
        #t
        #f)))
; ************************************************************************
; ** problem 4 ** (11 points)
; Create a procedure (random-list) to generate a list of n random numbers,
; each of which ranges from [0,n-1]
; We will use this to generate lots of lists to use in timing tests of
; our sorting procedures.

; Examples:
; Since random numbers are generated, your results will vary
; note that the same number may (but does not have to) appear several times in the list

;(random-list 10) => '(7 2 1 7 0 3 8 7 2 8)
;(random-list 10) => '(4 0 7 4 5 4 2 7 1 6)
;(random-list 20) => '(7 11 5 19 17 4 15 16 7 15 12 4 3 6 19 3 10 9 18 9)
; ************************************************************************

(define random-list
  (lambda (n)
    (random-list-helper n n)))

(define random-list-helper
  (lambda (orig-n new-n)
    (cond ((= new-n 0) '())
          (else (cons (random orig-n) (random-list-helper orig-n (- new-n 1)))))))

; ************************************************************************
; ** problem 5 ** (11 points)
; Use the procedure random-list to create some test lists of integers.
; Use these test lists to create a table comparing timings for isort, msort and
; qsort. Test how the timing varies as the length of list varies;
; Test whether the comparison operator (i.e. <=, <, >= or > ) makes
; difference in your results. 
; For your results show:
;      Your table
;      a conclusion about the relative efficiency of the sort methods
;      a conclusion about whether the comparison operator mattered
; Your results should be summarized as either a text file or a pdf file
; named timings.txt or timings.pdf file

; some examples with times from my Mac:
; (define sample-test (random-list 5000))
; (time-sorter isort sample-test <=) => 0.716
; (time-sorter msort sample-test <=) => 0.019
; (time-sorter qsort sample-test <=) => 0.005
; (time-sorter isort sample-test >=) => 0.427
; (time-sorter msort sample-test >=) => 0.006
; (time-sorter qsort sample-test >=) => 0.017

; (define sample2-test (random-list 50000))
; (time-sorter qsort sample2-test <=) => 0.122

; some examples with times from an old machine in the Zoo
; (define another-test (random-list 100000))
; (time-sorter msort another-test >=) => 0.217
; (time-sorter isort another-test >=) => 209.125
; (time-sorter qsort another-test >=) => 0.172
; ************************************************************************



;(define sample-test-1 (random-list 1000))
;(define sample-test-2 (random-list 5000))
;(define sample-test-3 (random-list 10000))
;(define sample-test-4 (random-list 50000))
;
;;<=
;
;(time-sorter isort sample-test-1 <=)
;(time-sorter msort sample-test-1 <=)
;(time-sorter qsort sample-test-1 <=) 
;
;(time-sorter isort sample-test-2 <=)
;(time-sorter msort sample-test-2 <=)
;(time-sorter qsort sample-test-2 <=)
;
;(time-sorter isort sample-test-3 <=)
;(time-sorter msort sample-test-3 <=)
;(time-sorter qsort sample-test-3 <=)
;
;(time-sorter isort sample-test-4 <=)
;(time-sorter msort sample-test-4 <=)
;(time-sorter qsort sample-test-4 <=)
;
;
;;>=
;(time-sorter isort sample-test-1 >=)
;(time-sorter msort sample-test-1 >=)
;(time-sorter qsort sample-test-1 >=)
;
;(time-sorter isort sample-test-2 >=)
;(time-sorter msort sample-test-2 >=)
;(time-sorter qsort sample-test-2 >=)
;
;(time-sorter isort sample-test-3 >=)
;(time-sorter msort sample-test-3 >=)
;(time-sorter qsort sample-test-3 >=)
;
;(time-sorter isort sample-test-4 >=)
;(time-sorter msort sample-test-4 >=)
;(time-sorter qsort sample-test-4 >=)
;
;
;;<
;(time-sorter isort sample-test-1 <)
;(time-sorter msort sample-test-1 <)
;(time-sorter qsort sample-test-1 <)
;
;(time-sorter isort sample-test-2 <)
;(time-sorter msort sample-test-2 <)
;(time-sorter qsort sample-test-2 <)
;
;(time-sorter isort sample-test-3 <)
;(time-sorter msort sample-test-3 <)
;(time-sorter qsort sample-test-3 <)
;
;
;(time-sorter isort sample-test-4 <)
;(time-sorter msort sample-test-4 <)
;(time-sorter qsort sample-test-4 <)
;
;
;;>
;(time-sorter isort sample-test-1 >)
;(time-sorter msort sample-test-1 >)
;(time-sorter qsort sample-test-1 >)
;
;(time-sorter isort sample-test-2 >)
;(time-sorter msort sample-test-2 >)
;(time-sorter qsort sample-test-2 >)
;
;(time-sorter isort sample-test-3 >)
;(time-sorter msort sample-test-3 >)
;(time-sorter qsort sample-test-3 >)
;
;(time-sorter isort sample-test-4 >)
;(time-sorter msort sample-test-4 >)
;(time-sorter qsort sample-test-4 >)





; ************************************************************************
; ** problem 6 ** (11 points)
; We define a Racket data structure for binary search trees
; using lists as follows.
; In this problem and problem 7, we will consider lists of integers only.
; The empty binary search tree is the empty list ()
; A nonempty binary search tree is a list consisting of
; a number (the root), a binary search tree (the left subtree)
; and another binary search tree (the right subtree).
; All the numbers appearing in the left subtree are less than
; the root, and the root is less than or equal to all the numbers appearing
; in the right subtree.


; For example, the following binary search tree can be drawn
;              7
;            /   \
;           5     9

(define tree1 '(7 (5 () ()) (9 () ())))

; Make use of these selectors for a binary search tree.

(define root car)
(define left-subtree cadr)
(define right-subtree caddr)

; Write a procedure
; (lookup n tree)
; to look up a number n in the given binary search tree.
; It should return #t if found, #f if not found.
; Please use the selectors defined above.

; Examples:
; (lookup 7 tree1) => #t
; (lookup 6 tree1) => #f
; (lookup 5 tree1) => #t
; (lookup 9 tree1) => #t
; (lookup 11 tree1) => #f
; ************************************************************************

(define lookup
  (lambda (n tree)
    (cond ((= (root tree) n) #t)
          ((< n (root tree)) (if (null? (left-subtree tree))
                                 #f
                                 (lookup n (left-subtree tree))))
          (else (if (null? (right-subtree tree))
                                 #f
                                 (lookup n (right-subtree tree)))))))

; ************************************************************************
; ** problem 7 ** (11 points)
; Write two procedures
; (depth tree)
; (update tree n)
; where n is a number and tree is a binary search tree

; (depth tree) should return the maximum number of comparisons
; required to look up a number in tree -- that is, the length
; of the longest search path in tree.
; Note: the depth of an empty tree is 0.

; (update tree n) should return a binary search tree equal to tree
; with n inserted in the correct position. You may assume that
; n is not already in the tree.

; Examples:
; (update tree1 6) => '(7 (5 () (6 () ())) (9 () ()))
; (update (update tree1 8) 11) => '(7 (5 () ()) (9 (8 () ()) (11 () ())))
; (depth tree1) => 2
; (depth '()) => 0
; (depth (update tree1 6)) => 3
; ************************************************************************

(define max1
  (lambda (num1 num2)
    (if (> num1 num2)
        num1
        num2)))

(define depth
  (lambda (tree)
    (if (null? tree)
        0
        (+ (max1 (depth (right-subtree tree)) (depth (left-subtree tree))) 1))))

(define update
  (lambda (tree n)
    (cond ((null? tree) (list n '() '()))
          ((> n (root tree)) (list (root tree) (left-subtree tree) (update (right-subtree tree) n)))
          (else (list (root tree) (update (left-subtree tree) n) (right-subtree tree))))))

; ************************************************************************
; ** problem 8 ** (11 points)
; 
; We expand the use of binary trees to looking up values in tables.
; Write a procedure (binary-table-tree table compare-key?) that takes a look-up table
; in the form of a list of (key value) and produces a binary tree of key-values
; by recursively calling a procedure (update-table-tree table-tree table-item compare-key?)
; where (compare-key? key1 key2) returns #t if the table-item with key1 should appear
; before the item with key2 in the sorted list.

; Examples:
; (update-table-tree '((7 "y") () ()) '(5 "z") <=) => '((7 "y") ((5 "z") () ()) ())
; (update-table-tree  '(("icelandic" "íkorni") (("dutch" "eekhoorn") ()()) ()) '("finnish" "orava") string<=?)
;  => '(("icelandic" "íkorni") (("dutch" "eekhoorn") () (("finnish" "orava") () ())) ())

;(define test-table '((9 "a") (3 "b") (5 "c") (4 "q") (1 "z") (7 "y") ))
(define translate-table '(("bulgarian" " kateritsa") ("estonian" "orav")
                          ("finnish" "orava")("french" "ecureuil") ("welsh" "gwiwer")
                          ("german" "eichhoernchen") ("italian" "scoiattolo")
                          ("lithuanian" "vovere") ("portuguese" "esquilo") ("romanian" "veverita")
                          ("slovak" "vevericka") ("swedish" "ekorre") ("polish" "wiewiorka")
                          ("dutch" "eekhoorn") ("norwegian" "ekorn") ("irish" "iora rua")
                          ("icelandic" "íkorni")))

; The binary tree you build should have the last element in the list as the root.
; Here are examples with you should get for the test tables.

; (binary-table-tree test-table <=) => '((7 "y") ((1 "z") () ((4 "q") ((3 "b") () ()) ((5 "c") () ()))) ((9 "a") () ()))
; (binary-table-tree translate-table string<=?)
;  => '(("icelandic" "íkorni") (("dutch" "eekhoorn") (("bulgarian" " kateritsa") () ()) (("german" "eichhoernchen") (("french" "ecureuil") (("finnish" "orava") (("estonian" "orav") () ()) ()) ()) ())) (("irish" "iora rua") () (("norwegian" "ekorn") (("lithuanian" "vovere") (("italian" "scoiattolo") () ()) ()) (("polish" "wiewiorka") () (("swedish" "ekorre") (("slovak" "vevericka") (("romanian" "veverita") (("portuguese" "esquilo") () ()) ()) ()) (("welsh" "gwiwer") () ()))))))
; ************************************************************************




(define update-table-tree
  (lambda (table-tree table-item compare-key?)
       (cond ((null? table-tree) (list table-item '() '()))
             ((compare-key? (first table-item) (first (root table-tree))) (list (root table-tree) (update-table-tree (left-subtree table-tree) table-item compare-key?) (right-subtree table-tree)))
             (else (list (root table-tree) (left-subtree table-tree) (update-table-tree (right-subtree table-tree) table-item compare-key?))))))

(define binary-table-tree-helper
  (lambda (orig-table new-table compare-key?)
    (cond ((null? orig-table) new-table)
          (else (binary-table-tree-helper (rest orig-table) (update-table-tree new-table (first orig-table) compare-key?) compare-key?)))))

(define binary-table-tree
  (lambda (table compare-key?)
    (binary-table-tree-helper (reverse table) '() compare-key?)))
  
; ************************************************************************
; Write a procedure (lookup-table-tree key tree compare-key?) that returns the value 
; associated with key in the table if it exists, and returns #f otherwise

; Examples:
; (define test-tree (binary-table-tree test-table <=))
; (lookup-table-tree 9 test-tree <=) => "a"
; (lookup-table-tree 15 test-tree <=)=> #f

; (define test-translation (binary-table-tree translate-table string<=?))
; (lookup-table-tree "welsh" test-translation string<=?) => "gwiwer"
; (lookup-table-tree "romanian" test-translation string<=?) => "veverita"
; (lookup-table-tree "english" test-translation string<=?) => #f
; ************************************************************************


(define lookup-table-tree
  (lambda (key tree compare-key?)
    (cond ((equal? (first (root tree)) key) (first (rest (root tree))))
          ((compare-key? key (first (root tree))) (if (null? (left-subtree tree))
                                 #f
                                 (lookup-table-tree key (left-subtree tree) compare-key?)))
          (else (if (null? (right-subtree tree))
                                 #f
                                 (lookup-table-tree key (right-subtree tree) compare-key?))))))


; ************************************************************************
;       _______. _______  _______    ____    ____  ______    __    __     __  .__   __.    ___     ___    ___    __     __   __
;      /       ||   ____||   ____|   \   \  /   / /  __  \  |  |  |  |   |  | |  \ |  |   |__ \   / _ \  |__ \  /_ |   |  | |  | 
;     |   (----`|  |__   |  |__       \   \/   / |  |  |  | |  |  |  |   |  | |   \|  |      ) | | | | |    ) |  | |   |  | |  | 
;      \   \    |   __|  |   __|       \_    _/  |  |  |  | |  |  |  |   |  | |  . `  |     / /  | | | |   / /   | |   |  | |  | 
;  .----)   |   |  |____ |  |____        |  |    |  `--'  | |  `--'  |   |  | |  |\   |    / /_  | |_| |  / /_   | |   |__| |__|
;  |_______/    |_______||_______|       |__|     \______/   \______/    |__| |__| \__|   |____|  \___/  |____|  |_|   (__) (__)


; ************************************************************************
; *** END OF HW #7; END OF HWs; END OF CPSC 201a, 2020!!!!!!!!!***********
; ************************************************************************
