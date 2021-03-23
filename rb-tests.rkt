#lang racket
(require rackunit)
(require rackunit/text-ui)

(require racket/include)
(include "rb-starter.rkt")

(define tree1 (node 1 nil nil 'black nil))
(define lnode (node 8 nil nil 'red nil))
(define rnode (node 15 nil nil 'red nil))
(define tree2 (node 10 lnode rnode 'black nil))
(set-node-parent! lnode tree2)
(set-node-parent! rnode tree2)



; TESTS
(define new-node-A (node 266 nil nil 'black nil)) 
(define new-node-B (node 17 nil nil 'black nil))
(define new-node-C (node 14 nil nil 'black nil))
(define new-node-D (node 1 nil nil 'black nil))
(define new-node-E (node 15 nil nil 'black nil))
(define new-node-F (node 16 nil nil 'black nil))
(define new-node-G (node 18 nil nil 'black nil))
(define new-node-H (node 20 nil nil 'black nil))
(define new-node-I (node 12 nil nil 'black nil))
(define new-node-J (node 11 nil nil 'black nil))
(define new-node-K (node 10 nil nil 'black nil))
(define new-node-L (node 9 nil nil 'black nil))
(define new-node-M (node 300 nil nil 'black nil))
(define new-node-N (node 7 nil nil 'black nil))
(define new-node-O (node 5 nil nil 'black nil))
(define new-node-P (node 15 nil nil 'black nil))
(define new-node-Q (node 20 nil nil 'black nil))
(define new-node-R (node 35 nil nil 'black nil))
(define new-node-S (node 39 nil nil 'black nil))
(define new-node-T (node 8 nil nil 'black nil))



#|
Insert elements into a big tree and then delete them one by one
After each deletion/insert, we check that the trees are what we expect them to be
|#



; These tests should all pass with the given starter code
(define tests-starter-code
  (test-suite
   "Tests of the given code"
   (test-case
    "given constructors"
    (check-equal? (node-left tree1) nil)
    (check-equal? (node-right tree1) nil)
    (check-equal? (node-parent tree1) nil)
    (check-equal? (node-color tree1) 'black)
    (check-equal? (node-val tree1) 1)
    (check-equal? (node-left nil) nil)
    (check-equal? (node-right nil) nil)
    (check-equal? (node-parent nil) nil)
    (check-equal? (node-color nil) 'black)
    (check-equal? (node-val nil) #f)
    )
   (test-case
    "given predicates"
    (check-true (nil? nil))
    (check-false (nil? tree1))
    (check-false (nil? tree2))
    (check-false (red-node? tree1))
    (check-false (red-node? tree2))
    (check-true (red-node? (node-left tree2)))
    (check-true (red-node? (node-right tree2)))
    (check-true (black-node? tree1))
    (check-true (black-node? tree2))
    (check-false (black-node? (node-left tree2)))
    (check-false (black-node? (node-right tree2)))
    (check-true (tree-contains? tree1 1))
    (check-false (tree-contains? tree1 10))
    (check-true (tree-contains? tree2 10))
    (check-true (tree-contains? tree2 8))
    (check-true (tree-contains? tree2 15))
    (check-false (tree-contains? tree2 2))
    )
   (test-case
    "More"
    (check-equal? tree1 (minimum tree1))
    (check-equal? (node-left tree2) (minimum tree2))
    (check-equal? tree1 (maximum tree1))
    (check-equal? (node-right tree2) (maximum tree2))
    (check-equal? (node-left tree2) (maximum (node-left tree2)))
    (check-equal? (node-right tree2) (maximum (node-right tree2)))
    (check-equal? (successor tree1) nil)
    (check-equal? (successor tree2) (node-right tree2))
    (check-equal? (successor (node-left tree2)) tree2)
    (check-equal? (predecessor tree1) nil)
    (check-equal? (predecessor tree2) (node-left tree2))
    (check-equal? (predecessor (node-left tree2)) nil)
    (check-equal? (predecessor (node-right tree2)) tree2)
    )
   ))


; These tests should pass after writing your new code

(define baby nil)
(define baby-tree (tree baby))
(rb-insert! baby-tree (node-copy new-node-A))
(rb-insert! baby-tree (node-copy new-node-A))
(rb-insert! baby-tree (node-copy new-node-B))
(rb-insert! baby-tree (node-copy new-node-C))
(rb-insert! baby-tree (node-copy new-node-D)) ; 14
(rb-insert! baby-tree (node-copy new-node-E))
 
(rb-insert! baby-tree (node-copy new-node-F)) ; 30
(rb-insert! baby-tree (node-copy new-node-G)) ; 47
(rb-insert! baby-tree (node-copy new-node-H)) ; 10
(rb-insert! baby-tree (node-copy new-node-I)) ; 16
(rb-insert! baby-tree (node-copy new-node-J)) ; 19
(rb-insert! baby-tree (node-copy new-node-K)) ; 23
(rb-insert! baby-tree (node-copy new-node-L)) ; 28

(print "-------------------------------------------")

(define baby1 (tree nil))
(rb-insert! baby1 (node-copy new-node-A)) ; 266


(define baby2 (tree nil))
(rb-insert! baby2 (node-copy new-node-A))
(rb-insert! baby2 (node-copy new-node-B))
(rb-insert! baby2 (node-copy new-node-C))


(define baby3 (tree nil))
(rb-insert! baby3 (node-copy new-node-A))
(rb-insert! baby3 (node-copy new-node-B))
(rb-insert! baby3 (node-copy new-node-C))
(rb-insert! baby3 (node-copy new-node-D))

(define baby4 (tree nil))
(rb-insert! baby4 (node-copy new-node-A))
(rb-insert! baby4 (node-copy new-node-B))
(rb-insert! baby4 (node-copy new-node-C))
(rb-insert! baby4 (node-copy new-node-D))
(rb-insert! baby4 (node-copy new-node-E))


(define baby5 (tree nil))
(rb-insert! baby5 (node-copy new-node-A))
(rb-insert! baby5 (node-copy new-node-B))
(rb-insert! baby5 (node-copy new-node-C))
(rb-insert! baby5 (node-copy new-node-D))
(rb-insert! baby5 (node-copy new-node-E))
(rb-insert! baby5 (node-copy new-node-F))


(define baby6 (tree nil))
(rb-insert! baby6 (node-copy new-node-A))
(rb-insert! baby6 (node-copy new-node-B))
(rb-insert! baby6 (node-copy new-node-C))
(rb-insert! baby6 (node-copy new-node-D))
(rb-insert! baby6 (node-copy new-node-E))
(rb-insert! baby6 (node-copy new-node-F))
(rb-insert! baby6 (node-copy new-node-G))
(rb-insert! baby6 (node-copy new-node-H))



(define baby7 (tree nil))
(rb-insert! baby7 (node-copy new-node-A))
(rb-insert! baby7 (node-copy new-node-B))
(rb-insert! baby7 (node-copy new-node-C))
(rb-insert! baby7 (node-copy new-node-D))
(rb-insert! baby7 (node-copy new-node-E))
(rb-insert! baby7 (node-copy new-node-F))
(rb-insert! baby7 (node-copy new-node-G))
(rb-insert! baby7 (node-copy new-node-H))
(rb-insert! baby7 (node-copy new-node-I))
(rb-insert! baby7 (node-copy new-node-J))



(define full-tree (tree nil))
(define some-node (node-copy new-node-C))
(rb-insert! full-tree (node-copy new-node-A))
(rb-insert! full-tree (node-copy new-node-B))
(rb-insert! full-tree (node-copy new-node-C))
(rb-insert! full-tree (node-copy new-node-D))
(rb-insert! full-tree (node-copy new-node-E))
(rb-insert! full-tree (node-copy new-node-F))
(rb-insert! full-tree (node-copy new-node-G))
(rb-insert! full-tree (node-copy new-node-H))
(rb-insert! full-tree (node-copy new-node-I))
(rb-insert! full-tree (node-copy new-node-J))
(rb-insert! full-tree (node-copy new-node-K))
(rb-insert! full-tree (node-copy new-node-L))

(print"BEFORE:@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
(print-tree full-tree)
;

(rb-delete! full-tree (node-right (tree-root full-tree)))

;;; (rb-delete! full-tree new-node-B)
;;; (rb-delete! full-tree new-node-C)
;;; (rb-delete! full-tree new-node-D)
;;; (rb-delete! full-tree new-node-E)
;;; (rb-delete! full-tree new-node-F)
;;; (rb-delete! full-tree new-node-G)
;;; (rb-delete! full-tree new-node-H)
;;; (rb-delete! full-tree new-node-I)
;;; (rb-delete! full-tree new-node-J)
;;; (rb-delete! full-tree new-node-K)


(print"AFTER:@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
(print-tree full-tree)

(define tests-new-code
  (test-suite
   "Tests of insert"
  (test-case
    "inserting into empty tree"
    (check-equal? (and (equal?(node-val(tree-root baby1)) 266) (equal? 'black (node-color (tree-root baby1)))) #t) 
;    "Inserting into an empty tree"
;    (check-equal? (rb-check-equals? test-tree1-x expected1-x) #t)
   )
   
   (test-case
    "case 3A y is not red"
    (check-true (equal?(node-val (node-left (tree-root baby2))) 14) )
    (check-true (red-node? (node-left (tree-root baby2))))
    (check-true(and (equal? (node-val (tree-root baby2)) 17) (black-node? (tree-root baby2))))
    (check-equal? (and (equal?(node-val(node-right (tree-root baby2))) 266) (red-node? (node-right (tree-root baby2)))) #t)
;    "Inserting into an empty tree"
;    (check-equal? (rb-check-equals? test-tree1-x expected1-x) #t)
   )
    (test-case
    "case 1A y is red"
    (check-true (equal? (node-val (minimum (tree-root baby3))) 1))
    (check-true (red-node? (minimum(tree-root baby3))))
    (check-true (black-node? (node-left (tree-root baby3))));14 has turned red, we are not violating the "no red parent and child" property
   )
    (test-case
    "case 3A y is not red"
    (check-true (equal? (node-val (minimum (tree-root baby3))) 1))
    (check-true (red-node? (minimum(tree-root baby3))))
    (check-true (black-node? (node-left (tree-root baby3))));14 has turned red, we are not violating the "no red parent and child" property
   )
   (test-case
    "case 1B y is red and y is uncle on the left side"
    (check-true (equal? (node-val (node-right(node-left (tree-root baby4)))) 15))
    (check-true (red-node? (node-right(node-left (tree-root baby4))))))
  
  (test-case
    "case 2A z is on the right side of it's parent"
    (check-true (equal? (node-val (node-right (node-right (node-left (tree-root baby5))))) 16))
    (check-true (red-node? (node-right (node-right (node-left (tree-root baby5))))))
  )


  (test-case
    "case 3B: y is not red and z is on the right side of its parent"
    (check-true (tree-contains? (tree-root baby6) 20))
    (check-true (black-node? (node-right (tree-root baby6))))
    (check-true (equal? (node-val (node-right (tree-root baby6))) 20))
    (check-true (equal? (node-val (node-right (node-right (tree-root baby6)))) 266))
    (check-true (red-node? (node-right (node-right (tree-root baby6)))))
    (check-true (equal? (node-val (node-left (node-right (tree-root baby6)))) 18))
    (check-true (red-node? (node-left (node-right (tree-root baby6)))))
  )
    


  (test-case
    "case 2B & 3B: z's parent is on its right side, z is on the left side of its parent and y is not red"
    (check-true (tree-contains? (tree-root baby7) 18))
    (check-true (tree-contains? (tree-root baby7) 11))
    (check-true (black-node? (node-left (node-left (tree-root baby7)))))
    (check-true (red-node? (node-parent (node-left (node-left (tree-root baby7))))))

    ;;;; What other tests can we do here? Might be easy if we have something to just check binary tree and rb tree properties for us
    (check-true (equal? (node-val (node-left (node-left (node-left (tree-root baby7))))) 1))
    (check-true (equal? (node-val (node-parent (node-left (node-left (tree-root baby7))))) 14))

  )


  ;;; DELETIONS


  ))


;(run-tests tests-starter-code) (newline)
(run-tests tests-new-code) (newline)




; new strategy. Define the tree before insert. Insert the node?
; define the tree after insert nd check that they are equal?