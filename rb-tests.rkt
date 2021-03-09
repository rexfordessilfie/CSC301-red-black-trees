#lang racket
(require rackunit)
(require rackunit/text-ui)

(require racket/include)
(include "nameera-starter.rkt")

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


; Inserting into an empty tree
(define test-tree1 nil)
(define test-tree1-x (tree test-tree1))
(rb-insert! test-tree1-x (node-copy new-node-A))

; Inserting into tree with only root
(define test-tree2-x (tree-copy test-tree1-x))
;(print "^^^^^^^^^^^^^^^^^^^^^^^^^^^")
;(print-tree test-tree2-x)
;(print "^^^^^^COPIED TREE 2^^^^^^^^^^")
(rb-insert! test-tree2-x (node-copy new-node-B))
;(print-tree test-tree2-x)
;(print "^^^^^^AFTER INSERT TREE 2^^^^^^^^^^")


; Inserting into tree with root and left node
(define test-tree3-x (tree-copy test-tree2-x))
;(print "^^^^^^^^^^^^^^^^^^^^^^^^^^^")
;(print-tree test-tree3-x)
;(print "^^^^^^COPIED TREE 3^^^^^^^^^^")
(rb-insert! test-tree3-x (node-copy new-node-C))
;(print-tree test-tree3-x)
;(print "^^^^^^AFTER INSERT TREE 3^^^^^^^^^^")

; Tree 4
(define test-tree4-x (tree-copy test-tree3-x))
(rb-insert! test-tree4-x (node-copy new-node-D))




; Expected result 1
(define expected1 (node-copy new-node-A))
(define expected1-x (tree expected1))

; Expected result 2
(define expected2 (node-copy expected1))
(define lnode1 (node-copy new-node-B))
(set-node-color! lnode1 'red)
(set-node-parent! lnode1 expected2)
(set-node-left! expected2 lnode1)
(define expected2-x (tree expected2))

; Expected result 3
(define expected3 (node-copy expected2))
(define rnode1 (node-copy new-node-C))
(set-node-color! rnode1 'red)
(set-node-parent! rnode1 expected3)
(set-node-right! expected3 rnode1)
(define expected3-x (tree expected3))


; Expected result 4
(define expected4 (node-copy expected3))
(define lnode1-left (node-copy new-node-D))
(set-node-color! lnode1-left 'red)
(set-node-parent! lnode1-left (node-left expected4))
(set-node-left! (node-left expected4) lnode1-left)
(define expected4-x (tree expected4))


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

(print"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
(print-tree baby7)
;
;(rb-insert! baby-tree (node-copy new-node-G))` ; 47
;(rb-insert! baby-tree (node-copy new-node-H)) ; 10
;(rb-insert! baby-tree (node-copy new-node-I)) ; 16
;(rb-insert! baby-tree (node-copy new-node-J)) ; 19
;(rb-insert! baby-tree (node-copy new-node-K)) ; 23
;(rb-insert! baby-tree (node-copy new-node-L)) ; 28


(define boo-tree (tree-copy baby-tree))
(rb-check-equals? boo-tree baby-tree)



; A, B, H, N

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
    (check-true (tree-contains? (tree-root baby7) 20))
    (check-true (tree-contains? (tree-root baby7) 11))
    (check-true (black-node? (node-right (tree-root baby7))))

    ;;;; What other tests can we do here? Might be easy if we have something to just check binary tree and rb tree properties for us
  )

  
;  (test-case
;   "Inserting into a tree with one node"
;   (check-equal? (rb-check-equals? test-tree2-x expected2-x) #t)
;   )
;   (test-case
;   "Inserting into a tree with one node and left child"
;   (check-equal? (rb-check-equals? test-tree3-x expected3-x) #t)
;   )
;   (test-case
;   "Inserting into tree 4"
;   (check-equal? (rb-check-equals? test-tree4-x expected4-x) #t)
;   )
  ))

;(run-tests tests-starter-code) (newline)
(run-tests tests-new-code) (newline)




; new strategy. Define the tree before insert. Insert the node?
; define the tree after insert nd check that they are equal?