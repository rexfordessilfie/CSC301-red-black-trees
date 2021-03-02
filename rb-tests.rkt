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
(define test-tree1 nil)
(define test-tree1-x (tree test-tree1))

(define new-node-A (node 26 nil nil 'black nil))
(define new-node-B (node 17 nil nil 'black nil))
(define new-node-C (node 41 nil nil 'black nil))
(define new-node-D (node 14 nil nil 'black nil))
(define new-node-E (node 21 nil nil 'black nil))
(define new-node-F (node 30 nil nil 'black nil))
(define new-node-G (node 47 nil nil 'black nil))
(define new-node-H (node 10 nil nil 'black nil))
(define new-node-I (node 16 nil nil 'black nil))
(define new-node-J (node 19 nil nil 'black nil))
(define new-node-K (node 23 nil nil 'black nil))
(define new-node-L (node 28 nil nil 'black nil))
(define new-node-M (node 38 nil nil 'black nil))
(define new-node-N (node 7 nil nil 'black nil))
(define new-node-O (node 12 nil nil 'black nil))
(define new-node-P (node 15 nil nil 'black nil))
(define new-node-Q (node 20 nil nil 'black nil))
(define new-node-R (node 35 nil nil 'black nil))
(define new-node-S (node 39 nil nil 'black nil))
(define new-node-T (node 3 nil nil 'black nil))


; Inserting into an empty tree
(rb-insert! test-tree1-x new-node-A)

(define test-tree2-x test-tree1-x)
(rb-insert! test-tree2-x new-node-B)




; Expected result 1
(define expected1 new-node-A)
(define expected1-x (tree new-node-A))

; Expected result 2
(define expected2 new-node-A)
(define lnode1 new-node-B)
(set-node-color! lnode1 'red)
(set-node-parent! lnode1 expected2)

(define expected2-x (tree expected2))

; TODO
; Reimplement while loop
; Write some tests to make sure everything is working as expected

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

(define tests-new-code
  (test-suite
   "Tests of new code"
   (test-case
    "Inserting into an empty tree"
    (rb-check-equals? test-tree1-x expected1-x)
   )
  (test-case
   "Inserting into a tree with one node"
   (rb-check-equals? test-tree2-x expected2-x)
   )
  ))

(run-tests tests-starter-code) (newline)
(run-tests tests-new-code) (newline)