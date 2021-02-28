#lang racket
(require rackunit)
(require rackunit/text-ui)

(require racket/include)
(include "rb-starter.rkt")

(define tree1 (node 1 nil nil 'black nil))




(define lnode (node 8 nil nil 'red nil))
(define rnode (node 15 nil nil 'red nil))

(define rnode-child-left (node 14 nil nil 'black nil))
(define rnode-child-right (node 30 nil nil 'black nil))


(set-node-left! rnode rnode-child-left)
(set-node-right! rnode rnode-child-right)

; We had some problems with this not setting the parent right!!!
(set-node-parent! rnode-child-left rnode)
(set-node-parent! rnode-child-right rnode)

(define tree2 (node 10 lnode rnode 'black nil))
(set-node-parent! lnode tree2)
(set-node-parent! rnode tree2)

(define my-tree2 (tree tree2))
(define my-tree1 (tree tree1))
(define copyof-my-tree2 my-tree2)

;(right-rotate! my-tree (node-right tree2))


; Insert on an existing tree
(define my-new-node  (node 2 nil nil 'red nil)) 
;(rb-insert my-tree2 my-new-node)


;Insert on an empty tree
(define tree-empty nil)
(define my-tree-empty (tree tree-empty))

(define copyof-my-tree-empty my-tree-empty)

(rb-insert! my-tree-empty my-new-node)
(rb-delete! my-tree-empty my-new-node)



; Insert on an existing tree with multiple nodes
(rb-insert! my-tree2 my-new-node)
(rb-delete! my-tree2 my-new-node)


; NB: Check the colors on this. The colors after our operations do not seem to be correct!! But possibly because we are manually inserting them in :/
(rb-check-equal-trees? my-tree2 copyof-my-tree2)


; Different trees for testing
#|
Insert elements into a big tree and then delete them one by one
After each deletion/insert, we check that the trees are what we expect them to be

Insertion:

Deletion:


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
#|
(define tests-new-code
  (test-suite
   "new-code"
   (test-case
    "test case name here"
    ;test 1
    ;test 2
   )
  (test-case
   "another test name here"
   ;test 1
   ;test 2
   )
  ))
|#

;(run-tests tests-starter-code) (newline)
;(run-tests tests-new-code) (newline)