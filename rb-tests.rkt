#lang racket
(require rackunit)
(require rackunit/text-ui)

(require racket/include)
(include "rb-starter.rkt")

(define tree1 (node 1 nil nil 'black nil))




(define lnode (node 8 nil nil 'red nil))
(define rnode (node 15 nil nil 'red nil))

(define rnode-child-left (node 5 nil nil 'black nil))
(define rnode-child-right (node 30 nil nil 'black nil))


(set-node-left! rnode rnode-child-left)
(set-node-right! rnode rnode-child-right)

; We had some problems with this not setting the parent right!!!
(set-node-parent! rnode-child-left rnode)
(set-node-parent! rnode-child-right rnode)

(define tree2 (node 10 lnode rnode 'black nil))
(set-node-parent! lnode tree2)
(set-node-parent! rnode tree2)

; It seems as the the left rotation is supposed to be done so that we are really rotating y. Instead of passing y, we must pass y's parents??
(define my-tree (tree tree2))
(left-rotate! my-tree tree2)

(print)
(print)
(print "root: " (node-val (tree-root my-tree)))
(print "left/right of root: " (node-val (node-left (tree-root my-tree))) (node-val (node-right (tree-root my-tree))))
(print "left/right of left of root: " (node-val (node-left (node-left (tree-root my-tree)))) (node-val (node-right (node-left (tree-root my-tree)))))
(print "left/right of right of root: " (node-val (node-left (node-right (tree-root my-tree)))) (node-val (node-right (node-right (tree-root my-tree)))))







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