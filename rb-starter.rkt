;#lang racket

(require csc151)
(require racket/match)


; +--------------+---------------------------------------------------
; | Constructors |
; +--------------+

(struct tree (root)
  #:mutable)

;;;; Procedure:
;;;   node
;;; Parameters:
;;;   val, a value (can assume it's a number)
;;;   left, a node
;;;   right, a node
;;;   color, 'red or 'black
;;;   parent, a node
;;; Purpose:
;;;   Create a red-node in a red black tree.
;;; Produces:
;;;   a node, which acts as a part of a red-black tree
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (node-left node) = left-subtree.
;;;   (node-left node) = right-subtree.
;;;   (node-val node) = val (the value you want the node to hold).
;;;   (node-color node) = the color of the node.
;;;   (node-parent node) = the parent of the node.
(struct node (val 
              left 
              right 
              color 
              parent )
  #:mutable)

;;; Procedure:
;;;   nil
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Create a nil node for a red-black tree
;;;   As CLRS describes, we use this to designate the bottom of the tree
;;; Produces:
;;;   a nil node
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (node-left nil) = nil
;;;   (node-left nil) = nil
;;;   (node-val nil) = #f
;;;   (node-color nil) = 'black
;;;   (node-parent nil) = nil
(define nil (let ([v (node #f #f #f 'black #f)])
              (set-node-parent! v v)
              (set-node-left! v v)
              (set-node-right! v v)
              v))

; +------------+-----------------------------------------------------
; | Predicates |
; +------------+

;;; Procedure:
;;;   nil?
;;; Parameters:
;;;   node
;;; Purpose:
;;;   Tell us if we're at the distinguished nil node.
;;; Produces:
;;;   is-nil?, a boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]
(define-syntax (nil? stx)
  (syntax-case stx ()
    [(_ x)
     (syntax/loc stx 
       (eq? x nil))]
    [(_ . args)
     (syntax/loc stx
       (nil?/proc . args))]
    [_
     (identifier? stx)
     #'nil?/proc]))


;;; Procedure:
;;;   red-node?
;;; Parameters:
;;;   n, a node
;;; Purpose:
;;;   Determine if n is a red node
;;; Produces:
;;;   is-red?, a Boolean
;;; Preconditions:
;;;   n must be a node
;;; Postconditions:
;;;   Returns true if n is a red node
(define red-node?
  (lambda (n)
    (eq? (node-color n) 'red)))

;;; Procedure:
;;;   black-node?
;;; Parameters:
;;;   n, a node
;;; Purpose:
;;;   Determine if n is a black node
;;; Produces:
;;;   is-black?, a Boolean
;;; Preconditions:
;;;   n must be a node
;;; Postconditions:
;;;   Returns true if n is a black node
(define black-node?
  (lambda (n)
    (eq? (node-color n) 'black)))


;;; Procedure:
;;;   tree-contains?
;;; Parameters:
;;;   tree, a red-black tree
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val appears in tree
;;; Produces:
;;;   contains?, a Boolean value
(define tree-contains?
  (lambda (node val)
    (let ([root (node-val node)]
          )
      (and (not (nil? node))
           (or (equal? root val)
               (tree-contains? (node-left node) val)
               (tree-contains? (node-right node) val))))
    ))



; +------------+-----------------------------------------------------
; | More |
; +------------+
;;; Procedure:
;;;   minimum
;;; Parameters:
;;;   n, a node
;;; Purpose:
;;;   Looks for the minimum element of the tree rooted at n.
;;; Produces:
;;;   n, a node
(define (minimum n)
  (let kernel ([n n])
    (define left (node-left n))
    (cond
      [(nil? left) n]
      [else (kernel left)])
    ))

;;; Procedure:
;;;   maximum
;;; Parameters:
;;;   n, a node
;;; Purpose:
;;;   Looks for the maximum element of the tree rooted at n.
;;; Produces:
;;;   n, a node
(define (maximum n)
  (let kernel ([n n])
    (define right (node-right n))
    (cond
      [(nil? right) n]
      [else(kernel right)])
    ))

;;; Procedure:
;;;   successor
;;; Parameters:
;;;   x, a node
;;; Purpose:
;;;   Returns the successor of x in the tree
;;; Produces:
;;;   n, a node
(define (successor x)
  (cond [(not (nil? (node-right x)))
         (minimum (node-right x))]
        [else
         (let kernel ([x x]
                      [y (node-parent x)])
           (cond
             [(and (not (nil? y)) (eq? x (node-right y)))
              (kernel y (node-parent y))]
             [else y]))]))

;;; Procedure:
;;;   predeccessor
;;; Parameters:
;;;   x, a node
;;; Purpose:
;;;   Returns the predeccessor of x in the tree
;;; Produces:
;;;   n, a node
;;; Preconditions:
;;;
;;; Postconditions:
;;; If there is no predecessor, returns the nil node.
(define (predecessor x)
  (cond [(not (nil? (node-left x)))
         (maximum (node-left x))]
        [else
         (let kernel ([x x]
                      [y (node-parent x)])
           (cond
             [(and (not (nil? y)) (eq? x (node-left y)))
              (kernel y (node-parent y))]
             [else
              y]))]
        ))

; +---------------------+---------------------------------------------
; | Your code goes here |
; +---------------------+



;;; Procedure:
;;;   rb-transplant
;;; Parameters:
;;;   Tree, a tree
;;;   u, a node
;;;   v, a node
;;; Purpose:
;;;   Replaces one subtree as a child of its parent with another subtree
;;; Produces:
;;;   [None]. Causes a side-effect where the subtree rooted at node u is replaced with the subtree rooted at node v.
;;; Preconditions:
;;;   [None]
;;; Postconditions:
;;;   The node, u is no longer present in the tree
;;;   The parent of u, now points to v
;;;   If u is nil, then z becomes the root of the tree
(define rb-transplant!
  (lambda (Tree u v)    
    (cond
      [(nil? (node-parent u))
       (set-tree-root! Tree v)
       ]
      [(equal? u (node-left (node-parent u)))
       (set-node-left! (node-parent u) v)
       ]
      [else
       (set-node-right! (node-parent u) v)
       ]
      )
    (set-node-parent! v (node-parent u))
    ))


;;; Procedure:
;;;   rb-insert-fixup
;;; Parameters:
;;;   Tree, a red-black tree
;;;   z, a node
;;; Purpose:
;;;   Restores the red-black properties of Tree after an insert
;;; Produces:
;;;   [None]. Causes a side-effect on Tree, where it is modified to maintain red-black tree properties
;;; Preconditions:
;;;   z must be a node in Tree
;;; Postconditions:
;;;   All the node values originally in tree remain in tree
;;;   Tree maintains properties 2 and 4 of red-black trees
(define rb-insert-fixup!
  (lambda (Tree z)
    ;(print "INSIDE INSERT_FIXUP")
    
    (let kernel ()

    (if (red-node? (node-parent z)) ; WHILE
      (begin
        ;(print "Entering while")
          (cond 
            [(equal? (node-parent z) (node-left (node-parent (node-parent z))))
              ;(print "z's parent is on the left side")
              (define y (node-right (node-parent (node-parent z))))
              (cond
                [(red-node? y)
                 (print "CASE 1A: y is red")
                 (set-node-color! (node-parent z) 'black)
                 (set-node-color! y 'black)
                 (set-node-color! (node-parent (node-parent z)) 'red)
                 (set! z (node-parent (node-parent z)))
                 ]

                [else

                 (cond
                   [(equal? z (node-right (node-parent z)))
                    (print "CASE 2A: z is on the right side of it's parent")
                    
                    (set! z (node-parent z))
                    (left-rotate! Tree z)
                    ]
                   [else (void)]
                   )

                 (print "CASE 3A")
                 (set-node-color! (node-parent z) 'black)
                 (set-node-color! (node-parent (node-parent z) )'red)
                 (right-rotate! Tree (node-parent (node-parent z)))

                 ]
              )]

             [else ; ELSE
              ;(print "z's parent is on the right side")
              (define y (node-left (node-parent (node-parent z))))
              (cond
                [(red-node? y)
                 (print "CASE 1B: y is red")
                 (set-node-color! (node-parent z) 'black)
                 (set-node-color! y 'black)
                 (set-node-color! (node-parent (node-parent z)) 'red)
                 (set! z (node-parent (node-parent z)))
                 ]

                [else
                 (cond
                   [(equal? z (node-left (node-parent z)))
                    (print "CASE 2B: z is on the left side of it's parent")
                    (set! z (node-parent z))

                    (print "---------> Printing Tree before Left Rotate in Case 2B")
                    (print-tree Tree)

                    (right-rotate! Tree z)

                    (print "---------> Printing Tree after Case 2B")
                    (print-tree Tree)
                    ]
                   [else (void)]
                   )

                 (print "CASE 3B: y is not red")
                 (set-node-color! (node-parent z) 'black)
                 (set-node-color! (node-parent (node-parent z)) 'red)
                 (left-rotate! Tree (node-parent (node-parent z)))

                 ;(print "---------> Printing Tree after Case 3B")
                 ;(print-tree Tree)
                 ]
              )
             ]
           )
        (kernel)
      ) 
        (void) ; Else do nothing
      )
    )

    ;(define T (tree-root Tree)) ; Gets the node at the tree's root
    (set-node-color! (tree-root Tree) 'black)
  ))


;;; Procedure:
;;;   rb-insert!
;;; Parameters:
;;;   Tree, a tree
;;;   z, a node
;;; Purpose:
;;;   Inserts the node, z into Tree, maintaining all red-black tree properties
;;; Produces:
;;;   [None]. Causes a side effect on Tree, such that it contains the node z, and maintains red-black tree properties
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   All node values originally in Tree remain in tree
;;;   z is now a node in Tree
;;;   Tree maintains red-black tree properties
(define rb-insert!
  (lambda (Tree z)

    ;(print "BEGIN RB-INSERT")

    (define y nil)
    (define x (tree-root Tree))

    ;(print "x is: " (node-val x))
    ;(print "y is: " (node-val y))
    ;(print "inserting : " (node-val z))


    
    (let kernel ()
      (if (not (nil? x))
          (begin
            ;(print "INSIDE LET")
            ;(print "before --> y is: " (node-val y))
            (set! y x)
            ;(print "after --> y is: " (node-val y))

            ;(print "x right is: " (node-val (node-right x)))
            ;(print "x left is: "  (node-val (node-left x)))
            ;(print "before --> x is: " (node-val x))
            (if (< (node-val z) (node-val x))
                (set! x (node-left x))
                (set! x (node-right x))
                )
            ;(print "after --> x is: " (node-val x))

            (kernel)
            )
          (void))
      )
 
    ;;(print)
    ;(print "before --> parent of z is: " (node-val (node-parent z)))
    (set-node-parent! z y)
    ;(print "after --> parent of z is: " (node-val (node-parent z)))

    (cond
      [(nil? y)
       ;(print "Entering Cond 1")
       ;(print "before --> T is: " (node-val (tree-root Tree)))
       (set-tree-root! Tree z)
       ;(print "after --> T is: " (node-val (tree-root Tree)))
       ]

      [(< (node-val z) (node-val y))
       ;(print "Entering Cond 2")
       
       ;(print "before --> left of y is: " (node-val (node-left y)))
       (set-node-left! y z)
       ;(print "after --> left of y is: " (node-val (node-left y)))
       ]

      [else
       ;(print "Entering Cond 3")
       ;(print "before --> right of y is: " (node-val (node-right y)))
       (set-node-right! y z)
       ;(print "before --> right of y is: " (node-val (node-right y)))
       ]
      )


    ; Set z's children to nil
    (set-node-left! z nil)
    (set-node-right! z nil)

    (set-node-color! z 'red)
    
    ;(print "---------> Printing Tree Before Fixup")
    ;(print-tree Tree)

    (rb-insert-fixup! Tree z)
    ;(print "END RB-INSERT")
    ;(print "RESULT: ")
    ;(print-tree Tree)
    ;(print)
    ))


;;; Procedure:
;;;   rb-delete-fixup!
;;; Parameters:
;;;   Tree, a tree
;;;   x, a node
;;; Purpose:
;;;   Restores the red-black tree properties of a red-black tree given the subtree x, that was transplanted
;;; Produces:
;;;   [None]. Causes a side effect on the tree, where red-black tree properties of Tree are restored
;;; Preconditions:
;;;   Tree must not be empty
;;;   x must be a subtree in Tree
;;; Postconditions:
;;;   All the node values originally in tree remain in tree
;;;   Red-black tree properties 1, 2 and 4 of Tree are restored
(define rb-delete-fixup!
  (lambda (Tree x)
    ;(print "INSIDE DELETE_FIXUP")
    (let kernel ()

      (print "x is: " (node-val x))
      (print "x's color is: " (node-color x))

       
      (if (and (not (nil? x)) (black-node? x))
        (begin
           (print "I entered the if!!!")
           (cond
             [(equal? x (node-left (node-parent x)))
              (define w (node-right (node-parent x)))
              (print "x is on the left side of it's parent")

              (cond
                [(red-node? w)
                 (print "CASE 1A: w (x's sibling) is red")
                 (set-node-color! w 'black)
                 (set-node-color! (node-parent x) 'red)
                 (left-rotate! Tree (node-parent x))
                 (set! w (node-right (node-parent x)))
                 ]
                [(and (equal? (black-node? (node-left w))) (black-node? (node-right w)))
                 (print "CASE 2A: w (x's sibling) is black and both w's children are black")
                 (set-node-color! w 'red)
                 (set! x (node-parent x))]
                [else
                 (if (black-node? (node-right w))
                     (begin
                     (print "CASE 3A: w is black and right of w is black and left of w is red")
                       (set-node-color! (node-left w) 'black)
                       (set-node-color! w 'red)
                       (right-rotate! Tree w)
                       (set! w (node-right (node-parent x)))
                       )
                     #f)
                 (print "CASE 4A: x's sibling, w is black and w's right child is red")
                 (set-node-color! w (node-color (node-parent x)))
                 (set-node-color! (node-parent x) 'black)
                 (set-node-color! (node-right w) 'black)
                 (left-rotate! Tree (node-parent x))
                 (set! x (tree-root Tree))
                 ])
              ]
             [else
              (print "x is on the right side of it's parent")
              (define w (node-left (node-parent x)))

              (cond
                [(red-node? w)
                 (print "CASE 1B: w (x's sibling) is red")
                 (set-node-color! w 'black)
                 (set-node-color! (node-parent x) 'red)
                 (right-rotate! Tree (node-parent x))
                 (set! w (node-left (node-parent x)))
                 ]
                [(and (equal? (black-node? (node-right w))) (black-node? (node-left w)))
                 (print "CASE 2B: w (x's sibling) is black and both w's children are black")
                 (set-node-color! w 'red)
                 (set! x (node-parent x))]
                [else
                 (if (black-node? (node-left w))
                     (begin
                       (print "CASE 3B: w is black and left of w is black and right of w is red")
                       (set-node-color! (node-right w) 'black)
                       (set-node-color! w 'red)
                       (left-rotate! Tree w)
                       (set! w (node-left (node-parent x)))
                       )
                     #f)
                (print "CASE 4B: x's sibling, w is black and w's right child is red")
                 (set-node-color! w (node-color (node-parent x)))
                 (set-node-color! (node-parent x) 'black)
                 (set-node-color! (node-left w) 'black)
                 (right-rotate! Tree (node-parent x))
                 (set! x (tree-root Tree))
                 ])

              ])
           (kernel)
        ) 
        (print "I did not enter the if statement!!"))
    )
           
    (set-node-color! x 'black)
    ))


;;; Procedure:
;;;   rb-delete!
;;; Parameters:
;;;   Tree, a tree
;;;   z, a value
;;; Purpose:
;;;   Deletes a node from a red-black tree, keeping the tree balanced
;;; Produces:
;;;   None. But causes a side effect on the given tree
;;; Preconditions:
;;;   The node, z, should be a valid node in the tree
;;; Postconditions:
;;;   The node, z, no longer exists in the tree
;;;   The properties of red-black trees are maintained
(define rb-delete!
  (lambda (Tree z)
    (print "DELETING " (node-val z))

    (define y z)
    (define x nil)

    (define y-original-color (node-color y))
    (print "y-original-color is: " y-original-color)

    (cond
      [(nil? (node-left z))
       (print "Left of z is nil")
       (print "---->Before, x is: " (node-val x))
       (set! x (node-right z))
       (print "------>After, x is: " (node-val x))
       (rb-transplant! Tree z (node-right z))
       ]
      [(nil? (node-right z))
      (print "Right of z is nil")
       (set! x (node-left z))
       (rb-transplant! Tree z (node-left z))
       ]
      [else
       (print "Both Left and Right of z are not nil")
       (set! y (minimum (node-right z)))
       (set! y-original-color (node-color y)) 
       (print "y-original-color is: " y-original-color)
       (set! x (node-right y))

       (if (equal? (node-parent y) z)
           (set-node-parent! x y)
           (rb-transplant! Tree y (node-right y))
           )

       (rb-transplant! Tree z y)
       (set-node-left! y (node-left z))
       (set-node-parent! (node-left y) y)

       (set-node-color! y (node-color z))
       ]
      )

    (print "before-if: y-original-color is: " y-original-color)
    (if (equal? y-original-color 'black)
        (rb-delete-fixup! Tree x) 
        (void))

    (print "EXITING DELETE")
    (print)
    (print)
    ))
  

;;; Procedure:
;;;   left-rotate!
;;; Parameters:
;;;   Tree, a tree (representing a red-black tree)
;;;   x, a node (representing the node in the tree to be rotated)
;;; Purpose:
;;;   Performs a left rotation on the node, x in a binary tree
;;; Produces:
;;;   [None]. Causes a side-effect on the tree
;;; Preconditions:
;;;   The right node of x is not nil
;;;   The parent of the root of Tree is nil
;;; Postconditions:
;;;   All nodes originally in the tree remain in the tree after rotation
(define left-rotate!
  (lambda (Tree x) 
    
    ;(define T (tree-root Tree)) ; Gets the node at the tree's root
    (define y (node-right x))

    ;(print "T is: " (node-val (tree-root Tree))) 
    ;(print "x is: " (node-val x))
    ;(print "y is: " (node-val y))
    ;(print)

    
    ;(print "before --> right of x is: " (node-val (node-right  x)))
    (set-node-right! x (node-left y))
    ;(print "after --> right of x is: " (node-val (node-right x)))
    ;(print)
    

    (cond
      [(not (nil? (node-left y)))

;       (print "Entering Cond Initial")

;       (print "before --> parent of left of y is: " (node-val (node-parent (node-left y))))
       (set-node-parent! (node-left y) x)
;       (print "after --> parent of left of y is: " (node-val (node-parent (node-left y))))
       ;(print)
       ]

      [else (void)]
      )
       

    (print "before --> parent of y is: " (node-val (node-parent y)))
    (set-node-parent! y (node-parent x))
    (print "after --> parent of y is: " (node-val (node-parent y)))
    ;(print)


    (cond
      [(nil? (node-parent x))
      ; (print "Entering Cond 1")
       
      ; (print "before --> T is: " (node-val (tree-root Tree)))
       (set-tree-root! Tree y)
      ; (print "after --> T is: " (node-val (tree-root Tree)))
       ;(print)
       ]
      
      [(equal? (node-left (node-parent x)) x)
      ; (print "Entering cond 2")
       
      ; (print "before --> left of parent of T is: " (node-val (node-left (node-parent (tree-root Tree)))))
       (set-node-left! (node-parent x) y)
      ; (print "after --> left of parent of T is: " (node-val (node-left (node-parent (tree-root Tree)))))
       ;(print)

       ]
      
      [else
      ; (print "Entering Cond 3")
       
      ; (print "before --> right of parent of x is: " (node-val (node-right (node-parent x))))
       (set-node-right! (node-parent x) y)
      ; (print "after --> right of parent of x is: " (node-val (node-right (node-parent x))))
       ;(print)
       ])

   ; (print "Out of Cond")
   ; (print "before --> left of y is: " (node-val (node-left y)))
    (set-node-left! y x) 
   ; (print "after --> left of y is: " (node-val (node-left y)))
    ;(print)

   ; (print "before --> parent of x is: " (node-val (node-parent x)))
    (set-node-parent! x y)
   ; (print "after --> parent of x is: " (node-val (node-parent x)))
    ;(print)
    ))


;;; Procedure:
;;;   right-rotate!
;;; Parameters:
;;;   Tree, a tree (representing a red-black tree)
;;;   x, a node (representing the node in the tree to be rotated)
;;; Purpose:
;;;   Performs a right rotation on the node, x in a binary tree
;;; Produces:
;;;   [None]. Causes a side-effect on the tree
;;; Preconditions:
;;;   The left node of x is not nil
;;;   The parent of the root of Tree is nil
;;; Postconditions:
;;;   All nodes originally in the tree remain in the tree after rotation
(define right-rotate!
  (lambda (Tree x) 
    
    ;(define T (tree-root Tree)) ; Gets the node at the tree's root
    (define y (node-left x))

    ;(print "T is: " (node-val (tree-root Tree))) 
    ;(print "x is: " (node-val x))
    ;(print "y is: " (node-val y))
    ;;(print)

    
    ;(print "before --> right of x is: " (node-val (node-right  x)))
    (set-node-left! x (node-right y))
    ;(print "after --> right of x is: " (node-val (node-right x)))
    ;;(print)
    

    (if (not (nil? (node-right y)))
        (begin 
          ;(print "Entering If Initial")
          ;(print "before --> parent of left of y is: " (node-val (node-parent (node-left y))))
          (set-node-parent! (node-right y) x)
          ;(print "after --> parent of left of y is: " (node-val (node-parent (node-left y))))
          ;;(print)
        )
        (void)
      )
       

    ;(print "before --> parent of y is: " (node-val (node-parent y)))
    (set-node-parent! y (node-parent x))
    ;(print "after --> parent of y is: " (node-val (node-parent y)))
    ;;(print)


    (cond
      [(nil? (node-parent x))
       ;(print "Entering Cond 1")
       
       ;(print "before --> T is: " (node-val (tree-root Tree)))
       (set-tree-root! Tree y)
       ;(print "after --> T is: " (node-val (tree-root Tree)))
       ;;(print)
       ]
      
      [(equal? (node-right (node-parent x)) x)
       ;(print "Entering cond 2")
       
       ;(print "before --> left of parent of T is: " (node-val (node-left (node-parent (tree-root Tree)))))
       (set-node-right! (node-parent x) y)
       ;(print "after --> left of parent of T is: " (node-val (node-left (node-parent (tree-root Tree)))))
       ;;(print)
       ]
      
      [else
       ;(print "Entering Cond 3")
       ;(print "before --> right of parent of x is: " (node-val (node-right (node-parent x))))
       (set-node-left! (node-parent x) y)
       ;(print "after --> right of parent of x is: " (node-val (node-right (node-parent x))))
       ;;(print)
       ])

    ;(print "Out of Cond")
    ;(print "before --> left of y is: " (node-val (node-left y)))
    (set-node-right! y x) 
    ;(print "after --> left of y is: " (node-val (node-left y)))
    ;;(print)

    ;(print "before --> parent of x is: " (node-val (node-parent x)))
    (set-node-parent! x y)
    ;(print "after --> parent of x is: " (node-val (node-parent x)))
    ;;(print)
    ))


;;; Procedure:
;;;   print
;;; Parameters:
;;;   label, value, value2, value3: strings
;;; Purpose:
;;;   Displays label, value, value2 and value3 on a single line followed by newline
;;; Produces:
;;;   [None]
(define print
  (lambda ([label ""] [value ""] [value2 ""] [value3 ""])
    (display label)
    (display value)
    (display " ") 
    (display value2)
    (display " ") 
    (display value3)
    (display "\n")
    ))

;;; Procedure:
;;;   print-one
;;; Parameters:
;;;   label, value: strings
;;; Purpose:
;;;   Displays label and value on the same line
;;; Produces:
;;;   [None]
(define print-one
  (lambda ([label ""] [value ""])
    (display label)
    (display value)
    ))

;;; Procedure:
;;;   print-one
;;; Parameters:
;;;   tree1, a tree
;;;   tree2,, a tree
;;; Purpose:
;;;   Checks whether two red-black trees are equal. i.e same values, and same color, printing both trees along the way
;;; Produces:
;;;   result, true if the trees are equal, false otherwise
(define rb-check-equals?
  (lambda (tree1 tree2)

    (define t1 (tree-root tree1))
    (define t2 (tree-root tree2))
    (define level 0)
    (define trees-equal #t)
    (print "-------------------/------------------")

    (let kernel ([nodeA t1]
                 [nodeB t2]
                 [level 0])
      
      (print-one "Level: " level)
      (print-one " |") 
      (print-one " NodeA: " (node-val nodeA))
      (print-one " Parent: "(node-val(node-parent nodeA)))
      (print-one " ColorA: " (node-color nodeA))
      (print-one " |") 
      (print-one " Node B: " (node-val nodeB))
      (print-one " ColorB: " (node-color nodeB))
      (display "\n")
      
      (if (not (equal? (node-val nodeA) (node-val nodeB))) (set! trees-equal #f) #f)
      
      (cond
        [(and (nil? nodeA) (nil? nodeB))
         0
         ]

        [(nil? nodeA)
         (kernel nodeA (node-left nodeB) (+ level 1))
         (kernel nodeA (node-right nodeB) (+ level 1))
         ]
        [(nil? nodeB)
         (kernel (node-left nodeA) nodeB (+ level 1))
         (kernel (node-right nodeA) nodeB (+ level 1))
         ]
        [else
         (kernel (node-left nodeA) (node-left nodeB) (+ level 1))
         (kernel (node-right nodeA) (node-right nodeB) (+ level 1))
         ]
        )
      )   
    (print "------------------/-------------------")
    trees-equal
    ))


;;; Procedure:
;;;   print-tree
;;; Parameters:
;;;   Tree, a tree
;;; Purpose:
;;;   Prints a red-black tree
;;; Produces:
;;;   [None]
(define print-tree
  (lambda (Tree)

    (define t1 (tree-root Tree))
    (define level 0)
    (define trees-equal #t)
     (print "------------------/-------------------")
    (let kernel ([node t1]
                 [level 0])
      
      (print-one "Level: " level)
      (print-one " |") 
      (print-one " Node: " (node-val node))
      (print-one " Color: " (node-color node))
      (print-one " Parent: " (node-val (node-parent node)))
      (display "\n")    
    
      (cond
        [(nil? node)
         (void)
         ]
        [else
         (kernel (node-left node) (+ level 1))
         (kernel (node-right node) (+ level 1))
         ]
        )
      )
     (print "------------------/-------------------")
    ))

;;; Procedure:
;;;   node-copy
;;; Parameters:
;;;   n, a node
;;; Purpose:
;;;   Creates and returns a copy of node n
;;; Produces:
;;;   result, a copy of node n
(define node-copy
  (lambda (n)
  (struct-copy node n)
  ))

   

; +---------------------+---------------------------------------------
; |    Testing          |
; +---------------------+

; Include here any code that will not be graded
(define tree-copy-old
  (lambda (n)
  (let kernel
   ( [orig n]
    [so-far nil])
    
  (set! so-far (node-copy orig))
 
  (set-node-left! so-far (node-copy(node-left orig)))
  (set-node-right! so-far(node-copy(node-right orig)))
  (set-node-parent! so-far (node-copy (node-parent orig)))
  (print-tree (tree so-far))
  (cond [ (and (not(nil? (node-right orig))) (not(nil? (node-left orig)))) (kernel (node-right orig) (node-right so-far)) (kernel (node-left orig) (node-left so-far))]
        [(not(nil? (node-right orig)))  (kernel (node-right orig) (node-right so-far))]
        [(not(nil? (node-left orig)))  (kernel (node-left orig) (node-left so-far))]
        [else so-far]))))
        

(define tree-copy
  (lambda (Tree) ; Tree is a tree

  (define final nil)
  (define Tree-image (struct-copy tree Tree))

  (let kernel
    ([old-curr (tree-root Tree-image)]
    [new-curr (tree final)]
    [new-parent nil]
    [level 0])

    (set! new-curr (node-copy old-curr))
    
    ;(print (node-val old-curr) " " (node-val new-curr))
    (set-node-parent! new-curr (node-copy (node-parent new-parent)))

    (if (equal? level 0) ; set external final to first new node
        (set! final new-curr)
        (void))

    (cond
      [(and (nil? (node-right old-curr)) (nil? (node-left old-curr))) 
        ; do nothing
      ]
      [(nil? (node-left old-curr))
        (set-node-left! new-curr nil)
        (set-node-right! new-curr (node-right old-curr))
        (kernel (node-right new-curr) (node-right old-curr) new-curr (+ level 1))
      ]
      [(nil? (node-right old-curr))
        (set-node-right! new-curr nil)
        (set-node-left! new-curr (node-left old-curr))
        (kernel (node-left new-curr) (node-left old-curr) new-curr (+ level 1))
      ]
      [else

        (set-node-left! new-curr (node-left old-curr))
        (set-node-right! new-curr (node-right old-curr))

        (kernel (node-left new-curr) (node-left old-curr) new-curr (+ level 1))
        (kernel (node-right new-curr) (node-right old-curr) new-curr (+ level 1))
      ]
    )
  )

  (define final-tree (tree final))
  final-tree
))

(define print-list
  (lambda (lst)
    (let kernel ([current lst])

      (if (null? (cdr current))
          (display (car current))
          (display (car current) (kernel (cdr current)))
          )

      )
    (display "\n")
    ))
