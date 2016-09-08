;; ***********************************************************************************************
;; Gareth Sharpe (20337731)
;; CS 135 Winter 2016
;; Assignment 08, Question 1
;; ***********************************************************************************************

  ;; A binary search tree (BST) is one of:
  ;; * empty
  ;; * a Node

  (define-struct node (key val left right))
  ;; A Node is a (make-node Num Str BST BST)
  ;; requires: key > every key in left BST
  ;;           key < every key in right BST

;; Constants
(define test-tree-one (make-node 8 "8" 
                                 (make-node 4 "4" 
                                            (make-node 2 "2" 
                                                       (make-node 1 "1" empty empty)
                                                       (make-node 3 "3" empty empty))
                                            (make-node 6 "6"
                                                       (make-node 5 "5" empty empty)
                                                       (make-node 7 "7" empty empty)))
                                 (make-node 12 "12"
                                            (make-node 10 "10"
                                                       (make-node 9 "9" empty empty)
                                                       (make-node 11 "11" empty empty))
                                            (make-node 14 "14"
                                                       (make-node 13 "13" empty empty) empty))))

(define test-tree-two (make-node 10 "10"
                                 (make-node 4 "4"
                                            (make-node 1 "1" empty empty)
                                            (make-node 3 "3"
                                                       (make-node 1 "1" empty empty)
                                                       (make-node 2 "2"
                                                                  (make-node 1 "1" empty empty)
                                                                  (make-node 1 "1" empty empty))))
                                 empty))

(define test-tree-three (make-node 10 "10"
                                   (make-node 5 "5" empty
                                              (make-node 6 "6"
                                                         (make-node 4 "4" empty empty)
                                                         (make-node 7 "7" empty empty)))
                                   (make-node 15 "15" (make-node 13 "13"
                                                                 (make-node 12 "12" empty empty)
                                                                 (make-node 14 "14" empty empty))
                                              (make-node 20 "20"
                                                         (make-node 19 "19" empty empty)
                                                         (make-node 21 "21" empty empty)))))

(define test-tree-four (make-node -10 "-10"
                                  (make-node -12 "-12"
                                             (make-node -14 "-14"
                                                        (make-node -16 "-16" empty
                                                                   (make-node -15 "-15" empty empty))
                                                        (make-node -13 "-13" empty empty))
                                             (make-node -11 "-11" empty empty))
                                  (make-node -8 "-8"
                                             (make-node -9 "-9" empty empty)
                                             (make-node -6 "-6"
                                                        (make-node -7 "-7" empty empty)
                                                        (make-node -4 "-4"
                                                                   (make-node -5 "-5" empty empty)
                                                                   empty)))))

;; (root-at-smallest bst) consumes a BST and rearranges it so that
;;    its smallest element is at the root.
;; root-at-smallest: BST -> BST

;; Examples:
(check-expect (root-at-smallest test-tree-one) (make-node 1 "1" empty
                                                          (make-node 8 "8"
                                                                     (make-node 4 "4"
                                                                                (make-node 2 "2" empty
                                                                                           (make-node 3 "3" empty empty))
                                                                                (make-node 6 "6"
                                                                                           (make-node 5 "5" empty empty)
                                                                                           (make-node 7 "7" empty empty)))
                                                                     (make-node 12 "12"
                                                                                (make-node 10 "10"
                                                                                           (make-node 9 "9" empty empty)
                                                                                           (make-node 11 "11" empty empty))
                                                                                (make-node 14 "14"
                                                                                           (make-node 13 "13" empty empty)
                                                                                           empty)))))
(check-expect (root-at-smallest test-tree-two) (make-node 1 "1" empty
                                                          (make-node 10 "10"
                                                                     (make-node 4 "4" empty
                                                                                (make-node 3 "3"
                                                                                           (make-node 1 "1" empty empty)
                                                                                           (make-node 2 "2"
                                                                                                      (make-node 1 "1" empty empty)
                                                                                                      (make-node 1 "1" empty empty))))
                                                                     empty)))

(define (root-at-smallest abst)
  (local [(define (smallest-root-fn abst)
            (cond
              [(empty? (node-left abst)) (node-key abst)]
              [else (smallest-root-fn (node-left abst))]))
          (define (bst-remove key abst)
            (cond
              [(= key (node-key abst))
               (local [(define (remove-root abst)
                         (cond
                           [(and (empty? (node-left abst))
                                 (empty? (node-right abst))) empty]
                           [(empty? (node-left abst)) (node-right abst)]))]
                 (remove-root abst))]
              [(< key (node-key abst))
               (make-node (node-key abst)
                          (node-val abst)
                          (bst-remove key (node-left abst))
                          (node-right abst))]))]
    (make-node (smallest-root-fn abst)
               (number->string (smallest-root-fn abst))
               empty
               (bst-remove (smallest-root-fn abst) abst))))




;; Tests:
(check-expect (root-at-smallest test-tree-three) (make-node 5 "5" empty
                                                            (make-node 10 "10"
                                                                       (make-node 6 "6"
                                                                                  (make-node 4 "4" empty empty)
                                                                                  (make-node 7 "7" empty empty))
                                                                       (make-node 15 "15"
                                                                                  (make-node 13 "13"
                                                                                             (make-node 12 "12" empty empty)
                                                                                             (make-node 14 "14" empty empty))
                                                                                  (make-node 20 "20"
                                                                                             (make-node 19 "19" empty empty)
                                                                                             (make-node 21 "21" empty empty))))))
(check-expect (root-at-smallest test-tree-four) (make-node -16 "-16" empty
                                                           (make-node -10 "-10"
                                                                      (make-node -12 "-12"
                                                                                 (make-node -14 "-14"
                                                                                            (make-node -15 "-15" empty empty)
                                                                                            (make-node -13 "-13" empty empty))
                                                                                 (make-node -11 "-11" empty empty))
                                                                      (make-node -8 "-8"
                                                                                 (make-node -9 "-9" empty empty)
                                                                                 (make-node -6 "-6"
                                                                                            (make-node -7 "-7" empty empty)
                                                                                            (make-node -4 "-4"
                                                                                                       (make-node -5 "-5" empty empty)
                                                                                                       empty))))))

;; (bst-remove key abst) consumes a key and a BST, and produces the
;;    BST with the node containing the given key removed.
;; bst-remove: Num BST -> BST

;; Examples:
(check-expect (bst-remove -10 test-tree-four) (make-node -8 "-8"
                                                         (make-node -12 "-12"
                                                                    (make-node -14 "-14"
                                                                               (make-node -16 "-16" empty
                                                                                          (make-node -15 "-15" empty empty))
                                                                               (make-node -13 "-13" empty empty))
                                                                    (make-node -11 "-11" empty empty))
                                                         (make-node -6 "-6"
                                                                    (make-node -9 "-9" empty empty)
                                                                    (make-node -4 "-4"
                                                                               (make-node -7 "-7" empty empty)
                                                                               (make-node -5 "-5" empty empty)))))

(check-expect (bst-remove -14 test-tree-four) (make-node -10 "-10"
                                                         (make-node -12 "-12"
                                                                    (make-node -13 "-13"
                                                                               (make-node -16 "-16" empty
                                                                                          (make-node -15 "-15" empty empty)) empty)
                                                                    (make-node -11 "-11" empty empty))
                                                         (make-node -8 "-8"
                                                                    (make-node -9 "-9" empty empty)
                                                                    (make-node -6 "-6"
                                                                               (make-node -7 "-7" empty empty)
                                                                               (make-node -4 "-4"
                                                                                          (make-node -5 "-5" empty empty)
                                                                                          empty)))))

(define (bst-remove key abst)
  (cond
    [(empty? abst) empty]
    [(empty? key) abst]
    [(= key (node-key abst))
     (local [(define (remove-root abst)
               (cond
                 [(and (empty? (node-left abst))
                       (empty? (node-right abst))) empty]
                 [(empty? (node-left abst)) (node-right abst)]
                 [(empty? (node-right abst)) (node-left abst)]
                 [else (make-node (node-key (node-right abst))
                                  (node-val (node-right abst))
                                  (node-left abst)
                                  (remove-root (node-right abst)))]))]
       (remove-root abst))]
    [(< key (node-key abst))
     (make-node (node-key abst)
                (node-val abst)
                (bst-remove key (node-left abst))
                (node-right abst))]
    [else (make-node (node-key abst)
                     (node-val abst)
                     (node-left abst)
                     (bst-remove key (node-right abst)))]))

;; Tests:
(check-expect (bst-remove 5 empty) empty)
(check-expect (bst-remove 10 test-tree-four) test-tree-four)
(check-expect (bst-remove empty empty) empty)
(check-expect (bst-remove empty test-tree-four) test-tree-four)
(check-expect (bst-remove 10 test-tree-three) (make-node 15 "15"
                                                         (make-node 5 "5" empty
                                                                    (make-node 6 "6"
                                                                               (make-node 4 "4" empty empty)
                                                                               (make-node 7 "7" empty empty)))
                                                         (make-node 20 "20"
                                                                    (make-node 13 "13"
                                                                               (make-node 12 "12" empty empty)
                                                                               (make-node 14 "14" empty empty))
                                                                    (make-node 21 "21" (make-node 19 "19" empty empty)
                                                                               empty))))
(check-expect (bst-remove 5 test-tree-three) (make-node 10 "10"
                                                        (make-node 6 "6"
                                                                   (make-node 4 "4" empty empty)
                                                                   (make-node 7 "7" empty empty))
                                                        (make-node 15 "15"
                                                                   (make-node 13 "13"
                                                                              (make-node 12 "12" empty empty)
                                                                              (make-node 14 "14" empty empty))
                                                                   (make-node 20 "20"
                                                                              (make-node 19 "19" empty empty)
                                                                              (make-node 21 "21" empty empty)))))
