#lang racket

(require racket/include rackunit rackunit/gui)

(include "tree.rkt")

(test/gui
  (test-suite
    "Tests for tree.rkt"
    (test-suite
      "tree?"
      (test-case "Empty String"                                  (check-equal? (tree? "") #f))
      (test-case "Only one empty tree"                           (check-equal? (tree? "*") #t))
      (test-case "Only one empty tree with prefixed whitespace"  (check-equal? (tree? "     *") #t))
      (test-case "Only one empty tree with postfixed whitespace" (check-equal? (tree? "*     ") #t))
      (test-case "Only one empty tree with whitespaces from both sides" (check-equal? (tree? "   *     ") #t))

      (test-case "Simple Tree"                (check-equal? (tree? " { 5 * * }") #t))
      (test-case "Simple Tree with no spaces" (check-equal? (tree? "{5**}") #t))
      (test-case "Wrong structure 1" (check-equal? (tree? "{** 5}") #f))
      (test-case "Wrong structure 2" (check-equal? (tree? "{**}") #f))
      (test-case "Wrong structure 3" (check-equal? (tree? "{*124356 *}") #f))

      (test-case "Complex Tree Level 1" (check-equal? (tree? "{1 {2 * {3 * *}} *}") #t))
      (test-case "Complex Tree Missing Child" (check-equal? (tree? "{1 {2 * {3 * *}} }") #f))
    )

    (test-suite
      "balanced?"
      (test-case "Only one empty tree"                    (check-equal? (balanced? '()) #t))
      (test-case "Simple Tree"                            (check-equal? (balanced? '(2 () ())) #t))
      (test-case "Simple Tree with 1 level difference"    (check-equal? (balanced? '(2 (2 () ()) ())) #t))
      (test-case "Simple Tree with 2 levels difference"   (check-equal? (balanced? '(2 (2 (3 () ()) ()) ())) #f))
      (test-case "Simple Tree with 1 level difference 2"  (check-equal? (balanced? '(2 () (3 () ()))) #t))
      (test-case "Simple Tree with 2 levels difference 2" (check-equal? (balanced? '(2 () (2 (3 () ()) ()))) #f))

      (test-case "Complex Tree"                           (check-equal? (balanced? '(1 (2 (3 () (4 () ())) ()) (5 (6 (7 () ()) (8 () ())) ()))) #f))

      (test-case "Non-binary tree"                        (check-equal? (balanced? '(2 () () () ())) #f))
    )

    (test-suite
      "ordered?"
      (test-case "Only one empty tree" (check-equal?  (ordered? '()) #t))
      (test-case "Simple tree"         (check-equal?  (ordered? '(2 () ())) #t))
      (test-case "Simple tree with only left child true"  (check-equal? (ordered? '(2 (1 () ()) ())) #t))
      (test-case "Simple tree with only left child false" (check-equal? (ordered? '(2 (3 () ()) ())) #f))
      (test-case "Simple tree with only right child true" (check-equal? (ordered? '(2 () (3 () ()))) #t))
      (test-case "Simple tree with only right child false"(check-equal? (ordered? '(2 () (1 () ()))) #f))

      (test-case "Complex tree false" (check-equal? (ordered? '(1 (2 (3 () (4 () ())) ()) (5 (6 (7 () ()) (8 () ())) ()))) #f))
      (test-case "Complex tree true"  (check-equal? (ordered? '(5 (3 (2 () (4 () ())) ()) (6 (5 (4 () ()) (8 () ())) ()))) #t))

      (test-case "Non-binary tree"    (check-equal? (ordered? '(2 () () () ())) #f))
    )

    (test-suite
      "tree->string"
      (test-case "Only one empty tree" (check-equal?  (tree->string '()) "*"))
      (test-case "Simple tree"         (check-equal?  (tree->string '(2 () ())) "{2 * *}"))
      (test-case "Simple tree with only left child true"  (check-equal? (tree->string '(2 (1 () ()) ())) "{2 {1 * *} *}"))
      (test-case "Simple tree with only left child false" (check-equal? (tree->string '(2 (3 () ()) ())) "{2 {3 * *} *}"))
      (test-case "Simple tree with only right child true" (check-equal? (tree->string '(2 () (3 () ()))) "{2 * {3 * *}}"))
      (test-case "Simple tree with only right child false"(check-equal? (tree->string '(2 () (1 () ()))) "{2 * {1 * *}}"))

      (test-case "Complex tree false" (check-equal? (tree->string '(1 (2 (3 () (4 () ())) ()) (5 (6 (7 () ()) (8 () ())) ()))) "{1 {2 {3 * {4 * *}} *} {5 {6 {7 * *} {8 * *}} *}}"))
      (test-case "Complex tree true"  (check-equal? (tree->string '(5 (3 (2 () (4 () ())) ()) (6 (5 (4 () ()) (8 () ())) ()))) "{5 {3 {2 * {4 * *}} *} {6 {5 {4 * *} {8 * *}} *}}"))
    )
  )
)