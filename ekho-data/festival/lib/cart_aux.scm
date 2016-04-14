;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 1996,1997                         ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;  Permission is hereby granted, free of charge, to use and distribute  ;;
;;;  this software and its documentation without restriction, including   ;;
;;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
;;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
;;;  permit persons to whom this work is furnished to do so, subject to   ;;
;;;  the following conditions:                                            ;;
;;;   1. The code must retain the above copyright notice, this list of    ;;
;;;      conditions and the following disclaimer.                         ;;
;;;   2. Any modifications must be clearly marked as such.                ;;
;;;   3. Original authors' names are not deleted.                         ;;
;;;   4. The authors' names are not used to endorse or promote products   ;;
;;;      derived from this software without specific prior written        ;;
;;;      permission.                                                      ;;
;;;                                                                       ;;
;;;  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ;;
;;;  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ;;
;;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ;;
;;;  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ;;
;;;  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ;;
;;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;;  THIS SOFTWARE.                                                       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Some functions for manipulating decision trees
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cart_prune_tree_thresh tree threshold default)
"(prune_cart_tree_thresh TREE THRESHOLD DEFAULT)
Prune the classification tree TREE so that all tail nodes with
a prediction probabality less than THRESHOLD and changed to return
DEFAULT instead.  This may be used when different mistakes have actually
different penalites hence some control of the defaults need to be
controlled."
  (cond
   ((cdr tree) ;; a question
    (list
     (car tree)
     (cart_prune_tree_thresh (car (cdr tree)) threshold default)
     (cart_prune_tree_thresh (car (cdr (cdr tree))) threshold default)))
   ((< (cart_class_probability (car tree)) threshold)
    (list (list (list threshold default) default)))
   (t   ;; leave asis
    tree)))

(define (cart_class_probability class)
  "(cart_class_probability CLASS)
Returns the probability of the best class in the cart leaf node CLASS.
If CLASS simple has a value and now probabilities the probabilities
it assume to be 1.0."
  (let ((val 0.0))
    (set! val (assoc (car (last class)) class))
    (if val
	(car (cdr val))
	1.0)))

(define (cart_class_prune_merge tree)
  "(cart_class_prune_merge tree)
Prune all sub trees which are pure.  That is they all predict the
same class.  This can happen when some other pruning technique
as modified a sub-tree now making it pure."
  (let ((pure (cart_tree_pure tree)))
    (cond
     (pure pure)
     ((cdr tree);; a question   
      (list
       (car tree)
       (cart_class_prune_merge (car (cdr tree)))
       (cart_class_prune_merge (car (cdr (cdr tree))))))
     (t;; a leaf leave asis
      tree))))

(define (cart_tree_pure tree)
  "(cart_tree_pure tree)
Returns a probability density function if all nodes in this tree
predict the same class and nil otherwise"
  (cond
   ((cdr tree) 
    (let ((left (cart_tree_pure (car (cdr tree))))
	  (right (cart_tree_pure (car (cdr (cdr tree))))))
      (cond
       ((not left) nil)
       ((not right) nil)
       ((equal? (car (last left)) (car (last right)))
	left)
       (t
	nil))))
   (t   ;; its a leaf, so of couse its pure
    tree)))

(define (cart_simplify_tree tree map)
  "(cart_simplify_tree TREE)
Simplify a CART tree by reducing probability density functions to
simple single clasifications (no probabilities).  This removes valuable
information from the tree but makes them smaller easier to read by humans
and faster to read by machines.  Also the classes may be mapped by the assoc
list in map.  The bright ones amongst you will note this could be
better and merge 'is' operators into 'in' operators in some situations
especially if you are ignoring actual probability distributions."
  (cond
   ((cdr tree)
    (list
     (car tree)
     (cart_simplify_tree (car (cdr tree)) map)
     (cart_simplify_tree (car (cdr (cdr tree))) map)))
   (t
    (let ((class (car (last (car tree)))))
      (if (assoc class map)
	  (list (cdr (assoc class map)))
	  (list (last (car tree))))))))

(define (cart_simplify_tree2 tree)
  "(cart_simplify_tree2 TREE)
Simplify a CART tree by reducing probability density functions to
only non-zero probabilities."
  (cond
   ((cdr tree)
    (list
     (car tree)
     (cart_simplify_tree2 (car (cdr tree)))
     (cart_simplify_tree2 (car (cdr (cdr tree))))))
   (t
    (list
     (cart_remove_zero_probs (car tree))))))

(define (cart_remove_zero_probs pdf)
  "(cart_remove_zero_probs pdf)
Removes zero probability classes in pdf, last in list
is best in class (as from cart leaf node)."
  (cond
   ((null (cdr pdf)) pdf)
   ((equal? 0 (car (cdr (car pdf))))
    (cart_remove_zero_probs (cdr pdf)))
   (t
    (cons 
     (car pdf)
     (cart_remove_zero_probs (cdr pdf))))))

(define (cart_interpret_debug i tree)
  "(cart_interpret_debug i tree)
In comparing output between different implementations (flite vs festival)
This prints out the details as it interprets the tree."
  (cond
   ((cdr tree) ;; question
    (format t "%s %s %s\n" (car (car tree)) (upcase (cadr (car tree)))
            (car (cddr (car tree))))
    (set! a (item.feat i (car (car tree))))
    (format t "%s\n" a)
    (cond
     ((string-equal "is" (cadr (car tree)))
      (if (string-equal a (car (cddr (car tree))))
          (begin
            (format t "   YES\n")
            (cart_interpret_debug i (car (cdr tree))))
          (begin
            (format t "   NO\n")
            (cart_interpret_debug i (car (cddr tree))))))
     ((string-equal "<" (cadr (car tree)))
      (if (< (parse-number a) (parse-number (car (cddr (car tree)))))
          (begin
            (format t "   YES\n")
            (cart_interpret_debug i (car (cdr tree))))
          (begin
            (format t "   NO\n")
            (cart_interpret_debug i (car (cddr tree))))))
     (t
      (format t "unknown q type %l\n" (car tree)))))
   (t ;; leaf
    (car tree)
    )))
    
(provide 'cart_aux)
	
