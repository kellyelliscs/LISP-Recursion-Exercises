;;; 1) Write a single Boolean LISP function, called TREE-CONTAINS, which takes two arguments N and TREE, and checks whether number N appears in the ordered tree TREE.  Returns t or NIL.

(defun TREE-CONTAINS (N TREE)
	(cond ((NULL TREE) NIL)													    ;if the list is empty, return NIL
		  ((numberp TREE) (equal N TREE))									;if there is one atom in the tree, check if it is equal to N
		  ((equal N (second TREE)) t)										  ;return t if N matches the root of the tree
		  ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
		                                                  ;if N < the root of the tree, look to the left of the root (first TREE)
		  (t (TREE-CONTAINS N (third TREE)))							;otherwise, check the rest of the TREE if the element doesn't match N
	)
)


;;; 2) Write a single LISP function, called TREE-MAX, which takes one argument TREE, and returns the maximum number appearing in the ordered tree TREE.  Returns a number. (Know that in an ordered tree, the last value in the list (or if the last value is a list, the last value within that list) will be the max value of the tree).

(defun TREE-MAX(TREE)
	(cond ((NULL TREE) NIL)												              ;if the list is empty, return NIL
		  ((NOT (NULL (rest TREE))) (TREE-MAX(rest TREE)))				;if the list has more than one element, go to the next (we want to get to the last element)
		  ((listp (first TREE)) (TREE-MAX (first TREE)))				  ;once we get to the last element, if it is a list, look through that list
		  ((NULL (rest TREE)) (first TREE))								        ;if we reach the last atom in the list, return it
		  (t (TREE-MAX(rest TREE)))
	)
)


;;; 3) Write a single LISP function, called TREE-ORDER, which takes one argument TREE, and returns an in-ordered list of the numbers appearing in the ordered tree TREE.

(defun TREE-ORDER(TREE)
	(cond ((NULL TREE) NIL)
		  ((numberp TREE) (list TREE))									;a single element in TREE should be returned as a list
		  (t (append (TREE-ORDER (first TREE)) (list (second TREE)) (TREE-ORDER (third TREE))))				
		  									                            ;(cons (1 2 3) (4 5 6)) will create ((1 2 3) 4 5 6), but append would create (1 2 3 4 5 6)
	)
)


;;; 4) Write a single LISP function, called a SUB-LIST, that takes a list L and two non-negative integers START and LEN, and returns the sub-list of L starting at position START and having length LEN.  Assume that the first element of L has position 0

(defun SUB-LIST(L START LEN)
	(cond ((= 0 LEN) NIL)														                          ;if we need not traverse the list anymore, return NIL
		  ((NULL L) NIL)														                            ;if START/LEN go beyond the length of the list, return up to the end of the list
		  ((NOT (= 0 START)) (SUB-LIST (rest L) (- START 1) LEN))				        ;get to the proper starting point of the sublist before constructing
		  ((= 0 START) (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))		;construct the list, use (= 0 START) as the condition ( instead of 't')to  
		  																				                                  ;prevent problems when recursion of prev line returns to the top layer
	)
)


;;; 5) Write a single LISP function, called SPLIT-LIST, that takes a list L, and returns a list of two lists L1 and L2, in that order, such that a) L is the result of appending L1 and L2; b) Length of L2 minus length of L1 is 0 or 1
;;;RECOGNIZE: (cons '(a b) '(c d)) --> ((a b) c d)
;;;			  (list '(a b) '(c d)) --> ((a b) (c d))

(defun SPLIT-LIST(L)
	(cond ((NULL L) NIL)							  ;if the list is empty, return NIL
		  ((= 1 (length L)) L)						;if there is only 1 element in L, return the list as is

		  ((evenp (length L)) (list (SUB-LIST L 0 (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (length L))))			
		  											          ;if L has an even number of elements, create SUB-LISTs of each half and construct together

		  (t (list (SUB-LIST L 0 (let* ((midpoint (- (length L) 1))) (/ midpoint 2))) (SUB-LIST L (let* ((midpoint (- (length L) 1))) (/ midpoint 2)) (length L))))
		  											          ;if L has an odd number of elements, create SUB-LISTS where the first half has 1 less element than the second half
	)
)


;;; 6) Write a single LISP function, called BTREE-HEIGHT, which takes a binary tree TREE, and returns the height of TREE.  Note that the height of a binary tree is defined as the length of the longest path from the root node to the farthest leaf node.

(defun BTREE-HEIGHT(TREE)
	(cond ((NULL TREE) 0)											;if the tree is empty, return NIL
		  ((numberp TREE) 0)										;if there is only one element in the tree, return 0
		  (t (let* ((leftTree (+ 1 (BTREE-HEIGHT (first TREE)))) (rightTree (+ 1 (BTREE-HEIGHT (second TREE))))) (cond ((> leftTree rightTree) leftTree) (t rightTree))))
		  															        ;return the path of greatest length
	)
)


;;; 7) Write a single LISP function, called LIST2BTREE, that takes a non-empty list of atoms LEAVES, and returns a binary tree such that the tree leaves are the elements of LEAVES, and for any internal (non-leaf) node in the tree, the number of leaves in its right branch minus the number of leaves in its left branch is 0 or 1.

(defun LIST2BTREE(LEAVES)
	(cond ((NULL LEAVES) NIL)
		  ((= 1 (length LEAVES)) (first LEAVES))						;if only 1 element in list, return as a number, not a single-element list
		  ((= 2 (length LEAVES)) LEAVES)								    ;do not split lists with 2 elements
		  (t (let* ((rightSide (cons (LIST2BTREE (second (SPLIT-LIST LEAVES))) NIL))) (cons (LIST2BTREE (first (SPLIT-LIST LEAVES))) rightSide)))
		  																                  ;manipulate lists with cons to ensure proper list embedding
	)
)


;;; 8) Write a single LISP function, called BTREE2LIST, that takes a binary tree TREE, and returns a list of atoms (assume TREE follows the constraints we defined earlier). As the input is a binary tree, each node has at most 2 children; This function is the inverse of LIST2BTREE. That is, (BTREE2LIST (LIST2BTREE X)) = X for all lists of atoms X.

(defun BTREE2LIST(TREE)
	(cond ((NULL TREE) NIL)														    ;if the TREE is empty, return NIL
		  ((numberp TREE) (list TREE))											;if there is 1 element in the TREE, return it as a list
		  (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
		                                                    ;append the parts of the TREE together (2 parts to a binary tree)
	)
)
