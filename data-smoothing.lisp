;;;; data-smoothing.lisp

(in-package #:data-smoothing)

;;; "data-smoothing" goes here. Hacks and glory await!

(defun simple-moving-average (list &optional (order 3))
"   list:   List of numerical Values
   order:  Number of Values from list used for calculating the smoothed list
           If order is set to 1 the simple-moving-average algorithm produces an output-list identical to the input-list
   
   function returns: Smoothed list
                     or NIL if order < (length list)
                            if order > 0
                            if list is not of type list
                            if at least one item of list is not numerical

   Example: (simple-moving-average '(2 2 8 2 2 2) 3) 
                                    => '(4 4 4 2)
            Note that you'll lose the Values for the first n-1 periods if order==n
            You'll also get a time leg of (/ (- n 1) 2) time units in your data row

   Literature: https://en.wikipedia.org/wiki/Moving_average
"
  (cond ((and (listp list)
	      (< order (length list))
	      (> order 0)
	      (every #'numberp list))
	 ;; calculate moving average
	 (loop for x from 0 upto (- (length list) order)
	    collect (/ (apply #'+ (subseq list x (+ x order))) order)))
	(t
	 nil)))

	      
	      

(defun centered-moving-average (list &optional (order 3))  
"   list:   List of numerical Values
   order:  odd number of Values from list used for calculating the smoothed list
           If order is set to 1 the centered-moving-average algorithm produces an output-list identical to the input-list
   
   function returns: Smoothed list
                     or NIL if order < (length list)
                            if order > 0
                            if order is odd
                            if list is not of type list
                            if at least one item of list is not numerical

   Example: (simple-moving-average '(2 2 8 2 2 2) 3) 
                                  => '(4 4 4 2)
            Note that you'll lose the Values for the first and last (n-1)/2 periods if order==n

   Note: The centered-moving-average (CMA) is the same algorithm like the simple-moving-average (SMA) algorithm.
         The difference is how you set the result-list relative to the input list:
      
         order==3
         Timeline:   0  1  2  3  4  5  6  7  8
                   ============================
         Data List: (2  2  8  2  2  2)
         SMA:             (4  4  4  2)
         CMA:          (4  4  4  2)
"

       (if (oddp order)
	   (simple-moving-average list order)
	   nil))