; Given a rectangular matrix, compute its diagonal.
;
; A helper function keeps track of the current column in the matrix. Recurse 
; over each row using list-tail to retrieve the desired pair in the row. Finally
; use cons to build the list of the matrices diagonal.
(define diagonal
  (lambda (matrix)
    (define helper
      (lambda (matrix pos)
        (if (pair? matrix)
          (let ((pair (nth-pair (car matrix) pos)))
            (if (pair? pair)
              (cons (car pair) (helper (cdr matrix) (+ pos 1)))
              '()
            )
          )
          '()
        )
      )
    )
	
	; I was going to use list-tail, but despite being listed in the r5rs spec,
	; using the function gave me "Unbound variable list-tail".
	(define nth-pair
      (lambda (list pos)
        (if (<= pos 0) list (nth-pair (cdr list) (- pos 1)))
      )
    )
	
    (helper matrix 0)
  )
)

