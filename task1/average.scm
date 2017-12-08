; Calculate the increasing average of a list, false if sometime decreasing.
;
; Use a helper function that keeps track of the list and the average, total,
; and count of the last list. This means, it only divides once per element in
; the list. For each element, add it to the total, increment count, and 
; calculate the average; recurse. When the list is null, average is returned.
(define increasing-average
  (lambda (list)
    (define helper
      (lambda (list last-average last-total last-count)
        (if (pair? list)
          (let* (
			(total (+ last-total (car list)))
			(count (+ last-count 1))
			(average (/ total count))
	      )
            (if (< average last-average)
              #f
              (helper (cdr list) average total count)
            )
          )
          last-average
        )
      )
    )

    (if (pair? list)
      (helper list -inf.0 0 0) ; -Infinity means the first always passes.
      (error 'increasing-average "no average")
    )
  )
)