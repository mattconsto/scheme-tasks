; Convert from a simple S-expression to memory representation using an accumulator.
(define (convert-to-memory-representation S accum offset)
  (cond
    ((pair? S)   (let*
                   ((a (convert-to-memory-representation (car S) accum offset))
                    (d (convert-to-memory-representation (cdr S) (cadr a) (caddr a)))
                   (p (cons (car a) (car d))))
                 (list (caddr d) (cons p (cadr d)) (+ (caddr d) 1))))
    ((string? S) (list offset (cons S accum) (+ offset 1)))
    (else        (error 'convert-to-memory-representation "Invalid mini-Sexpression!"))))