; Recursively convert from a memory representation to a simple S-expression.
(define (convert-from-memory position memory)
  (if (vector? memory)
    (let ((cell (vector-ref memory position)))
      (cond
        ((and (pair? cell) (number? (car cell)) (number? (cdr cell)))
          (cons
            (convert-from-memory (car cell) memory)
            (convert-from-memory (cdr cell) memory)))
        ((string? cell) cell)
        (else (error 'convert-from-memory "Invalid Entry!"))))
    (error 'convert-from-memory "Invalid Memory!")))