; Trace the visible cells in memory.
(define (tracer roots memory)
  ; Search the current root, possibly adding it to addresses
  (define (search root memory todo addresses)
    (if (memq root addresses)
      (process memory todo addresses)
      (let ((cell (vector-ref memory root)))
        (cond ((pair?   cell) (process memory (cons (car cell) (cons (cdr cell) todo)) (cons root addresses)))
              ((string? cell) (process memory todo (cons root addresses)))
              (else (error 'tracer "Invalid Entry"))))))

  ; Process the todo list, so it's searched
  (define (process memory todo addresses)
    (if (null? todo)
      addresses
      (search (car todo) memory (cdr todo) addresses)))

  ; Recurse through each root in roots
  (if (vector? memory)
    (if (null? roots)
      '()
      (search (car roots) memory '() (tracer (cdr roots) memory)))
    (error 'tracer "Invalid Memory")))