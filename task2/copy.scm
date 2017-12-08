; Copying collector garbage collector. Works with memories comprised of strings and pairs.
(define (copy-memory roots memory)
  (define (search root memory accum offset)
    (let ((cell (vector-ref memory root)))
      (cond ((pair? cell)
             (let* ((t (list (cons (list root (if (= offset 0) (cadr accum) offset)) (car accum))
                             (+ (cadr accum) 1)
                             (caddr accum)
                             (cadddr accum)
                             (cadddr (cdr accum))))
                    (a (if (assq (car cell) (car t)) t (search (car cell) memory t (cadddr (cdr t)))))
                    (d (if (assq (cdr cell) (car a)) a (search (cdr cell) memory a (cadddr (cdr a)))))
                    (p (cons (cadr (assq (car cell) (car d))) (cadr (assq (cdr cell) (car d))))))
               (add-cell p root (cadddr (cdr d)) d)))
            ((string? cell) (add-cell cell root offset accum))
            (else (error 'copy-memory "Invalid Entry")))))

  ; Recurse over each root, searching fully. Only add each root once.
  (define (combine roots memory)
    (if (null? roots)
      '(() 0 () () 0)
      (let ((last (combine (cdr roots) memory)))
        (if (assq (car roots) (car last))
          (add-root (car roots) last)
          (add-root (car roots) (search (car roots) memory last (cadddr (cdr last))))))))

  ; Add a root node to our output.
  (define (add-root root input)
    (list (car input)
          (cadr input)
          (cons (cadr (assq root (car input))) (caddr input))
          (cadddr input)
          (cadddr (cdr input))))

  ; Add a cell to the output, incrementing the offset.
  (define (add-cell cell root offset input)
    (let ((test (assq root (car input))))
    (list (if (and test (>= (cadr test) offset)) (car input) (cons (list root offset) (car input)))
          (+ (cadr input) 1)
          (caddr input)
          (cons cell (cadddr input))
          (+ (cadddr (cdr input)) 1))))

  ; Quick sanity check to ensure we are passed a vector, we don't want any other data type.
  (if (vector? memory)
    (cddr (combine roots memory))
    (error 'tracer "Invalid Memory")))