; Union two listsets.
;
; If both lists are null, listset-empty is returned. Otherwise a list is built
; from the elements in both listsets. This function assumes listset is actually
; a set and cannot contain duplicate entries.
(define listset-union
  (lambda (a b)
    (if (listset-null? a)
      (if (listset-null? b)
        listset-empty
        (listset-add (listset-first b) (listset-union a (listset-rest b)))
      )
      (listset-add (listset-first a) (listset-union (listset-rest a) b))
    )
  )
)