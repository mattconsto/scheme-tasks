; Map a graph
;
; Takes a graph and a map, reassigning the nodes in the graph based on the map. 
; Depending on the map, nodes can be merged, renamed, or dropped from the graph.
; This function splits the task into mapping each edge, then each node. The
; result is re-assembled and returned. If either the map or graph is empty, '()
; is returned.
(define graph-mapper
  (lambda (graph map)
    (define map-edge
      (lambda (edge map)
        (let ((a (map-node (car edge) map)) (b (map-node (cadr edge) map)))
		  ; If either node is dropped, the entire edge is.
          (if (or (null? a) (null? b))
            '()
            (list a b)
          )
        )
      )
    )
    
    (define map-node
      (lambda (node map)
        (cond
          ((or (null? node) (null? map))
            '())
          ((eq? node (caar map))
            (cdar map))
          (else
            (map-node node (cdr map)))
        )
      )
    )

    (if (and (pair? graph) (pair? map))
      (let ((edge (map-edge (car graph) map)))
	    ; Prevents empty edges being added to the new graph
        (if (null? edge)
          (graph-mapper (cdr graph) map)
          (cons edge (graph-mapper (cdr graph) map))
        )
      )
      '()
    )
  )
)