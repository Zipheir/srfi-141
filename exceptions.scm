(define-syntax assert-type
  (syntax-rules ()
    ((assert-type loc expr)
     (unless expr
       (abort
        (make-composite-condition
         (make-property-condition 'exn
          'location loc
          'message "type check failed"
          'arguments (list 'expr))
         (make-property-condition 'type)
         (make-property-condition 'assertion)))))))

(define (arithmetic-exception loc msg . args)
  (abort
   (make-composite-condition
    (make-property-condition 'exn
     'location loc
     'message msg
     'arguments args)
    (make-property-condition 'arithmetic))))
