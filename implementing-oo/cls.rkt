#lang racket/base
(require racket/local 
         racket/list test-engine/racket-tests)
#|
- objects: accept messages,
hold data
- methods: functions, references this
- fields: data held by an object
- classes: create subclass, create instances
|#

;; A Name is a Symbol
;; A Value is any value
;; A Message is (make-msg Name [Listof Value])
(define-struct msg (name args))
;; A Field is (make-field Name Value)
;; (define-struct field (name val))
(define-struct obj (methods))
;; An Object is (make-obj [Listof Method])

;; A Class is (make-class [Listof Method]
;;                        [Listof Name])
(define-struct class (methods fields))

;; A Method is (make-meth Name 
;;                      (Object [Listof Value] -> Anything))
(define-struct meth (name func))

;; new : Class [Listof Value] -> Object  
#;
(define (new cls vals)
  (make-obj (build-list (length vals)
                        (λ (i) 
                          (make-field 
                           (list-ref (class-fields cls) i)
                           (list-ref vals i))))
            (class-methods cls)))

;; find-method : [Listof Method] Name -> Method
;; produce the method with the given name, or error
(define (find-method meths name)
  (cond [(empty? meths) (error "method not found")]
        [(equal? name (meth-name (first meths)))
         (first meths)]
        [else (find-method (rest meths) name)]))

;; send : Object Message -> Any
(define (send obj msg)
  (local [(define meths (obj-methods obj))]
    ((meth-func (find-method meths (msg-name msg)))
     (msg-args msg))))

#;
(define c% (make-class (list
                        (make-meth 'm 
                                   (λ (args)
                                     (first args))))
                       empty))

;; c% : [Listof Value] -> Object
(define (new-c% field-vals)
  (make-obj 
   (list
    (make-meth 'm 
               (λ (args)
                 (first args))))))
#|
(define (find-field n flds)
  (cond [(empty? flds) (error "field not found")]
        [(equal? n (field-name (first flds)))
         (first flds)]
        [else (find-field n (rest flds))]))

;; get-field : Object Name -> Value
(define (get-field o n)
  (field-val (find-field n (obj-fields o))))
|#

;; a% : (Listof Value) -> Object
(define (a% fields)
  (make-obj (list (make-meth 'get-y
                                (λ (a) 10)))))

;; d% : ([Listof Value] -> Object) [Listof Value] -> Object
(define (d% super fields)
  (local 
    [(define a-methods (obj-methods 
                        super
                        (super fields)
                        #;(a% fields)))
     (define bob (+ 1 2 3 4 5 2))
     (define this 
       (make-obj ;(list) ;(make-field 'x (first args)))
        (append 
         (list (make-meth 'get-x 
                          (λ (m-args)
                            (first fields)))
               (make-meth 'bob
                          (λ (m-args)
                            (+ (send this 
                                     (make-msg 'get-x empty))
                               bob))))
         a-methods)))]
    this))

(check-expect (send (d% (λ (f) (make-obj empty)) (list 17))
                    (make-msg 'get-x empty))
              17)

(check-expect 
 (send (new-c% empty)
       (make-msg 'm (list 1)))
 1)

(check-expect 
 (send (new-c% empty)
       (make-msg 'm (list 117)))
 117)

(define o (make-obj ;empty
                    (list 
                     (make-meth 'add 
                                (λ (args)
                                  (add1 (first args)))))))

(check-expect 
 (send o (make-msg 'add (list 7)))
 8)

(test)
;(send o m 1 2 3)
