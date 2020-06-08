(define-syntax λ
  (syntax-rules ()
    [(_ arg* bd bd* ...)
      (lambda arg* bd bd* ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define e/o-procs
  (vector
    (λ (it n)
      (if (zero? n) #t
          ((vector-ref it 1) it (- n 1))))
    (λ (it n)
      (if (zero? n) #f
          ((vector-ref it 0) it (- n 1))))))
#|
  > e/o-procs
  #(#<procedure at test.ss:128> #<procedure at test.ss:208>)
  > ((vector-ref e/o-procs 0) e/o-procs 5)
  #f
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; enumerate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define enumerate-env
  (λ (vars)
    (let loop ([vars vars] [i 0])
      (cond
        [(null? vars) '()]
        [else
          (cons `(,(car vars) ,i)
            (loop (cdr vars) (+ i 1)))]))))
#|
  > (define p '(a b c))
  > (define q '(a d c))
  > (define penv (enumerate-env p))
  > (define qenv (enumerate-env q))
  > (define renv '([a 3] [d 5] [c 6]))
  > penv
  ([a 0] [b 1] [c 2])
  > qenv
  ([a 0] [d 1] [c 2])
|#

(define append-env
  (λ (e p)
    (cond
      [(null? e) p]
      [else
        [append e
          (let ([m (+ (cadar (last-pair e)) 1)])
            (map (λ (pr)
                   `(,(car pr) ,(+ (cadr pr) m)))
              p))]])))
#|
  > (append-env penv qenv)
  ([a 0] [b 1] [c 2] [a 3] [d 4] [c 5])
  > (append-env renv penv)
  ([a 3] [d 5] [c 6] [a 7] [b 8] [c 9])
|#

(define trim-env
  (λ (e)
    (cond
      [(null? e) '()]
      [(assv (caar e) (cdr e)) (trim-env (cdr e))]
      [else (cons (car e) (trim-env (cdr e)))])))
#|
  > (trim-env (append-env penv qenv))
  ([b 1] [a 3] [d 4] [c 5])
|#

(define append-env append)
#|
  > (enumerate-env (append-env p q))
  ([a 0] [b 1] [c 2] [a 3] [d 4] [c 5])
|#

;; 类: 有三个元素的列表: a field environment, a method environment, and a method vector.
;; 对象: 类似于类但是它们的第一个元素的值各不相同。
;; field vector: 长度为对应类 field environment 的大小。
;; method 或者 field 可以是任意值，但如果它是至少存在一个参数的过程，那么它就能以面向对象的方式被执行。
;; cdr 一个类或者对象后，得到的是它的 method 表。
;; field 表是类的 field environment 及其相关对象的 field 向量。

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interface operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (p is a position, e is a environment, c is a class, oc is an object or a class.)

;;;;;;;;;;;;;;;;;;;;;;;; internal interface operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Extends the field environment of a class.
  (define (_fx c e) (append-env (car c) e))
  ;; Extends the method environment of a class.
  (define (_mx c e) (append-env (cadr c) e))
  ;; To access (or update) a method by position, use _mp (or _mp!).
  (define (_mp oc p) (vector-ref (caddr oc) p))
  (define (_mp! oc p v) (vector-set! (caddr oc) p v))
  ;; To access (or update) a field by position, use _fp (or _fp!). 
  (define (_fp o p) (vector-ref (car o) p))
  (define (_fp! o p v) (vector-set! (car o) p v))

;;;;;;;;;;;;;;;;;;;;;;;; external interface operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; If oc1 and oc2 share the same method table, it returns true.
  (define (_mteq? oc1 oc2) (eq? (cdr oc1) (cdr oc2)))
  ;; Creates a new object.
  (define (_n c)
    (cons (make-vector (length (car c))) (cdr c)))
  ;; Given an object and a variable (symbol), accesses a method.
  (define (_mv oc m)
    (_mp oc
      (let loop ([m* (cadr oc)] [pos 0])
        (if (eqv? (car m*) m)
            pos
            (loop (cdr m*) (+ pos 1))))))
#||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; the example in our style ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <o>
  (list
    '()
    '(isa? init)
    (vector
      (λ (it c)
        (_mteq? it c))
      (λ (it . args)
        (void)))))
#|
  > <o>
  (()
  (isa? init)
  #(#<isa?> #<init>))
|#

(define <p>
  (list
    (_fx <o> '(x y))
    (_mx <o> '(move get-loc diag))
    (vector
      (λ (it c)
        (or (_mteq? it c)
            ((_mp <o> 0) <o> c)))
      (λ (it x^ y^)
        (_fp! it 0 x^)
        (_fp! it 1 y^)
        ((_mp <o> 1) it))
      (λ (it dx dy)
        (_fp! it 0 (+ (_fp it 0) dx))
        (_fp! it 1 (+ (_fp it 1) dy)))
      (λ (it)
        (list (_fp it 0) (_fp it 1)))
      (λ (it a)
        ((_mp it 2) it a a)))))
#|
  > <p>
  ((x y)
   (isa? init move get-loc diag)
   #(#<isa?> #<init> #<move> #<get-loc> #<diag>))
  > (define p (_n <p>))
  > ((_mp p 1) p 12 13)
  > p
  (#(12 13)
  (isa? init move get-loc diag)
  #(#<isa?> #<init> #<move> #<get-loc> #<diag>))
  > ((_mv p 'move) p 14 15)
  > (define map-nullary-method
      (lambda (o m*)
        (map (lambda (m) ((_mv o m) o)) m*)))
  > (map-nullary-method p '(get-loc))
  ((26 28))
  > ((_mp p 0) p <p>)
  #t
|#

(define <cp>
  (list
    (_fx <p> '(hue))
    (_mx <p> '(get-hue diag&set))
    (vector
      (λ (it c)
        (or (_mteq? it c)
            ((_mp <p> 0) <p> c)))
      (λ (it x^ y^ hue^)
        (_fp! it 2 hue^)
        ((_mp <p> 1) it x^ y^))
      (_mp <p> 2)
      (_mp <p> 3)
      (_mp <p> 4)
      (λ (it) (_fp it 2))
      (λ (it a)
        ((_mp it 4) it a)
        (_fp! it 2 a)))))
#|
  > <cp>
  ((x y hue)
  (isa? init move get-loc diag get-hue diag&set)
  #(#<isa?> #<init> ...))
  > (define cp (_n <cp>))
  > ((_mp cp 1) cp 16 17 7)
  > cp
  (#(16 17 7)
  (isa? init move get-loc diag get-hue diag&set)
  #(#<isa?> #<init> ...))
  > ((_mv cp 'diag&set) cp 8)
  > (map-nullary-method cp '(get-loc get-hue))
  ((24 25) 8)
  > ((_mp cp 0) cp <p>)
  #t
  > ((_mp p 0) p <cp>)
  #f
|#

(define <scp>
  (list
    (_fx <cp> '(y))
    (_mx <cp> '(show-y))
    (vector
      (λ (it c)
        (or (_mteq? it c)
            ((_mp <cp> 0) <cp> c)))
      (λ (it x^ y^ hue^)
        (_fp! it 3 ": Stuck: ")
        ((_mp <cp> 1) it x^ y^ hue^))
      (λ (it x^ y^) ; overridden move
        ((_mp it 7) it))
      (_mp <cp> 3)
      (λ (it a) ; overridden diag
        (write (_fp it 2))
        ((_mp <cp> 4) it a))
      (_mp <cp> 5)
      (_mp <cp> 6)
      (λ (it)
        (display (_fp it 3))))))
#|
  > <scp>
  ((x y hue y)
  (isa? init ... get-hue diag&set show-y)
  #(#<isa?> #<init> ...))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; imposing protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; introducing super ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <scp>
  (let ([super <cp>])
    (list
      (_fx super '(y))
      (_mx super '(show-y))
      (vector
        (λ (it c)
          (or (_mteq? it c)
              ((_mp super 0) super c)))
        (λ (it x^ y^ hue^)
          (_fp! it 3 ": Stuck: ")
          ((_mp super 1) it x^ y^ hue^))
        (λ (it x^ y^) ; overridden move
          ((_mp it 7) it))
        (_mp super 3)
        (λ (it a) ; overridden diag
          (write (_fp it 2))
          ((_mp super 4) it a))
        (_mp super 5)
        (_mp super 6)
        (λ (it)
          (display (_fp it 3)))))))

;;;;;;;;;;;;;;;;;;;;;;; position variables for methods ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <scp>
  (let ([isa? 0]
        [init 1]
        [move 2]
        [get-loc 3]
        [diag 4]
        [get-hue 5]
        [diag&set 6]
        [show-y 7])
    (let ([super <cp>])
      (list
        (_fx super '(y))
        (_mx super '(show-y))
        (vector
          (λ (it c)
            (or (_mteq? it c)
                ((_mp super isa?) super c)))
          (λ (it x^ y^ hue^)
            (_fp! it 3 ": Stuck: ")
            ((_mp super init) it x^ y^ hue^))
          (λ (it x^ y^) ; overridden move
            ((_mp it show-y) it))
          (_mp super get-loc)
          (λ (it a) ; overridden diag
            (write (_fp it 2))
            ((_mp super diag) it a))
          (_mp super get-hue)
          (_mp super diag&set)
          (λ (it)
            (display (_fp it 3))))))))

;;;;;;;;;;;;;;;;;;;;;;;; position variables for fields ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <scp>
  (let* ([x 0] [y 1] [hue 2] [y 3])
    (let ([isa? 0]
          [init 1]
          [move 2]
          [get-loc 3]
          [diag 4]
          [get-hue 5]
          [diag&set 6]
          [show-y 7])
      (let ([super <cp>])
        (list
          (_fx super '(y))
          (_mx super '(show-y))
          (vector
            (λ (it c)
              (or (_mteq? it c)
                  ((_mp super isa?) super c)))
            (λ (it x^ y^ hue^)
              (_fp! it y ": Stuck: ")
              ((_mp super init) it x^ y^ hue^))
            (λ (it x^ y^) ; overridden move
              ((_mp it show-y) it))
            (_mp super get-loc)
            (λ (it a) ; overridden diag
              (write (_fp it hue))
              ((_mp super diag) it a))
            (_mp super get-hue)
            (_mp super diag&set)
            (λ (it)
              (display (_fp it y)))))))))
#||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lifting methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; naive lifting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <scp>
  (let* ([x 0] [y 1] [hue 2] [y 3])
    (let ([isa? 0]
          [init 1]
          [move 2]
          [get-loc 3]
          [diag 4]
          [get-hue 5]
          [diag&set 6]
          [show-y 7])
      (let ([super <cp>])
        (let ([isa? (λ (it c)
                      (or (_mteq? it c)
                          ((_mp super isa?) super c)))]
              [init (λ (it x^ y^ hue^)
                      (_fp! it y ": Stuck: ")
                      ((_mp super init) it x^ y^ hue^))]
              [move (λ (it x^ y^) ; overridden move
                      ((_mp it show-y) it))]
              [get-loc (_mp super get-loc)]
              [diag (λ (it a) ; overridden diag
                      (write (_fp it hue))
                      ((_mp super diag) it a))]
              [get-hue (_mp super get-hue)]
              [diag&set (_mp super diag&set)]
              [show-y (λ (it)
                        (display (_fp it y)))])
          (list
            (_fx super '(y))
            (_mx super '(show-y))
            (vector isa? init move get-loc diag get-hue diag&set show-y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; triply-nested let ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <scp>
  (let* ([x 0] [y 1] [hue 2] [y 3])
    (let ([isa? 0]
          [init 1]
          [move 2]
          [get-loc 3]
          [diag 4]
          [get-hue 5]
          [diag&set 6]
          [show-y 7])
      (let ([super <cp>])
        (let ([g0 (λ (it c)
                    (or (_mteq? it c)
                        ((_mp super isa?) super c)))]
              [g1 (λ (it x^ y^ hue^)
                    (_fp! it y ": Stuck: ")
                    ((_mp super init) it x^ y^ hue^))]
              [g2 (λ (it x^ y^) ; overridden move
                    ((_mp it show-y) it))]
              [g3 (λ (it a) ; overridden diag
                    (write (_fp it hue))
                    ((_mp super diag) it a))]
              [g4 (λ (it)
                    (display (_fp it y)))])
          (let ([isa? (_mp super isa?)]
                [init (_mp super init)]
                [move (_mp super move)]
                [get-loc (_mp super get-loc)]
                [diag (_mp super diag)]
                [get-hue (_mp super get-hue)]
                [diag&set (_mp super diag&set)])
            (let ([isa? g0]
                  [init g1]
                  [move g2]
                  [diag g3]
                  [show-y g4])
              (list
                (_fx super '(y))
                (_mx super '(show-y))
                (vector isa? init move get-loc diag get-hue diag&set show-y)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; quadruply-nested let ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <scp>
  (let* ([x 0] [y 1] [hue 2] [y 3])
    (let ([isa? 0]
          [init 1]
          [move 2]
          [get-loc 3]
          [diag 4]
          [get-hue 5]
          [diag&set 6]
          [show-y 7])
      (let ([super <cp>])
        (let ([h0 (_mp super isa?)]
              [h1 (_mp super init)]
              [h2 (_mp super move)]
              [h3 (_mp super get-loc)]
              [h4 (_mp super diag)]
              [h5 (_mp super get-hue)]
              [h6 (_mp super diag&set)])
          (let ([g0 (λ (it c)
                      (or (_mteq? it c)
                          ((_mp super isa?) super c)))]
                [g1 (λ (it x^ y^ hue^)
                      (_fp! it y ": Stuck: ")
                      ((_mp super init) it x^ y^ hue^))]
                [g2 (λ (it x^ y^) ; overridden move
                      ((_mp it show-y) it))]
                [g3 (λ (it a) ; overridden diag
                      (write (_fp it hue))
                      ((_mp super diag) it a))]
                [g4 (λ (it)
                      (display (_fp it y)))])
            (let ([isa? h0]
                  [init h1]
                  [move h2]
                  [get-loc h3]
                  [diag h4]
                  [get-hue h5]
                  [diag&set h6])
              (let ([isa? g0]
                    [init g1]
                    [move g2]
                    [diag g3]
                    [show-y g4])
                (list
                  (_fx super '(y))
                  (_mx super '(show-y))
                  (vector isa? init move get-loc diag get-hue diag&set show-y))))))))))
#||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; simplifying the syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax assv-macro
  (λ (x)
    (syntax-case x ()
      [(_ i ([k0 h0] [k1 h1] ...))
       (if (eqv? (syntax->datum #'k0) (syntax->datum #'i)) #'h0
           #'(assv-macro i ([k1 h1] ...)))])))

(define-syntax if-shadowed
  (λ (x)
    (syntax-case x ()
      [(_ id ctx conseq altern)
       ;; 给 id 加上 ctx 的上下文信息，看看 id 是否在上下文中被绑定了
       ;; 如果绑定了就是 conseq 没绑定就是 altern
       (if (not (free-identifier=? #'id
                  (datum->syntax #'ctx
                    (syntax->datum #'id))))
           #'conseq
           #'altern)])))

(define-syntax field-var
  (λ (x)
    (syntax-case x ()
      [(_ ctx id it j)
       #'(identifier-syntax
          [var (if-shadowed id ctx id (_fp it j))]
          [(set! var val) (if-shadowed id ctx (set! id val) (_fp! it j val))])])))

(define-syntax method-var
  (λ (x)
    (syntax-case x ()
      [(_ ctx mapping m super it i)
       #'(λ (x)
          (syntax-case x (super)
            [(m_ super arg (... ...))
             #'(if-shadowed m ctx (m super arg (... ...)) ((assv-macro i mapping) it arg (... ...)))]
            [(m_ o arg (... ...))
             #'(if-shadowed m ctx (m o arg (... ...)) (let ([o^ o]) ((_mp o^ i) o^ arg (... ...))))]
            [(m_)
              #'(if-shadowed m ctx (m) (errorf 'method "Cannot take zero arguments:~s" m))]
            [m_ (identifier? #'m_)
             #'(if-shadowed m ctx m (errorf 'method "Connot be a symbol:~s" m))]))])))

(define-syntax extender
  (λ (syn)
    (syntax-case syn ()
      [(k ctx ([s n] ...) (all-f ...) ([f j] ...) ([m i] ...) ([m-var g e] ...))
       (with-syntax ([(h ...) (generate-temporaries #'(s ...))])
         (with-implicit (ctx super method)
           #'(λ (super)
              (let ([h (_mp super n)] ...)
                (let-syntax
                  ([transf-body
                     (λ (xx)
                       (syntax-case xx ()
                         [(_ k3 ctx body0 body1 (... ...))
                          (with-implicit (k3 it super set! f ... m ...)
                            #'(let-syntax ([f (field-var ctx f it j)] ...)
                               (let-syntax ([m (method-var ctx ([n h] ...) m super it i)] ...)
                                 body0 body1 (... ...))))]))])
                  (let-syntax
                    ([method
                       (λ (xx)
                         (syntax-case xx ()
                           [(k2 params body0 body1 (... ...))
                            (with-implicit (k2 it)
                              #'(λ (it . params)
                                 (transf-body k2 ctx body0 body1 (... ...))))]))])
                    (let ([g e] ...)
                      (let ([s h] ...)
                        (let ([m-var g] ...)
                          (list '(all-f ...) '(m ...) (vector m ...)))))))))))])))

(define fresh-m-vars
  (λ (m-vars sup-m-vars)
    (cond
      [(null? m-vars) '()]
      [(memv (car m-vars) sup-m-vars) (fresh-m-vars (cdr m-vars) sup-m-vars)]
      [else (cons (car m-vars) (fresh-m-vars (cdr m-vars) sup-m-vars))])))

(define-syntax build-shadow
  (λ (x)
    (syntax-case x ()
      [(_ ctx sup-f sup-m (f-var ...) ([m-var g e] ...))
       (let ([sup-f (syntax->datum #'sup-f)]
             [sup-m (syntax->datum #'sup-m)]
             [f-vars (syntax->datum #'(f-var ...))]
             [m-vars (syntax->datum #'(m-var ...))])
         (let ([f (append-env sup-f f-vars)]
               [m (append-env sup-m (fresh-m-vars m-vars sup-m))]
               )
           (with-syntax
             ([([s k] ...) (datum->syntax #'ctx (enumerate-env sup-m))]
              [([m i] ...) (datum->syntax #'ctx (enumerate-env m))]
              [([f j] ...) (datum->syntax #'ctx (trim-env (enumerate-env f)))]
              [(all-f ...) (datum->syntax #'ctx f)])
             #'(λ (xx)
                (syntax-case xx ()
                  [(__)
                   #'(extender ctx ([s k] ...) (all-f ...) ([f j] ...) ([m i] ...) ([m-var g e] ...))]
                  [(__ an-m-var oc)
                   #'(_mp oc (assv-macro an-m-var ([m i] ...)))]
                  ;; (... ...) 生成一个 ...
                  [(__ ctx (f-var^ (... ...)) ([m-var^ e^] (... ...)))
                   (with-syntax ([(g^ (... ...)) (generate-temporaries #'(m-var^ (... ...)))])
                     #'(build-shadow ctx (all-f ...) (m ...)
                        (f-var^ (... ...))
                        ([m-var^ g^ e^] (... ...))))])))))])))

(define-syntax extend-shadow
  (λ (x)
    (syntax-case x ()
      [(k sup-shadow (f-var ...) ([m-var e] ...))
       (with-implicit (k super isa?)
         #'(sup-shadow k (f-var ...)
            ([isa? (λ (it c)
                     (or (_mteq? it c)
                         ((_mp super 0) super c)))]
             [m-var e] ...)))])))

(define-syntax create-class
  (syntax-rules ()
    [(_ host-shadow super-class)
     ((host-shadow) super-class)]))

(define-syntax build-<<o>>
  (λ (x)
    (syntax-case x ()
      [(k ([m e] ...))
       (with-syntax ([(g ...) (generate-temporaries #'(m ...))])
         #'(build-shadow k () () ()
            ([m g e] ...)))])))

(define-syntax <<o>>
  (build-<<o>>
    ([isa?
       (λ (it c)
         (_mteq? it c))]
     [init
       (λ (it . args)
         (void))])))

(define <o> (create-class <<o>> #f))

(define-syntax new
  (syntax-rules ()
    [(_ c arg ...)
     (let ([o (_n c)])
       ((_mp o 1) o arg ...)
       o)]))

(define-syntax mbv
  (syntax-rules ()
    [(_ m oc arg ...)
     (let ([oc^ oc])
       ((_mv oc^ 'm) oc^ arg ...))]))

(define-syntax invoke
  (syntax-rules ()
    [(_ shadow m oc arg ...)
     (let ([oc^ oc])
       ((shadow m oc^) oc^ arg ...))]))

(define isa?
  (λ (it c)
    ((_mp it 0) it c)))
#||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax <<p>>
  (extend-shadow <<o>> (x y)
    ([init
       (method (x^ y^)
         (set! x x^)
         (set! y y^)
         (init super))]
     [move
       (method (dx dy)
         (set! x (+ x dx))
         (set! y (+ y dy)))]
     [get-loc
       (method ()
         (list x y))]
     [diag
       (method (a)
         (move it a a))])))

(define <p> (create-class <<p>> <o>))

(define-syntax <<cp>>
  (extend-shadow <<p>> (hue)
    ([init
       (method (x^ y^ hue^)
         (set! hue hue^)
         (init super x^ y^))]
     [get-hue
       (method () hue)]
     [diag&set
       (method (a)
         (diag it a)
         (set! hue a))])))

(define <cp> (create-class <<cp>> <p>))

(define-syntax <<scp>>
  (extend-shadow <<cp>> (y)
    ([init
       (method (x^ y^ hue^)
         (set! y ": Stuck: ")
         (init super x^ y^ hue^))]
     [move
       (method (x^ y^)
         (show-y it))]
     [diag
       (method (a)
         (write hue)
         (diag super a))]
     [show-y
       (method () (display y))])))

(define <scp> (create-class <<scp>> <cp>))

(define test
  (λ (c)
    (let ([p (new <p> 1 2)]
          [cp (new c 18 19 9)])
      (mbv diag&set cp 10)
      (list
        (list
          (invoke <<cp>> get-loc cp)
          (invoke <<cp>> get-hue cp))
        (isa? cp <p>)
        (isa? p c)))))
#|
  > (begin (write (test <scp>)) (write (test <cp>)))
  9: Stuck: (((18 19) 10) #t #f)(((28 29) 10) #t #f)
|#

(define-syntax <<escp>>
  (extend-shadow <<scp>> ()
    ([init
       (method (x^ y^ hue)
         (display hue)
         (init super x^ y^ hue))]
     [show-y
       (let ([hue "outside "]
             [diag* (λ (x y)
                      (display "moving "))])
         (method ()
           (display hue)
           (diag* 5 5)
           (let ([hue "inside "]
                 [diag (λ (n self)
                         (diag self n))])
             (display hue)
             (diag 5 it))))])))

(define <escp>-maker
  (λ (x)
    (let-syntax
      ([<<escp>>
         (extend-shadow <<scp>> ()
           ([e (begin
                 (write x)
                 (let ([y 1])
                   (method (q r . args)
                     (+ x y q r (car args)))))]))])
      (λ (s)
        (create-class <<escp>> s)))))
#|
  > (let ([escp (_n ((<escp>-maker 1) <scp>))])
      ((_mp escp 1) escp 10 20 7)
      ((_mv escp 'e) escp 1 1 1))
  114
|#
