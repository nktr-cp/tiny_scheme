;; Frame operations
(define (empty-frame)
	(list)
)

(define (update frame var val)
	(cons (cons var val) frame)
)

(define (lookup var frame)
	(assoc var frame)
)

;; environment operations
(define (make-env)
	(list (empty-frame))
)

(define (extend-env env)
	(cons (empty-frame) env)
)

(define (define-var env var val)
	(if (null? env)
		#f
		(set-car! env (update (car env) var val))
	)
	env
)

(define (lookup-var var env)
	(if (null? env)
		#f
		(let ((found (lookup var (car env))))
			(if (pair? found)
				found
				(lookup-var var (cdr env))
			)
		)
	)
)

;; lambda closure operations
;; lambda closure has *lambda symbols,
;; parameters and function body code and environment
(define (make-closure env params body)
	(cons '*lambda* (cons env (cons params body)))
)

(define (data-closure? data)
	(and (pair? data) (equal? (car data) '*lambda*))
)

(define closure-env cadr)
(define closure-params caddr)
(define closure-body cdddr)

;; primitive operations with specified arity and function
(define (make-primitive arity fun)
	(list '*primitive* arity fun)
)

(define (data-primitive? data)
	(and (pair? data) (equal? (car data) '*primitive*))
)

(define primitive-arity cadr)
(define primitive-fun caddr)

;; function to print data
(define (print-data data)
	(cond
		((data-closure? data) (display "#<closure>"))
		((data-primitive? data) (display "#<primitive>"))
		((equal? data '*unspecified*) (display "#<unspecified>"))
		((equal? data '*error*) (display "#<error>"))
		((equal? data '*exit*))
		(else (write data))
	)
)

;; evaluation
(define (base-eval env exp)
	(cond
		((eof-object? exp) (cons env '*exit*))
		((constant? exp) (cons env exp))
		((symbol? exp) (var-eval env exp))
		((not (pair? exp)) (eval-error env 'unknown-data exp))
		((equal? (car exp) 'exit) (cons env '*exit*))
		((equal? (car exp) 'define) (def-eval env exp))
		((equal? (car exp) 'let) (let-eval env exp))
		((equal? (car exp) 'letrec) (letrec-eval env exp))
		((equal? (car exp) 'lambda) (lambda-eval env exp))
		((equal? (car exp) 'if) (if-eval env exp))
		((equal? (car exp) 'begin) (begin-eval env exp))
		((equal? (car exp) 'quote) (quote-eval env exp))
		(else (app-eval env exp))
	)
)

(define (constant? exp)
	(or (boolean? exp) (number? exp) (string? exp))
)

(define (eval-error env type exp)
	(display "ERROR: ")
	(write type)
	(display ": ")
	(print-data exp)
	(newline)
	(cons env '*error*)
)

;; always returns #t
(define (correct-syntax? type exp) #t)

(define (let->app exp)
	(let ((decl (cadr exp)) (body (cddr exp)))
	(cons (cons 'lambda (cons (map car decl) body))
	(map cadr decl)))
)

(define (let-eval env exp)
	(if (correct-syntax? 'let exp)
		(base-eval env (let->app exp))
		(eval-error env 'syntax-error exp)
	)
)

(define (letrec-eval exp env)
	(if (correct-syntax? 'letrec exp)
		(let* ((cl (let->app exp)) (cenv (cons (closure-env cl) env)))
			(cons env (cdr (base-eval cl cenv)))
		)
		(eval-error env 'syntax-error exp)
	)
)

(define (def-eval env exp)
	(if (correct-syntax? 'define exp)
		(let*
		((var (cadr exp))
			(res (base-eval env (caddr exp)))
			(env (car res))
			(val (cdr res)))
		(cons (define-var env var val) var))
		(eval-error env 'syntax-error exp)
	)
)

(define (var-eval env exp)
	(let ((found (lookup-var exp env)))
	(if (pair? found)
		(cons env (cdr found))
		(eval-error env 'variable-not-found exp)
	))
)

(define (lambda-eval env exp)
	(if (correct-syntax? 'lambda exp)
		(cons env (make-closure env (cadr exp) (cddr exp)))
		(eval-error env 'syntax-error exp)
	)
)

(define (map-base-eval env el)
	(cons env (map (lambda (exp) (cdr (base-eval env exp))) el))
)

(define (define-var-multiple env vars vals)
	(if (null? vars)
		env
		(define-var-multiple (define-var env (car vars) (car vals)) (cdr vars) (cdr vals))
	)
)

(define (base-apply env fun args)
	(cond 
		((data-closure? fun)
		(let* ((params (closure-params fun))
				(extended-env (extend-env (closure-env fun)))
				(updated-env (define-var-multiple extended-env params args))
				(body (closure-body fun))
				)
			(cons env (cdr (base-eval updated-env (car body)))))
		)
		((data-primitive? fun)
		(if (or (not (number? (primitive-arity fun))) (= (primitive-arity fun) (length args)))
			((primitive-fun fun) env args)
			(eval-error env 'wrong-number-of-args fun)
		))
		(else
			(eval-error env 'non-function fun)
		)
	)
)

(define (app-eval env exp)
	(if (correct-syntax? 'app exp)
		(let* ((l (map-base-eval env exp))
		(env (car l))
		(fun (cadr l))
		(args (cddr l)))
		(base-apply env fun args))
		(eval-error env 'synatx-error exp)
	)
)

(define (if-eval env exp)
	(if (correct-syntax? 'if exp)
		(let* ((cond-res (base-eval env (cadr exp)))
			  (ok (cdr cond-res)))
		(if ok
			(base-eval env (caddr exp))
			(base-eval env (if (not (null? (cdddr exp)))
				(cadddr exp)
				#f
			))
		))
		(eval-error env 'synatx-error exp)
	)
)

(define (quote-eval env exp)
	(if (correct-syntax? 'quote exp)
		(cons env (cadr exp))
		(eval-error env 'syntax-error exp)
	)
)

; (define (begin-eval env exp)
; 	(if (correct-syntax? 'begin exp)
; 		()
; 		(eval-error env 'syntax-error exp)
; 	)
; )

;; function to assign primitive function data to function names
(define (make-top-env)
	(let* ((env (make-env))
		(env
			(define-var env '=
				(make-primitive 2 (lambda (env args) (cons env (= (car args) (cadr args)))))
			))
		(env
			(define-var env '+
				(make-primitive 2 (lambda (env args) (cons env (+ (car args) (cadr args)))))
			))
		(env
			(define-var env '-
				(make-primitive 2 (lambda (env args) (cons env (- (car args) (cadr args)))))
			))
		(env
			(define-var env '*
				(make-primitive 2 (lambda (env args) (cons env (* (car args) (cadr args)))))
			))
		(env
			(define-var env '<
				(make-primitive 2 (lambda (env args) (cons env (< (car args) (cadr args)))))
			))
		(env
			(define-var env '>
				(make-primitive 2 (lambda (env args) (cons env (> (car args) (cadr args)))))
			))
		(env
			(define-var env 'cons
				(make-primitive 2 (lambda (env args) (cons env (cons (car args) (cadr args)))))
			))
		(env
			(define-var env 'car
				(make-primitive 1 (lambda (env args) (cons env (car (car args)))))
			))
		(env
			(define-var env 'cdr
				(make-primitive 1 (lambda (env args) (cons env (cdr (car args)))))
			))
		(env
			(define-var env 'list
				(make-primitive #f (lambda (env args) (cons env args)))
			))
		(env
			(define-var env 'null?
				(make-primitive 1 (lambda (env args) (cons env (null? (car args)))))
			))
		(env
			(define-var env 'equal?
				(make-primitive 2 (lambda (env args) (cons env (equal? (car args) (cadr args)))))
			))
		(env
			(define-var env 'display
				(make-primitive 1 (lambda (env args) (display (car args)) (cons env '*unspecified*)))
			))
		(env
			(define-var env 'load
				(make-primitive
					1
					(lambda (env args)
						(with-input-from-file (car args)
						(lambda ()
					(define (re-loop env)
						(let* ((res (base-eval env (read)))
						(env (car res))
						(val (cdr res)))
						(if (equal? val '*exit*)
							(cons env '*unspecified*)
							(re-loop env)
						))
					)
					(re-loop env)))))
			))
		)
	env)
)

(define (scheme)
(let ((top-env (make-top-env)))
  (define (rep-loop env)
	(display "my-scheme> ")
	(let* ((res (base-eval env (read)))
		   (env (car res))
		   (val (cdr res)))
	  (print-data val)
	  (newline)
	  (if (equal? val '*exit*)
		  #t
		  (rep-loop env))))
  (rep-loop top-env)))