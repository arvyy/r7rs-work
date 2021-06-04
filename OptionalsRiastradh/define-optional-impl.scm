(define-syntax define-optional
  (syntax-rules ()
    ((_ (name arg ... . rest) body ...)
     (h 1 name (arg ...) rest (body ...)))))

(define-syntax h
  (syntax-rules ()

    ;; ============================
    ;; step 1: separate required and opt args
    ;; ============================
    ;; initialize
    ((_ 1 name arg-lst rest body-lst)
     (h 1 name arg-lst rest body-lst () ()))

    ;; handle opt arg
    ((_ 1 name ((arg-name params ...) arg* ...) rest body-lst required-lst (opt ...))
     (h 1 name (arg* ...) rest body-lst required-lst (opt ... (arg-name params ...))))

    ;; handle required arg
    ((_ 1 name (arg arg* ...) rest body-lst (required ...) opt-lst)
     (h 1 name (arg* ...) rest body-lst (required ... arg) opt-lst))

    ;; handle end of list -- proceed to step 2
    ((_ 1 name () rest body-lst required-lst opt-lst)
     (h 2 name required-lst opt-lst rest body-lst))

    ;; ============================
    ;; step 2: build all combinations by which optional arguments could have been called
    ;; ============================
    ;; initialize
    ((_ 2 name required-lst opt-lst rest body-lst)
     (h 2 name required-lst opt-lst rest body-lst () opt-lst ()))

    ;; take current opt list and add to combinations; remove last opt and repeat until empty
    ((_ 2 name required-lst opt-lst rest body-lst (combination ...) (opt* ... opt) (discarded-opt ...))
     (h 2 name required-lst opt-lst rest body-lst (combination ... ((opt* ... opt) (discarded-opt ...))) (opt* ...) (opt discarded-opt ...)))

    ;; on empty current opt list, don't forget to add empty to combinations as well! Proceed to next step
    ((_ 2 name required-lst opt-lst rest body-lst (combination ...) () (discarded-opt ...))
     (h 3 name required-lst opt-lst rest body-lst (combination ... (() (discarded-opt ...)))))

    ;; ============================
    ;; step 3: for each opt combination, build relavant lambda-case argument clause and fn invocation clause
    ;; ============================
    ;; initialize
    ;; extra variables are lambda-clause-list, fn call list, current-args, current-params, current-combination, rest combinations
    ((_ 3 name required-lst opt-lst rest body-lst (combination combination* ...))
     (h 3 name required-lst opt-lst rest body-lst (combination combination* ...) () () required-lst required-lst combination (combination* ...)))

    ;; take next opt parameter if it has exists? field
    ((_ 3 name required-lst opt-lst rest body-lst combination-lst
        lcc-lst fc-lst (arg ...) (param ...) (((opt-name default exists?) opt* ...) discarded-opt-lst) combination*-lst)
     (h 3 name required-lst opt-lst rest body-lst combination-lst
        lcc-lst fc-lst (arg ... opt-name) (param ... opt-name #t) ((opt* ...) discarded-opt-lst) combination*-lst))

    ;; take next opt parameter if it doesn't have exists? field
    ((_ 3 name required-lst opt-lst rest body-lst combination-lst
        lcc-lst fc-lst (arg ...) (param ...) (((opt-name default) opt* ...) discarded-opt-lst) combination*-lst)
     (h 3 name required-lst opt-lst rest body-lst combination-lst
        lcc-lst fc-lst (arg ... opt-name) (param ... opt-name) ((opt* ...) discarded-opt-lst) combination*-lst))

    ;; take next discarded opt parameter if it has exists? field
    ((_ 3 name required-lst opt-lst rest body-lst combination-lst
        lcc-lst fc-lst (arg ...) (param ...) (() ((discarded-opt-name default exists?) discarded-opt* ...)) combination*-lst)
     (h 3 name required-lst opt-lst rest body-lst combination-lst
        lcc-lst fc-lst (arg ...) (param ... default #f) (() (discarded-opt* ...)) combination*-lst))

    ;; take next discarded opt parameter if it doesn't have exists? field
    ((_ 3 name required-lst opt-lst rest body-lst combination-lst
        lcc-lst fc-lst (arg ...) (param ...) (() ((discarded-opt-name default) discarded-opt* ...)) combination*-lst)
     (h 3 name required-lst opt-lst rest body-lst combination-lst
        lcc-lst fc-lst (arg ...) (param ... default) (() (discarded-opt* ...)) combination*-lst))

    ;; opt parameters exhausted -- add collected parameters / args
    ;; and take next combination
    ((_ 3 name required-lst opt-lst rest body-lst combination-lst
        (lambda-case-clause ...) (fn-call ...) (arg ...) (param ...) (() ()) (combination combination* ...))
     (h 3 name required-lst opt-lst rest body-lst combination-lst
        (lambda-case-clause ... (arg ...)) (fn-call ... (param ...)) required-lst required-lst combination (combination* ...)))

    ;; all combinations exhausted -- add collected parameters / args
    ;; and proceed to next step
    ((_ 3 name required-lst opt-lst rest body-lst combination-lst
        (lambda-case-clause ...) (fn-call ...) (arg ...) (param ...) (() ()) ())
     (h 4 name rest body-lst
        (lambda-case-clause ... (arg ...)) (fn-call ... (param ...)) required-lst opt-lst))

    ;; ============================
    ;; step 4: build fn lambda formals
    ;; ============================
    ;; init formals list; use #t to separate init from working cases
    ((_ 4 name rest body-lst lcc-lst fc-lst required-lst opt-lst)
     (h 4 name rest body-lst lcc-lst fc-lst required-lst opt-lst #t))

    ((_ 4 name rest body-lst lcc-lst fc-lst (formal ...) ((opt-name default present?) opt* ...) #t)
     (h 4 name rest body-lst lcc-lst fc-lst (formal ... opt-name present?) (opt* ...) #t))

    ((_ 4 name rest body-lst lcc-lst fc-lst (formal ...) ((opt-name default) opt* ...) #t)
     (h 4 name rest body-lst lcc-lst fc-lst (formal ... opt-name) (opt* ...) #t))

    ((_ 4 name rest body-lst lcc-lst fc-lst formal-lst () #t)
     (h 5 name rest body-lst lcc-lst fc-lst formal-lst))


    ;; ============================
    ;; step 5: put everything together
    ;; ============================
    ;; treat first case of lambda-case specially to attach rest argument
    ((_ 5 name rest (body ...) ((lambda-case-arg ...) lambda-case-clause* ...) ((param ...) (param* ...) ...) (fn-formal ...))
     (define name (let ((fn (lambda (fn-formal ... . rest) body ...)))
                    (case-lambda
                      ((lambda-case-arg ... . rest) (apply fn param ... rest))
                      (lambda-case-clause* (fn param* ...)) ...))))))
