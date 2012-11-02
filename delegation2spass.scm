(module delegation2spass scheme
        (provide translate-file pretty-print-spass)

        (define *agents* '())
        (define *atoms* '())

        (require parser-tools/yacc
                 parser-tools/lex
                 (prefix-in : parser-tools/lex-sre)
                 syntax/readerr
                 scheme/string
                 scheme/cmdline
                 scheme/match)

        (define-tokens value-tokens (PROP_ATOM SUPERS SIGN))
        (define-empty-tokens op-tokens (EOF SOA LPAREN RPAREN COMMA CARET UNDERSCORE AMPERAMPER NOT LSQBRACKET RSQBRACKET PLUS MINUS QUESTION BARBAR COLON))

        (define-lex-abbrevs
          (comment (:or (:: "//" (:* (:~ #\newline)) #\newline) (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))) ; C style
          ;(comment (:: "(*" (complement (:: any-string "*)" any-string)) "*)")) ; OCaml style
          (lower-letter (:/ "a" "z"))
          (upper-letter (:/ #\A #\Z))
          (digit (:/ "0" "9")))

        (define dsll
          (lexer-src-pos
            ((eof) 'EOF)
            ((:or comment #\newline #\return #\tab #\space #\vtab) (return-without-pos (dsll input-port)))
            ("soa" 'SOA)
            ("(" 'LPAREN)
            (")" 'RPAREN)
            ("," 'COMMA)
            ("^" 'CARET)
            ("_" 'UNDERSCORE)
            ("&&" 'AMPERAMPER)
            ("D" (token-SUPERS 'D))
            ("P" (token-SUPERS 'P))
            ("+" (token-SIGN 'plus))
            ("-" (token-SIGN 'minus))
            ("not" 'NOT)
            ("[" 'LSQBRACKET)
            ("]" 'RSQBRACKET)
            ("+" 'PLUS)
            ("-" 'MINUS)
            ("?" 'QUESTION)
            ("||" 'BARBAR)
            (";" 'COLON)
            ((:: (:+ (:or lower-letter upper-letter)) (:* (:or lower-letter upper-letter digit))) (token-PROP_ATOM (string->symbol lexeme)))))

        (define dslp
          (lambda (source-name)
            (parser
              (src-pos)
              (start start)
              (end EOF)
              (tokens value-tokens op-tokens)
              (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                       (raise-read-error
                         (format "unexpected ~a" tok-name)
                         source-name
                         (position-line start-pos)
                         (position-col start-pos)
                         (position-offset start-pos)
                         (- (position-offset end-pos)
                            (position-offset start-pos)))))

              (precs (left AMPERAMPER)
                     (left NOT)
                     (left PLUS)
                     (left MINUS)
                     (left BARBAR)
                     (left COLON))

              (grammar
                (start (() #f)
                       ((phi) $1))
                (agent ((PROP_ATOM) (begin
                                     (set! *agents* (cons $1 *agents*))
                                     $1)))
                (atom ((PROP_ATOM) (begin
                                     (set! *atoms* (cons $1 *atoms*))
                                     $1)))
                (p ((SOA UNDERSCORE agent COMMA atom) `(soa ,$3 ,$5))
                   ((LPAREN agent COMMA LPAREN atom COMMA atom RPAREN COMMA agent RPAREN CARET SUPERS UNDERSCORE SIGN) (list (match (list $13 $15)
                                                                                                                                    ((list 'D 'plus) 'p_d)
                                                                                                                                    ((list 'D 'minus) 'n_d)
                                                                                                                                    ((list 'P 'plus) 'p_p)
                                                                                                                                    ((list 'P 'minus) 'n_p))
                                                                                                                             $2 $5 $7 $10)))
                (pi ((SIGN p) (list $1 $2))
                    ((phi QUESTION) `(question ,$1))
                    ((pi BARBAR pi) (prec BARBAR) `(or ,$1 ,$3))
                    ((pi COLON pi) (prec COLON) `(colon ,$1 ,$3)))
                (phi ((p) $1)
                     ((p-phi) $1)
                     ((phi AMPERAMPER phi) `(and ,$1 ,$3))
                     ((NOT phi) `(not ,$2))
                     ((LSQBRACKET pi RSQBRACKET p-phi) `(sq ,$2 ,$4)))
                (p-phi ((LPAREN phi RPAREN) $2))))))


        (define translate
          (lambda (s #:src-name (src-name "current-input-port"))
            (let ((ois (open-input-string s)) (statements '()))
              (port-count-lines! ois)
              ((dslp src-name) (lambda () (dsll ois))))))

        (define translate-file
          (lambda (path)
            (call-with-input-file path
                                  (lambda (in)
                                    (translate (port->string in) #:src-name path)))))

        (define reduce
          (lambda (code)
            (case (car code)
              ((sq) (match (cdr code)
                           ((list (list 'plus x) (list 'implies y z)) (list 'implies (reduce (list 'sq (list 'plus x) y)) (reduce (list 'sq (list 'plus x) z))))    ; K+
                           ((list (list 'minus x) (list 'implies y z)) (list 'implies (reduce (list 'sq (list 'minus x) y)) (reduce (list 'sq (list 'minus x) z)))) ; K-
                           ((list (list 'question x) y) (reduce (list 'implies x y)))                                                                               ; Test
                           ((list (list 'colon x y) z) (reduce (list 'sq x (reduce (list 'sq y z)))))                                                               ; Comp
                           ((list (list 'or x y) z) (list 'and (reduce (list 'sq x z)) (reduce (list 'sq y z))))                                                    ; Choice
                           ((list (list 'plus x) x) 'true)                                                                                                          ; Red1
                           ((list (list 'plus x) (list 'not x)) 'false)                                                                                             ; Red2
                           ((list (list 'plus x) y) y)                                                                                                              ; Red3
                           ((list (list 'minus x) x) 'false)                                                                                                        ; Red4
                           ((list (list 'minus x) (list 'not x)) 'true)                                                                                             ; Red5
                           ((list (list 'minus x) y) y)))                                                                                                           ; Red6
              ((and) `(and ,(reduce (list-ref code 1)) ,(reduce (list-ref code 2))))
              ((not) `(not ,(reduce (list-ref code 1))))
              (else code))))

         (define pretty-print-spass
           (lambda (code)
             (cond
               ((symbol? code) (symbol->string code))
               ((number? code) (number->string code))
               (else
                 (case (car code)
                   ((formula) (format "~n\tformula(~a)." (pretty-print-spass (list-ref code 1))))
                   (else (format "~a(~a)" (car code) (string-join (map (lambda (e) (pretty-print-spass e)) (cdr code)) ", "))))))))

        (define pretty-print-symbols
          (lambda (symbols)
            (let ((listify (lambda (name ls)
                             (format "~a[~a]." name (string-join (map (lambda (e)
                                                                        (apply format "(~a, ~a)" (if (list? e)
                                                                                                   (list (car e) (cadr e))
                                                                                                   (list e 0)))) ls) ", ")))))
              (format "list_of_symbols.~n~n\t~a~n\t~a~n\tsorts[agent].~n~nend_of_list.~n~n"
                      (listify "functions" symbols)
                      (listify "predicates" '((soa 2) (p_p 4) (p_d 4) (n_p 4) (n_d 4)))))))


        (define pretty-print-declarations
          (lambda (agents)
            (let ((listify (lambda (sort-name ls)
                             (foldr string-append
                                    ""
                                    (map (lambda (e)
                                           (format "~n\t~a(~a)." sort-name e)) ls)))))
              (format "list_of_declarations.~n~a~n~nend_of_list.~n~n"
                      (listify "agent" agents)))))


        (define main
          (command-line
            #:args (filename)
            filename))

        (let* ((filename main) (code (translate-file filename)) (agents (remove-duplicates (reverse *agents*))) (atoms (remove-duplicates (reverse *atoms*))))
          (display (format "begin_problem(problem_name).~n~nlist_of_descriptions.~n~n\tname({*Problem's name*}).~n\tauthor({*Author*}).~n\tstatus(unsatisfiable). % or satisfiable or unknown~n\tdescription({*Description*}).~n~nend_of_list.~n~n~a~alist_of_formulae(axioms).~n~n\t~n~nend_of_list.~n~nlist_of_formulae(conjectures).~n~a~n~nend_of_list.~n~nend_problem.~n"
                           (pretty-print-symbols (append agents atoms))
                           (pretty-print-declarations agents)
                           (pretty-print-spass (list 'formula (reduce code)))))))
