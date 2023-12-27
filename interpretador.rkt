#lang racket

(define vars '())
(define listaDeFuncoes '())
(define names '())
(define sub '())
(define zero 0)

(define var_def
  (lambda (names code params)
    (if (null? params)
        (if (null? names)
              (executeFunction (cdr code) params)
            (begin
              (set! vars (cons (list (car names)) vars))
              (var_def (cdr names) code params)))
        (begin
          (if (eq? (car code) 'begin)
              (executeFunction (cdr code) params)
              (begin
                (set! sub (cons (car code) sub))
                (set! sub (cons zero sub))
                (set! params (cons sub params))
                (var_def names (cdr code) params)))))))
        

(define names_var_def
  (lambda (names code)
    (if (eq? (car code) 'begin)
        names
        (names_var_def (cons (car code) names) (cdr code)))))


(define attr_def
  (lambda (code params)
    (begin
      (if (null? params)
          (if (or(eq? (list-ref code 3) '!)(eq? (list-ref code 3) 'fi))
              (set! vars (atribuicaoSimples vars (caddr code) (car code)))
              (set! vars (atribuicaoExpressao vars (caddr code) (cadddr code) (cadr (cdddr code)) (car code))))
          (if (or(eq? (list-ref code 3) '!)(eq? (list-ref code 3) 'fi))
              (set! params (atribuicaoSimples params (caddr code) (car code)))
              (set! params (atribuicaoExpressao params (caddr code) (cadddr code) (cadr (cdddr code)) (car code)))))
      (executeFunction (cdr code) params))))

(define (atribuicaoSimples vars valor name)
  (map (lambda (sublista)
         (if (and (> (length sublista) 1)
                  (not (null? sublista))
                  (eq? name (cadr sublista)))
             (if (> (length sublista) 1)
                 (begin
                   (set! sublista (cons (cadr sublista) '()))
                   (cons (valorVariavel valor vars) sublista))
                 (cons (valorVariavel valor vars) sublista))
             sublista))
       vars))

(define (atribuicaoExpressao vars n1 op n2 name)
  (map (lambda (sublista)
         (if (and (> (length sublista) 1)
                  (not (null? sublista))
                  (eq? name (cadr sublista)))
             (if (> (length sublista) 1)
                 (begin
                   (set! sublista (cons (cadr sublista) '()))
                   (cons (calculaExpressao (valorVariavel n1 vars) op (valorVariavel n2 vars)) sublista))
                   (cons (calculaExpressao (valorVariavel n1 vars) op (valorVariavel n2 vars)) sublista))
             sublista))
       vars))

(define valorVariavel
  (lambda (name vars)
    (if (number? (car (car vars)))
        (if (number? name)
            name
            (if (eq? (cadr (car vars)) name)
                (car (car vars))
                (valorVariavel name (cdr vars))))
        (valorVariavel name (cdr vars))
        )))

(define calculaExpressao
  (lambda (n1 op n2)
    (if (eq? op '+)
        (+ (valorVariavel n1 vars) (valorVariavel n2 vars))
        (if (eq? op '-)
            (- (valorVariavel n1 vars) (valorVariavel n2 vars))
            (if (eq? op '*)
                (* (valorVariavel n1 vars) (valorVariavel n2 vars))
                (/ (valorVariavel n1 vars) (valorVariavel n2 vars)))))))

(define functionIf
   (lambda(n1 op n2)
     (begin
       (if (eq? op 'lt)
         (if (< n1 n2)
             1 0)
         (if (eq? op 'le)
             (if (<= n1 n2)
                 1 0)
             (if (eq? op 'gt)
                 (if (> n1 n2)
                     1 0)
                 (if (eq? op 'ge)
                     (if (>= n1 n2)
                         1 0)
                     (if (eq? op 'eq)
                         (if (= n1 n2)
                             1 0)
                         (if (not(= n1 n2))
                             1 0)))))))))

(define if_def
  (lambda(code params)
    (if (null? params)
        (if (= (functionIf (valorVariavel (car code) vars) (cadr code) (valorVariavel (caddr code) vars)) 1)
            (executeFunction (cdr (cdddr code)))
            (executeFunction (goToFi code)))
        (if (= (functionIf (valorVariavel (car code) params) (cadr code) (valorVariavel (caddr code) params)) 1)
            (executeFunction (cdr (cdddr code)) params)
            (executeFunction (goToFi code) params)) 
            )))

(define goToFi
  (lambda (code)
    (if (eq? (car code) 'fi)
        (cdr code)
        (goToFi (cdr code)))))
        
(define criaNovaFuncao
  (lambda (linha function n newFunction)
    (if (not(empty? function))
        (if (= n 0)
            (if (eq? (car function) 'begin)
                (criaNovaFuncao linha (cdr function) 1 (cons (car function) newFunction))
                (criaNovaFuncao linha (cdr function) 0 (cons (car function) newFunction)))
            (if (= linha n)
                (if (or (eq? (car function) '!) (eq? (car function) 'fi))
                    (criaNovaFuncao linha (cdr function) (+ n 1) newFunction) 
                    (criaNovaFuncao linha (cdr function) n newFunction))
                (if (or (eq? (car function) '!) (eq? (car function) 'fi))
                    (criaNovaFuncao linha (cdr function) (+ n 1) (cons (car function) newFunction))
                    (criaNovaFuncao linha (cdr function) n (cons (car function) newFunction)))))
        (set! listaDeFuncoes (cons (reverse newFunction) listaDeFuncoes)))))

(define delete_def
  (lambda (code lista params)
    (if (eq? (cadr (car lista)) (car code))                 ; delete f @ 2
      (begin        
        (set! listaDeFuncoes (remove-funcao (car lista) listaDeFuncoes))
        (criaNovaFuncao (caddr code)  (car lista) 0 '())
        (executeFunction (cdddr code) params))
      (delete_def code (cdr lista)))
 ))

(define updateFuncao
  (lambda (linha comando function n updatedFunction params)
    (if (not(empty? function))
        (if (= n 0)
            (if (eq? (car function) 'begin)
                (updateFuncao linha comando (cdr function) 1 (cons (car function) updatedFunction) params)
                (updateFuncao linha comando (cdr function) 0 (cons (car function) updatedFunction)params))
            (if (= linha n)
                (if (or (eq? (car function) '!) (eq? (car function) 'fi))
                    (updateFuncao linha comando (cdr function) (+ n 1) updatedFunction params)
                    (updateFuncao linha comando (cdr function) n updatedFunction params))
                (if (= (+ linha 1) n)
                    (if (or (eq? (car comando) '!) (eq? (car comando) 'fi))
                        (updateFuncao linha (cdr comando) function (+ n 1) (cons (car comando) updatedFunction) params) ;adicionar nova linha
                        (updateFuncao linha (cdr comando) function n (cons (car comando) updatedFunction) params))
                    (if (or (eq? (car function) '!) (eq? (car function) 'fi))
                        (updateFuncao linha comando (cdr function) (+ n 1) (cons (car function) updatedFunction params))
                        (updateFuncao linha comando (cdr function) n (cons (car function) updatedFunction params))))))
        (begin
          (set! listaDeFuncoes (cons (reverse updatedFunction) listaDeFuncoes))
          (executeFunction comando params)
        ))))

(define update_def
  (lambda (code lista params)
    (if (eq? (cadr (car lista)) (car code))      ; update f @ 1 : if a gt b then a = 0 fi
        (begin
          (set! listaDeFuncoes (remove-funcao (car lista) listaDeFuncoes))
          (updateFuncao (caddr code) (cdr (cdddr code)) (car lista) 0 '() params))
        (update_def code (cdr lista) params))))

(define remove-funcao
  (lambda (lista lista-de-listas)
    (filter (lambda (lst) (not (equal? lst lista))) lista-de-listas)))

(define functionPrint
  (lambda (param params)
    (if (null? params) 
        (if (string? param)
            (display param)
            (display (valorVariavel param vars)))
        (if (string? param)
            (display param)
            (display (valorVariavel param params))))
    (printf "\n")
  )
)

(define print_def
  (lambda (code params)
    (functionPrint (car (cdr code)) params)
    (executeFunction (cdr (cdddr code)) params)
  )
)

(define read_def
  (lambda (code params)
    (define value 0)
    (set! value (read))
    (executeFunction (cdddr code) params)
))

(define limpaLista
  (lambda (lista)
    (if (null? lista)
        lista
        (limpaLista (cdr lista)))))

(define findFunction
  (lambda (function params listaParams listaFuncao); 1 2 > (a b) (1 2)
    (if (eq? (car params) '>)
        (if (eq? (cadr (car listaFuncao)) function)      ; ((a 1) (b 2))
            (executeFunction (cddr (car listaFuncao)) (criaListaDeParams (cdddr(car listaFuncao)) listaParams '()))
            (findFunction function params listaParams (cdr listaFuncao)))
        (findFunction function (cdr params) (cons (car params) listaParams) listaFuncao))))

(define criaListaDeParams
  (lambda (function lista-valores lista-variaveis)
    (if (eq? (car function) '>)
        (map list lista-valores lista-variaveis)
        (criaListaDeParams (cdr function) lista-valores (cons (car function) lista-variaveis)))))

(define executeFunction
  (lambda (code params)
     (if (null? code)
      (void)
      (begin
          (if (eq? (car code) 'vars)
            (begin (set! names (names_var_def names (cdr code))) (var_def names (cdr code) params))
            (if (eq? (car code) 'print)
                (print_def (cdr code) params)
                   (if (and (>= (length code) 3) (eq? (caddr code) 'read))
                       (read_def code params)
                       (if (eq? (car code) 'end)
                           (limpaLista params)
                           (if (eq? (car code) 'update)
                               (update_def (cdr code) listaDeFuncoes params)
                               (if (eq? (car code) 'if)
                                   (if_def (cdr code) params)
                                   (if (and (>= (length code) 2) (eq? (cadr code) '=))
                                       (attr_def code params)
                                       (if (eq? (car code) 'delete)
                                           (delete_def (cdr code) listaDeFuncoes params)
                                           (if (and (>= (length code) 2) (eq? (cadr code) '<))
                                               (findFunction (car code) (cddr code) '() listaDeFuncoes)
                                               (executeFunction (cdr code) params))))))))
))))))


(define criaListaDeComandos
  (lambda (code lista)
    (if (eq? (car code) 'main)
        (executeFunction (cdr code) '())
        (if (eq? (car code) 'end)
            (begin
              (set! listaDeFuncoes (cons (reverse lista) listaDeFuncoes))
              (criaListaDeComandos (cdr code) '()))
            (criaListaDeComandos (cdr code) (cons (car code) lista))

        )
      )
    )
  )

(define execute
  (lambda (code)
    (criaListaDeComandos code listaDeFuncoes)))

(define code '(
function f < a b >
vars c
begin
 c = 2 !
 print < c > !
 if b gt c then b = 12 fi
 print < a > !
 print < b > !
 c = read < > !
 return 0 !
end
function main < >
vars a b
begin
 a = 2 !
 b = a + 10 !
 f < 1 4 > !
end
))

(execute code)