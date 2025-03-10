#lang racket

(require dcc019/exercise/logic/ast)

(provide eval-query)

(define (eval-query prog query)
  (printf "Programa:~n ~a~n" prog) ; Representação do programa
  (printf "Query:~n ~a~n" query)   ; A query
  (resolve query prog))  ; Chama a resolução SLD

;; Função principal de resolução SLD
(define (resolve query prog)
  (match query
    [(ast:functor name args)
     (match (find-clause name prog)
       [#f #f]  ; Nenhuma cláusula correspondente
       [(ast:clause head body)
        (let ([substitutions (unify query head)])
          (if substitutions
              (resolve-body body prog substitutions)
              #f))])]))

;; Busca uma cláusula no programa correspondente ao nome do functor
(define (find-clause name prog)
  (findf (lambda (clause)
           (match clause
             [(ast:clause (ast:functor head-name _) _)
              (equal? name head-name)]
             [_ #f]))
         prog))

;; Resolve o corpo de uma cláusula recursivamente
(define (resolve-body body prog substitutions)
  (cond
    [(null? body) substitutions]  ; Se não há mais corpo, a resolução foi bem-sucedida
    [else
     (let ([substituted-query (apply-substitutions (car body) substitutions)])
       (match (resolve substituted-query prog)
         [#f #f]  ; Falha na resolução
         [new-substitutions
          (resolve-body (cdr body) prog (merge-substitutions substitutions new-substitutions))])])]))

;; Substitui variáveis na consulta com base em um conjunto de substituições
(define (apply-substitutions term substitutions)
  (match term
    [(ast:var name) (hash-ref substitutions name term)]
    [(ast:functor name args)
     (ast:functor name (map (lambda (arg) (apply-substitutions arg substitutions)) args))]
    [_ term]))

;; Une dois conjuntos de substituições
(define (merge-substitutions s1 s2)
  (hash-union s1 s2 #:combine (lambda (_ v1 v2) v2)))

;; Unificação de termos
(define (unify term1 term2)
  (cond
    [(ast:var? term1) (hash (ast:var-name term1) term2)]
    [(ast:var? term2) (hash (ast:var-name term2) term1)]
    [(ast:atom? term1) 
     (and (ast:atom? term2) 
          (equal? (ast:atom-name term1) (ast:atom-name term2)) 
          (hash))]
    [(ast:functor? term1)
     (and (ast:functor? term2)
          (equal? (ast:functor-name term1) (ast:functor-name term2))
          (unify-list (ast:functor-args term1) (ast:functor-args term2)))]
    [else #f]))

;; Unificação de listas de termos
(define (unify-list lst1 lst2)
  (if (equal? (length lst1) (length lst2))
      (foldl (lambda (pair acc)
               (match acc
                 [#f #f]
                 [substitutions
                  (match (unify (car pair) (cdr pair))
                    [#f #f]
                    [new-substitutions (merge-substitutions substitutions new-substitutions)])]))
             (hash) (map cons lst1 lst2))
      #f))
