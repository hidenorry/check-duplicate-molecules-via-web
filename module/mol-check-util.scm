#!/bin/sh
:;if which gosh > /dev/null 2>&1; then
:;   exec gosh -- "$0" "$@"
:;else
:;   ~/apl/gauche/bin/gosh -- "$0" "$@"
:;fi

(define-module mol-check-util
  (use text.tree)
  (use srfi-1)
  (use srfi-13)
  (use mol-check-var)
  (export nest-reverse
          flatten
          count-elem
          prune
          sym2lis
          ntimes-cadr
          ntimes-cons
          string-cdr
          string-car
          string-car-x
          string-cdr-x
          symbollist
          sym2lis
          parse-inp
          mol->atm
          ))
(select-module mol-check-util)

(define (nest-reverse lis)
  ;;(nest-reverse '((1 2 3) 4 (5 6 7) 8 (((9 10))))) => ((((10 9))) 8 (7 6 5) 4 (3 2 1))
  (define (rec lis acc)
    (cond ((null? lis) acc)
          ((pair? (car lis)) (rec (cdr lis) (cons (rec (car lis) '()) acc)))
          (else (rec (cdr lis) (cons (car lis) acc)))))
  (rec lis '()))

(define (flatten lis)
  (define (rec lis acc)
    (cond ((null? lis) (reverse acc))
          ((pair? lis) (rec (car lis) (rec (cdr lis) acc)))
          (else (cons lis acc))))
  (rec lis '()))

(define (count-elem lis)
  ;;(count-elem '(A B C A A B)) => ((C 1) (B 2) (A 3)) ; not dotted list because of using proc prune later.
  (define (rec lis acc)
    (cond ((null? lis) acc)
          ((assoc (car lis) acc) => (lambda (elem) (set-cdr! elem (list (+ 1 (cadr elem)))) (rec (cdr lis) acc)))
          (else (rec (cdr lis) (cons (list (car lis) 1) acc)))))
  (rec lis '()))

(define (prune pred? lis)
  (define (rec lis acc)
    (cond ((null? lis) (reverse acc))
          ((pair? (car lis)) (rec (cdr lis) (cons (rec (car lis) '()) acc )))
          ((pred? (car lis)) (rec (cdr lis) acc))
          (else (rec (cdr lis) (cons (car lis) acc)))))
  (rec lis '()))

(define (ntimes-cadr lis)
  ;;(ntimes-cadr '(a b 3 (c d) 4 e)) => (a b (c d) (c d) (c d) e e e e)
  (define (ntimes-cons times x y)
    (if (= times 0)
        y
        (cons x (ntimes-cons (dec! times) x y))))
  (define (rec lis acc)
    (cond ((null? lis)  (reverse acc))
          ((pair? (car lis)) (rec (cdr lis) (cons (rec (car lis) '()) acc)))
          ((number? (car lis)) (rec (ntimes-cons (car lis) (cadr lis) (cddr lis)) acc))
          (else (rec (cdr lis) (cons (car lis) acc)))))
  (rec lis '()))

(define (string-cdr str)
  (let1 len (string-length str)
    (substring str 1 len)))

(define (string-car str)
  (let1 len (string-length str)
    (substring str 0 1)))

(define-macro (string-car-x-helper str acc proc . rest)
  `(if (string-null? ,str)
       (,proc ,acc)
       (let1 head (substring ,str 0 1)
         ,@rest)))
(define (string-car-x strng)
  ;;"22AbD" => 22; "AbD" => Ab; "D" => D; "" => ""; "ABC" => A; "abcde" => ab
  (define (rec-num str acc)
    (string-car-x-helper str acc string->number
           (cond  ((string->number head) (rec-num (string-cdr str) (string-append acc head)))
                  (else (string->number acc)))))
  (define (rec-lc str acc)
    (string-car-x-helper str acc string->symbol
           (cond  ((and (< 1 (string-length str))
                        (char-lower-case? (string-ref str 1)))
                   (rec-lc (string-cdr str) (string-append acc head)))
                  (else (string->symbol (string-append acc head))))))
  (define (rec str acc)
    (string-car-x-helper str acc identity
           (cond  ((string->number head) (rec-num (string-cdr str) (string-append acc head)))
                  ((and (< 1 (string-length str))
                        (char-lower-case? (string-ref str 1)))
                   (rec-lc (string-cdr str) (string-append acc head)))
                  (else (string->symbol head)))))
  (rec strng ""))

(define (string-cdr-x str)
  ;;"22AbD" => "AbD"; "AbD" => "D"; "D" => ""; "" => ""; "ABC" => "BC"; "abdce" => "dce"
  (let ((fin-len (string-length str))
        (str-len (string-length (x->string (string-car-x str)))))
     (substring str str-len fin-len)))

(define (symbollist lis)
  ;;(symbollist '(ABC 2 (DE3F) GH I)) => (A B C 2 (D E 3 F) G H I)
  (define (rec lis acc)
    (cond ((null? lis) (reverse acc))
          ((pair? (car lis)) (rec (cdr lis) (cons (rec (car lis) '()) acc)))
          (else (rec (cdr lis) (append (reverse (sym2lis (car lis)))  acc)))))
  (rec lis '()))

(define (sym2lis sym)
  ;;(sym2lis 'AB2D) => (A B 2 D)
  (define str (x->string sym))
  (define (str2lis str acc)
    (cond ((string-null? str) (reverse acc))
          (else (str2lis (string-cdr-x str) (cons (string-car-x str) acc)))))
  (str2lis str '()))

(define (parse-inp lis)
  ;;(parse-inp '("AB, CD " "  " "EF,G  ;t1, t2" "H,I,J;;;")) => ("AB" "CD" "" "EF" "G" "H" "I" "J")
  (define (get-any-key str key-lis)
    (define (get-key str key) (if (string-scan str key) key #f))
    (define (rec key-lis)
      (cond ((null? key-lis) #f)
            ((get-key str (car key-lis)) => (lambda (x) x))
            (else (rec (cdr key-lis)))))
    (rec key-lis)) 
  (define (rec lis acc)
    (cond ((null? lis) (map string-trim-both (reverse acc)))
          ((get-any-key (car lis) *comment-out-strings*)
           => (lambda (key) (rec (cons (string-take (car lis) (string-scan (car lis) key)) (cdr lis)) acc)))
          ((string-any #[,*/] (car lis))
           (rec (append (string-split (car lis) #[,*/]) (cdr lis)) acc))
          (else (rec (cdr lis) (cons (car lis) acc)))))
  (rec lis '()))

(define (mol->atm lis)
 ;;(mol->atm (AB(CrDd2)3)) => (A B Cr Cr Cr Dd Dd Dd Dd Dd Dd)
 (set! lis (symbollist lis))
 (sort
  (flatten
   (ntimes-cadr
    (nest-reverse lis)))
  (lambda (x y) (string<= (x->string x) (x->string y)))))
