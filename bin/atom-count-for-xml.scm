#!/usr/bin/env gosh
;;; count number of atoms from nested molecular formula for xml.
;;; (CH3)2O => OH6C2

(use text.tree)
(use srfi-1)
(use srfi-13)
(use sxml.ssax)

(add-load-path "./module")
(add-load-path "../module")
(add-load-path "~/public_html/module")
(use www.cgi)
(use text.html-lite)
(use text.tree)
(use srfi-1)
(use srfi-13)
(use slib)
(require 'format)

(use mol-check-var)
(use mol-check-util)
(use mol-check-mod)

(define (string->list/molcheck str)
  ;;(mol->atm "AB(CrDd2)3") => (A B Cr Cr Cr Dd Dd Dd Dd Dd Dd)
  (define in (open-input-string str))
  (guard (e (else (format #t "malformed molecule : ~a~%" str)
                  (raise e)))
    (port->list read in)))

(define (nest-multi-assoc key lis :optional (leaf identity))
  ;;(nest-multi-assoc 'c '(((a 1)) (b (c 2)) ((d c 3)) (c (c 4)) (c (g 9))))=>((c 2) (c 3) (c (c 4)) (c (g 9)))
  (define (pred? x) (equal? x key))
  (define (rec lis acc)
    (cond ((null? lis)  acc)
          ((pair? (car lis)) (rec (cdr lis) (rec (car lis) acc)))
          ((pred? (car lis)) (append acc (list (leaf lis))))
          (else (rec (cdr lis) acc))))
  (rec lis '()))

(define (contract-mol mol)
  ;;(contract-mol "(OCH3)2(CHCl2)(POOH)-") => "PO4H8Cl2C3-"
  (tree->string (prune (^x (equal? x 1))
                       (count-elem (mol->atm (string->list/molcheck mol))))))

(define (multi-member-num key lis)
  ;;(multi-member-num 3 '(3 1 2 3 4 3 2 1 2 3)) => (0 3 5 9)
  (define (rec lis acc)
    (cond ((null? lis) acc)
          ((member key lis) => (^ (it) (rec (cdr it) (cons (- (length it) 1) acc))))
          (else acc)))
  (rec (reverse lis) '()))

(define (make-print)
  (let ((acc '()))
    (^ (mol1 mol2 page)
       (cond ((member mol1 acc) =>
              (^ (it) (format #t "~3d ~5a  ~20a <= ~10a  ~5a~% "
                              (length acc)
                              (multi-member-num mol1 (reverse it))
                              mol1 mol2 page)))
             (else (format #t "~3d ~5a ~20a <= ~10a   ~5a~%" (length acc) 'ok mol1 mol2 page)))
       (set! acc (cons mol1 acc)))))

(define (main args)
  (let* ((in (open-input-file (cadr args)))
         ;;(in (open-input-file "./foo.xml"))
         (lis (ssax:xml->sxml in '()))
         (close-input-port in)
         (eachart-atomlis
          (map (^ (art) (nest-multi-assoc 'substance art last)) (nest-multi-assoc 'article lis)))
         (eachart-pages
          (map (^ (art) (nest-multi-assoc 'pageBegin art last)) (nest-multi-assoc 'article lis))))
    (for-each
     (^ (atom-lis page)
        (let1 pr (make-print)
            (for-each pr (map contract-mol atom-lis) atom-lis (make-list (length atom-lis) page))))
     eachart-atomlis eachart-pages)))
