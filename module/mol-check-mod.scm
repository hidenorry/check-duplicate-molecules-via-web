#!/bin/sh
:;if which gosh > /dev/null;then 2>&1
:;   exec gosh -- "$0" "$@"
:;else
:;   ~/apl/gauche/bin/gosh -- "$0" "$@"
:;fi

(define-module mol-check-mod
  (add-load-path ".")
  (use www.cgi)
  (use text.html-lite)
  (use text.tree)
  (use srfi-1)
  (use srfi-13)
  (use mol-check-var)
  (use mol-check-util)
  (export-all))
(select-module mol-check-mod)

(define *exit* #f)

(define *style* "span.notok {background-color : #ffcccc;}")
(define (exist? atom)
  (if (or (number? atom)
          (member atom *atoms*))
      atom
      (string->symbol (tree->string (html:span :class "notok" (html:big (html:strong atom)))))))

(define (str->list str)
  (let1 in-port (open-input-string str)
    (guard (e (else
               (close-input-port in-port)
               (*exit* (html:p (html:strong "\n" "*** error occured while reading following molecule. : " str "\n")))))
      (rlet1 lis (port->list read in-port)
             (close-input-port in-port)))))

(define (original-print new-lis old-lis)
  (define len (length new-lis))
  (define (rec lis sublis acc)
    (cond ((null? lis) (reverse acc))
          ((member (car lis) (cdr lis)) =>
           (lambda (mlis)
              (rec (cdr lis) (cdr sublis)
                   (cons (html:span :class "notok" (format #f "<del>~3d ~3d  ~20a <= ~50a</del>"
                                  (- len (length lis)) (- len (length mlis))
                                  (check-string (car lis)) (car sublis))) acc))))
          (else
           (rec (cdr lis) (cdr sublis) (cons (format #f "~3d ~3a  ~20a <= ~50a" (- len (length lis))'ok
                                                     (check-string (car lis)) (car sublis)) acc)))))
  (rec new-lis old-lis '()))

(define (check-string word)
  (define (rec str acc)
    (cond ((string-null? str) (reverse acc))
          ((exist? (string-car-x str)) =>
           (lambda (it) (rec (string-cdr-x str) (cons it acc))))))
  (tree->string (rec word '())))

(define (mol-list->correct-one data)
  (define (line-list->string lis)
    (define (rec lis acc)
      (cond ((null? lis) acc)
            (else (rec (cdr lis) (string-append acc (x->string (car lis)))))))
    (rec lis ""))
  (let1 acc (reverse data)
        (set! acc (filter (complement null?) (map str->list (parse-inp acc))))
        (let1 acc2 (map (lambda (line) (tree->string (prune (lambda (x) (equal? x 1)) (count-elem (mol->atm line))))) acc)
          (original-print acc2 acc))))
;(mol-list->correct-one '("CH3 ;ama" "CH3COO-,ch4"))
