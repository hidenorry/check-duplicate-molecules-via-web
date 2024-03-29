#!/usr/bin/env gosh

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

(define (main args)
  (define (line-list->string lis)
    (define (rec lis acc)
      (cond ((null? lis) acc)
            (else (rec (cdr lis) (string-append acc (x->string (car lis)))))))
    (rec lis ""))
  (let ((acc '()))
    (with-input-from-file (cadr args)
      (^ ()
         (port-for-each
          (^ (line) (set! acc (cons line acc)))
          read-line)))
    (let1 acc (reverse acc)
      (set! acc (filter (complement null?) (map str->list (parse-inp acc))))
      (let1 acc2 (map (^ (line) (tree->string (prune (^x (equal? x 1)) (count-elem (mol->atm line))))) acc)
        (format #t "~{~a~%~}" (original-print acc2 (map line-list->string acc)))))))
