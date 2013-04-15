#!/bin/sh
:; if which gosh > /dev/null 2>&1
:; then
:;    exec gosh -- "$0" "$@"
:; else
:;    exec ~/apl/gauche/bin/gosh -- "$0" "$@"
:; fi

(add-load-path "./module")
(add-load-path "~/public_html/module")
(use www.cgi)
(use text.html-lite)
(use text.tree)
(use srfi-1)
(use srfi-13)

(use mol-check-var)
(use mol-check-util)
(use mol-check-mod)

(define (page . content)
  `(,(cgi-header)
    ,(html:html
      (html:head (html:title "table")
                 (html:style :type "text/css" *style*))
      (apply html:body content))))

(define (insert :optional (param param))
  (page
   (apply html:pre "使用できるコメントアウト " (map (lambda (x y) (string-append x y ))
                *comment-out-strings* (make-list (length *comment-out-strings*) " ")))
   (html:pre "使用できる原子" *atoms*)
   (html:p "分子式を記入し、'check'ボタンを押して下さい。")
   (html:form
    (html:p (html:textarea :rows 8 :cols 100 :name "p"))
    (html:p (html:input :type "submit" :name "submit" :value "check"))
    (let/cc cc
      (set! *exit* cc)
      (if (cgi-get-parameter "p" param)
          (map html:pre
               (mol-list->correct-one (reverse (string-split (cgi-get-parameter "p" param) "\n"))))))
    )))

(define (main args)
  (cgi-main
   ;;(^ (params)(insert))
   (lambda (param)
     (insert param))))
