#! /usr/bin/env racket

#lang racket

;; raco pkg install css-expr sxml
(require css-expr sxml)

(define (read-all)
  (let loop ((forms '()))
    (let ((form (read)))
      (if (eof-object? form) (reverse forms) (loop (cons form forms))))))

(define (string->file string file)
  (call-with-atomic-output-file
   file (λ (out . _) (write-string string out))))

(define (assoc1 key alist)
  (let ((pair (assoc key alist)))
    (cond ((not pair)
           (error "Not found:" key alist))
          ((not (and (list? pair) (= 2 (length pair))))
           (error "Bad pair:" key))
          (else
           (cadr pair)))))

(define (assoc* key alist)
  (filter (lambda (pair)
            (and (list? pair) (equal? key (car pair))))
          alist))

(string->file
 (srl:sxml->html
  `(html
    (head
     (title "Lisp")
     (style ,(css-expr->css
              (css-expr
               (html #:background-color white #:font-family sans-serif)
               (table #:border-collapse collapse)
               (table (td th
                          #:border (1px solid black)
                          #:padding 3px))))))
    (body
     (h1 "Symbols")
     ,@(append-map
        (lambda (form)
          (cond ((and (list? form) (equal? 'enumeration (car form)))
                 `((h2 ,(assoc1 'title (cdr form)))
                   (table
                    (tr (th "Symbol")
                        (th "Title"))
                    ,@(map (lambda (value)
                             `(tr (td (code
                                       ,(symbol->string
                                         (assoc1 'symbol (cdr value)))))
                                  (td ,(assoc1 'title (cdr value)))))
                           (assoc* 'value (cdr form))))))
                (else '())))
        (with-input-from-file "symbols.lisp" read-all)))))
 "symbols.html")
