#!/usr/bin/env gosh
;; -*- coding: utf-8; mode: scheme -*-
;;
;; tgosh.scm - テストケース生成を支援
;;
;;   Copyright (c) 2006 KOGURO, Naoki (naoki@koguro.net)
;;   All rights reserved.
;;
;;   Redistribution and use in source and binary forms, with or without 
;;   modification, are permitted provided that the following conditions 
;;   are met:
;;
;;   1. Redistributions of source code must retain the above copyright 
;;      notice, this list of conditions and the following disclaimer.
;;   2. Redistributions in binary form must reproduce the above copyright 
;;      notice, this list of conditions and the following disclaimer in the 
;;      documentation and/or other materials provided with the distribution.
;;   3. Neither the name of the authors nor the names of its contributors 
;;      may be used to endorse or promote products derived from this 
;;      software without specific prior written permission.
;;
;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;   $Id: tgosh.scm 335 2006-09-03 04:20:30Z naoki $
;;

(use util.match)
(use srfi-1)
(use srfi-13)
(use util.list)
(use gauche.vport)

(define-class <tee-port> (<virtual-input-port>)
  ((char-list :init-value '()
              :accessor char-list-of)))

(define-method initialize ((obj <tee-port>) . rest)
  (next-method)
  (set! (ref obj 'getc) (lambda ()
                          (let ((c (read-char (standard-input-port))))
                            (unless (eof-object? c)
                              (push! (char-list-of obj) c))
                            c))))

(define (get-string obj)
  (let ((lst (char-list-of obj)))
    (cond
     ((null? lst)
      #f)
     (else
      (list->string (reverse lst))))))

(define (make-test vec)
  (define (sexpr->string sexpr)
    (format "~s" sexpr))
  (match vec
    (#(sexpr (? undefined? v) stdout #f)
     (write/ss `(test* ,#`"case ',(sexpr->string sexpr)'" ,stdout
                       (with-output-to-string
                         (lambda ()
                           ,sexpr))))
     (newline))
    (#(sexpr (? undefined? v) stdout stdin)
     (write/ss `(test* ,#`"case ',(sexpr->string sexpr)'" ,stdout
                       (with-input-from-string ,stdin
                         (lambda ()
                           (with-output-to-string
                             (lambda ()
                               ,sexpr))))))
     (newline))
    (#(sexpr
       (? circular-list? v)
       (? (lambda (s) (eq? (string-length s) 0)) stdout)
       #f)
     (write/ss `(test* ,#`"case ',(sexpr->string sexpr)'" #t
                       (isomorphic? ,(list 'quote v) ,sexpr)))
     (newline))
    (#(sexpr
       (? circular-list? v)
       (? (lambda (s) (eq? (string-length s) 0)) stdout)
       stdin)
     (write/ss `(test* ,#`"case ',(sexpr->string sexpr)'" #t
                       (isomorphic? ,(list 'quote v) 
                                    (with-input-from-string ,stdin
                                      (lambda ()
                                        ,sexpr)))))
     (newline))
    (#(sexpr (? circular-list? v) stdout #f)
     (let ((rvar (gensym))
           (svar (gensym)))
     (write/ss `(test ,#`"case ',(sexpr->string sexpr)'" #t
                      (isomorphic? #(,v ,stdout)
                                   (lambda ()
                                     (let (,rvar ,svar)
                                       (set! ,svar (with-output-to-string
                                                     (lambda ()
                                                       (set! ,rvar ,sexpr))))
                                       (vector ,rvar ,svar))))))
     (newline)))
    (#(sexpr (? circular-list? v) stdout stdin)
     (let ((rvar (gensym))
           (svar (gensym)))
       (write/ss `(test* ,#`"case ',(sexpr->string sexpr)'" #t
                         (isomorphic? #(,v ,stdout)
                                      (with-input-from-string ,stdin
                                        (lambda ()
                                          (let (,rvar ,svar)
                                            (set! ,svar (with-output-to-string
                                                          (lambda ()
                                                            (set! ,rvar ,sexpr))))
                                            (vector ,rvar ,svar)))))))
       (newline)))
    (#(sexpr v (? (lambda (s) (eq? (string-length s) 0)) stdout) #f)
     (write/ss `(test* ,#`"case ',(sexpr->string sexpr)'" ,(cond
                                                            ((or (pair? v)
                                                                 (symbol? v))
                                                             (list 'quote v))
                                                            (else
                                                             v))
                    ,sexpr))
     (newline))
    (#(sexpr v (? (lambda (s) (eq? (string-length s) 0)) stdout) stdin)
     (write/ss `(test* ,#`"case ',(sexpr->string sexpr)'" ,(cond
                                                            ((or (pair? v)
                                                                 (symbol? v))
                                                             (list 'quote v))
                                                            (else
                                                             v))
                       (with-input-from-string ,stdin
                         (lambda ()
                           ,sexpr))))
     (newline))
    (#(sexpr v stdout #f)
     (let ((rvar (gensym))
           (svar (gensym)))
     (write/ss `(test ,#`"case ',(sexpr->string sexpr)'" #(,v ,stdout)
                      (lambda ()
                        (let (,rvar ,svar)
                          (set! ,svar (with-output-to-string
                                        (lambda ()
                                          (set! ,rvar ,sexpr))))
                          (vector ,rvar ,svar)))))
     (newline)))
    (#(sexpr v stdout stdin)
     (let ((rvar (gensym))
           (svar (gensym)))
       (write/ss `(test* ,#`"case ',(sexpr->string sexpr)'" #(,v ,stdout)
                         (with-input-from-string ,stdin
                           (lambda ()
                             (let (,rvar ,svar)
                               (set! ,svar (with-output-to-string
                                             (lambda ()
                                               (set! ,rvar ,sexpr))))
                               (vector ,rvar ,svar))))))
       (newline)))
    (else
     (error "Can't make test case"))))
     

(define (condition-type-name c)
  (cond
   ((<compound-condition> c)
    (string-join (map condition-type-name (condition-ref c '%conditions)) ","))
   (else
    (string-upcase (rxmatch-case
                       (symbol->string (class-name (class-of c)))
                     (#/<(.*)>/ (#f name) name)
                     (#/.*/ (str #f) str))))))

(define (show-history history num)
  (for-each (lambda (vec i)
              (when vec
                (format #t "input [~a]: ~s~%result[~a]: ~s~%stdin [~a]: ~a~%stdout[~a]: ~a~%~%"
                        i
                        (vector-ref vec 0)
                        i
                        (vector-ref vec 1)
                        i
                        (or (vector-ref vec 3) "")
                        i
                        (vector-ref vec 2))))
            (reverse (take* history num))
            (iota num (max 0 (- (length history) num)))))

(define (repl port)
  (let ((repl-history '())
        (module 'user)
        (n 0))
    (define (my-eval sexpr module)
      (let* ((result #f)
             (in (make <tee-port>))
             (stdout (with-output-to-string
                       (lambda ()
                         (with-input-from-port in
                           (lambda ()
                             (set! result (eval sexpr (find-module module)))))))))
        (display stdout)
        (format #t "=> ~a~%" (with-output-to-string
                               (lambda ()
                                 (write/ss result))))
        (push! repl-history (vector sexpr result stdout (get-string in)))))
    (port-for-each (lambda (sexpr)
                     (guard (e (else
                                (format (standard-error-port)
                                        "*** ~a: ~a~%"
                                        (condition-type-name e)
                                        (or (and-let* ((msg (condition-ref e 'message)))
                                              msg)
                                            ""))))
                            (read-char) ;; skip '\n'
                            (match sexpr
                              (('make-test)
                               (make-test (car repl-history))
                               (push! repl-history #f))
                              (('make-test (? number? x))
                               (make-test (list-ref repl-history (- n x 1)))
                               (push! repl-history #f))
                              (('history)
                               (show-history repl-history 20)
                               (push! repl-history #f))
                              (('history (? number? v))
                               (show-history repl-history v)
                               (push! repl-history #f))
                              (('select-module mod)
                               (if (find-module mod)
                                   (begin
                                     (set! module mod)
                                     (push! repl-history #f))
                                   (errorf "no such module: ~a" mod)))
                              (else
                               (my-eval sexpr module)))
                            (inc! n)))
                   (lambda ()
                     (format #t "~a:~a> " n module)
                     (flush (standard-output-port))
                     (read port)))))

(define (main args)
  (repl (standard-input-port))
  0)

;; end of file
