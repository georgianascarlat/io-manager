#lang racket/base
(require (for-syntax racket/base)
         racket/runtime-path)

(define-runtime-module-path-index pb 'lang/plt-pretty-big-text)
(define-runtime-path prog "simple-example.scm")

(namespace-require pb)
(load prog)
