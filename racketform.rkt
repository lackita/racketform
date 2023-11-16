#lang racket

(require racket/cmdline)
(require "actions/all.rkt")

(command-line
 #:program "racketform"
 #:args (action)
 (cond [(string=? "build") (build)]
       [else (unknown-action action)]))
