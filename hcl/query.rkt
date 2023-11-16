#lang racket

(provide lookup/tf referenced-files/tf)

(define (referenced-files/tf tf)
  (set-union (lookup/tf tf "data.archive_file.*.source_dir")
             (lookup/tf tf "data.local_file.*.filename")))

(define (lookup/tf tf selector)
  (cond [(string? selector) (lookup/tf tf (string-split selector "."))]
        [(empty? selector) tf]
        [(string=? (car selector) "*") (list->set (map (lambda (v) (lookup/tf v (cdr selector)))
                                                       (hash-values tf)))]
        [else (lookup/tf (hash-ref tf (car selector) hash)
                         (cdr selector))]))
