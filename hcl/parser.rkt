#lang racket

(require racket/hash)
(require megaparsack megaparsack/text)
(require data/monad data/applicative)

(provide parse/tf parse-file/tf parse-directory/tf)

(define (parse-directory/tf path)
  (apply parse-file/tf (filter terraform-file? (directory-list path))))

(define (terraform-file? file)
  (and (filename-extension file)
       (bytes=? (filename-extension file) #"tf")))

(define (parse-file/tf . paths)
  (apply hash-union #:combine hash-union
         (hash "metadata" (hash "files" paths))
         (map (lambda (file)
                (parse/tf (file->string file) file))
              paths)))

(define (parse/tf code [file "string"])
  (parse-result! (parse-string terraform/p code file)))

(define whitespace/p (many/p space/p))

(define identifier/p
  (do [ident <- (many/p (or/p letter/p digit/p (char-in/p "_-.")))]
    (pure (apply string ident))))

(define tf-string/p
  (do (char/p #\")
    [chars <- (many/p (char-not-in/p "\""))]
    (char/p #\")
    (pure (apply string chars))))

(define value/p
  (or/p tf-string/p
        (do [i <- identifier/p]
          (pure (list 'reference i)))))

(define argument/p
  (do [identifier <- identifier/p]
    whitespace/p
    (char/p #\=)
    whitespace/p
    [value <- value/p]
    (pure (hash identifier value))))

(define block-body/p
  (do (char/p #\{)
    whitespace/p
    [elements <- (many/p (do [element <- (or/p (try/p block/p) argument/p)]
                              whitespace/p
                              (pure element)))]
    (char/p #\})
    (pure (apply hash-union (hash) elements))))


(define (make-nested-hash keys value)
  (if (empty? keys)
      value
      (hash (car keys) (make-nested-hash (cdr keys) value))))

(define block/p
  (do [type <- identifier/p]
    whitespace/p
    [labels <- (many/p (do [label <- tf-string/p]
                         whitespace/p
                         (pure label)))]
    [body <- block-body/p]
    (pure (make-nested-hash (cons type labels) body))))

(define terraform/p
  (do whitespace/p
    [blocks <- (many/p (do [block <- block/p]
                         whitespace/p
                         (pure block)))]
    eof/p
    (pure (apply hash-union (hash) blocks #:combine hash-union))))

(module+ test
  (require rackunit)
  (require "generator.rkt")
  (require "query.rkt")

  (check-equal? (parse/tf "") (hash))
  (check-equal? (lookup/tf (parse/tf (generate/tf '([resource "foo" "bar" ()])))
                           "resource.foo.bar")
                (make-immutable-hash))
  (check-equal? (lookup/tf (parse/tf (generate/tf '([resource "foo" "bar"
                                                              ([baz "boo"])])))
                           "resource.foo.bar.baz")
                "boo")
  (let ([file (make-temporary-file "~a.tf")])
    (check-equal? (lookup/tf (parse-file/tf file)
                             "metadata.files")
                  (list file)))
  (check-equal? (referenced-files/tf (parse/tf (generate/tf '([data "local_file" "self"
                                                                    ([filename "main.tf"])]
                                                              [data "archive_file" "test"
                                                                    ([source_dir "copied"])]
                                                              []))))
                (set "main.tf" "copied"))
  (parse/tf (generate/tf '([terraform ([backend "http" ()])])))
  (check-equal? (lookup/tf (parse/tf (generate/tf '([resource "foo" "bar" ([baz foo.boo.baz])])))
                           "resource.foo.bar.baz")
                '(reference "foo.boo.baz")))
