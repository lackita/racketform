#lang racket

(provide generate/tf)

(require racket/contract)

(define (generate/tf hcl)
  (cond [(empty? hcl) ""]
        [(empty? (car hcl)) "\n"]
        [else (string-append (generate-block (car hcl)) "\n"
                             (generate/tf (cdr hcl)))]))

(struct block (type identifiers body))

(define (generate-block b)
  (block->string (list->block b)))

(define (list->block list)
  (block (car list)
         (takef (cdr list) (negate list?))
         (apply append (dropf list (negate list?)))))

(define (block->string block)
  (string-append
   (header->string block)
   " "
   (body->string block)))

(define (header->string block)
  (string-join (map identifier->string
                    (cons (block-type block)
                          (block-identifiers block)))
               " "))

(define (body->string block)
  (surround
   (list "{" (cond [(multi-line? block) "\n"]
                   [(= (length (block-body block)) 0) ""]
                   [else " "]))
   (string-join (map (if (multi-line? block) (prefix "  ") identity)
                     (map inner-line->string (block-body block)))
                "\n")))

(define (multi-line? block)
  (> (length (block-body block)) 1))

(define/contract (inner-line->string l)
  (list? . -> . string?)
  (if (and (list? (last l)))
      (generate-block l)
      (assignment->string l)))

(define (assignment->string assignment)
  (when (not (= (length assignment) 2))
    (raise-arguments-error 'assignment->string
                           "Assignments can only have two values"
                           "assignment" assignment))
  (string-append (identifier->string (car assignment))
                 " = "
                 (if (list? (last assignment))
                     (function->string (last assignment))
                     (identifier->string (last assignment)))))

(define/contract (function->string function)
  (list? . -> . string?)
  (when (not (= (length function) 2)) (error "Functions can only have two values"))
  (string-append (symbol->string (car function))
                 (surround "(" (body->string (block null null (last function))))))

(define (identifier->string value)
  (if (string? value)
      (string-append "\"" value "\"")
      (symbol->string value)))

(define ((prefix p) l)
  (string-append p l))

(define (surround s [l null])
  (cond [(null? l) (curry surround s)]
        [(list? s) (foldr surround l s)]
        [else (string-append s l
                             (cond [(eq? s "{") "}"]
                                   [(eq? s "(") ")"]
                                   [(eq? s "[") "]"]
                                   [else s]))]))

(module+ test
  (require rackunit)
  (check-equal? (generate/tf '([resource "foo" "bar"
                                         ([baz "boo"])]))
                "resource \"foo\" \"bar\" { baz = \"boo\" }\n")
  (check-equal? (generate/tf '([resource "foo" "bar"
                                         ([a "b"]
                                          [c "d"])]))
                #<<MULTILINE
resource "foo" "bar" {
  a = "b"
  c = "d"
}

MULTILINE
                )
  (check-equal? (generate/tf '([])) "\n")
  (check-equal? (generate/tf '([resource "foo" "bar" ()]
                               [resource "foo" "baz" ()]))
                #<<MULTILINE
resource "foo" "bar" {}
resource "foo" "baz" {}

MULTILINE
                )
  (check-equal? (generate/tf '([terraform ([backend "http" ()])]))
                "terraform { backend \"http\" {} }\n")
  (check-equal? (generate/tf '([resource "foo" "bar" ([baz (boo ([a "b"]))])]))
                "resource \"foo\" \"bar\" { baz = boo({ a = \"b\" }) }\n"))