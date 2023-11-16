#lang racket

(provide build)

(require string-interpolation)

(require "../hcl/parser.rkt")
(require "../hcl/query.rkt")

(define (build)
  (validate)
  (if (directory-exists? (rf-path))
      (clean-previous-run)
      (claim-terraform-files))
  (let* ([tf (parse-directory/tf (current-directory))])
    (copy-tf-files tf)
    (copy-referenced-files tf))
  (system "terraform -chdir=@{(rf-path)} init"))

(define (validate)
  (when (and (directory-exists? (rf-path))
             (directory-exists? ".terraform"))
    (error "Conflicting state, directory controlled by both Terraform and Racketform")))

(define (clean-previous-run)
  (for ([file (remove* (map string->path '(".terraform" ".terraform.lock.hcl"))
                       (directory-list (rf-path)))])
    (println "Removing @{file}")
    (delete-directory/files (rf-path file))))

(define (claim-terraform-files)
  (make-directory (rf-path))
  (when (directory-exists? ".terraform") (rename-file-or-directory ".terraform" (rf-path ".terraform")))
  (when (file-exists? ".terraform.lock.hcl") (rename-file-or-directory ".terraform.lock.hcl" (rf-path ".terraform.lock.hcl"))))
  
(define (copy-tf-files tf)
  (for ([file (lookup/tf tf "metadata.files")])
    (copy-file file (rf-path file))))

(define (copy-referenced-files tf)
  (for ([file (referenced-files/tf tf)])
    (println "Copying referenced: @{file}")
    (unless (file-exists? file)
      (copy-directory/files file (rf-path file)))))

(define (rf-path [path null])
  (if (null? path)
      ".racketform"
      (build-path ".racketform" path)))

(module+ test
  (require racket/file)
  (require racket/function)
  (require rackunit)

  (require "../hcl/generator.rkt")

  (current-directory (make-temporary-directory))
  (display-to-file "" "file.tf")

  (make-directory "referenced")
  (display-to-file (generate/tf '([data "archive_file" "test"
                                        ([type "zip"]
                                         [source_dir "referenced"]
                                         [output_path "referenced.zip"])]))
                   "archive-reference.tf" #:exists 'replace)
  (display-to-file (generate/tf '([data "local_file" "self" ([filename "circular-reference.tf"])]))
                   "circular-reference.tf")
  
  (build)

  (check-pred directory-exists? (rf-path ".terraform"))
  (check-pred file-exists? "file.tf")
  (check-pred file-exists? (rf-path "file.tf"))
  (check-pred directory-exists? (rf-path "referenced"))
  (check-pred file-exists? (rf-path "circular-reference.tf"))

  (clean-previous-run)
  (check-pred (negate file-exists?) (rf-path "file.tf"))
  (check-pred directory-exists? (rf-path ".terraform"))
  (check-pred file-exists? (rf-path ".terraform.lock.hcl"))

  (rename-file-or-directory (rf-path ".terraform") ".terraform")
  (rename-file-or-directory (rf-path ".terraform.lock.hcl") ".terraform.lock.hcl")
  (delete-directory/files (rf-path))
  (claim-terraform-files)
  (check-pred directory-exists? (rf-path ".terraform"))
  (check-pred (negate directory-exists?) ".terraform")
  (check-pred file-exists? (rf-path ".terraform.lock.hcl"))
  (check-pred (negate file-exists?) ".terraform.lock.hcl"))