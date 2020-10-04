(library-directories '("./thunderchez" "."))

(import (pfds hamts))

(include "hash.scm")

;;;; persistent records

;; api:
;;
;; (define-persistent-record foo
;;   (fields bar baz))
;;
;;;;; ====> functions
;;
;; constructor:
;; (make-foo <bar> <baz>)
;;
;; getter:
;; (foo-bar <foo>)
;; (foo-baz <foo>)
;;
;; update:
;; (foo-bar-set <foo> <bar>)
;; (foo-bar-update <foo> <f: bar -> bar>)

(define construct-name
  (lambda (template-identifier . args)
    (datum->syntax template-identifier
		   (string->symbol
		    (apply string-append
			   (map (lambda (x)
				  (if (string? x)
				      x
				      (symbol->string (syntax->datum x))))
				args))))))


(define-syntax gen-getter
  (lambda (stx)
    (syntax-case stx ()
      ((_ record-name field-name)
       (with-syntax ((getter (construct-name #'field-name
					     #'record-name
					     "-"
					     #'field-name)))
	 #'(define getter (lambda (obj) (hamt-ref obj 'field-name))))))))

(define-syntax gen-updater
  (lambda (stx)
    (syntax-case stx ()
      ((_ record-name field-name)
       (with-syntax ((updater (construct-name #'field-name
					      #'record-name
					      "-"
					      #'field-name
					      "-update")))
	 #'(define updater (lambda (obj f default)
			     (hamt-update obj 'field-name f default))))))))

(define-syntax gen-setter
  (lambda (stx)
    (syntax-case stx ()
      ((_ record-name field-name)
       (with-syntax ((setter (construct-name #'field-name
					     #'record-name
					     "-"
					     #'field-name
					     "-set")))
	 #'(define setter (lambda (obj val)
			    (hamt-set obj 'field-name val))))))))

(define-syntax define-persistent-record
  (lambda (stx)
    (syntax-case stx ()
      ((_ record-name (fields field-name ...))
       (with-syntax ((constructor-macro (construct-name #'record-name
							"make-"
							#'record-name))
		     (constructor-lambda (construct-name #'record-name
							 #'record-name
							 "-constructor")))
	 #'(begin
	     (define (constructor-lambda alist)
	       (let ((obj (make-hamt hash equal?)))
		 (fold-left (lambda (obj val)
			      (hamt-set obj (car val) (cadr val)))
			    obj
			    alist)))
	     (define-syntax constructor-macro
	       (syntax-rules ()
		 ((_ ((key val) (... ...)))
		  (constructor-lambda '((key val) (... ...))))))

	     (gen-getter record-name field-name) ...
	     (gen-setter record-name field-name) ...
	     (gen-updater record-name field-name) ...))))))


(define-persistent-record foo (fields bar baz))

(define f (make-foo ((baz 11) (bar 21))))

(foo-baz f)
(foo-bar f)

(foo-bar (foo-bar-update f (lambda (o) (+ 12 o)) 0))

(foo-bar f)

(foo-bar (foo-bar-set f 20))
