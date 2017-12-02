;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Contract-Utils: general-purpose PLT contract utilities.
;;  Copyright (C) 2005-6  Richard Cobbe
;;  Version 3.0
;;
;;  This library is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU Lesser General Public License as published by
;;  the Free Software Foundation; either version 2.1 of the License, or (at
;;  your option) any later version.
;;
;;  This library is distributed in the hope that it will be useful, but WITHOUT
;;  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;  License for more details.
;;
;;  You should have received a copy of the GNU Lesser General Public License
;;  along with this library; if not, write to the Free Software Foundation,
;;  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module contract-utils mzscheme

  (require (lib "contract.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "67.ss" "srfi"))

  ;; abstract types provided by contract.ss:
  ;;   Contract
  ;;   Flat-Contract

  ;; Pred-Contract ::= (Union (@a -> Bool) Contract)
  ;; Pred-Flat-Contract ::= (Union (@a -> Bool) Flat-Contract)

  ;; listof-unique/c :: (@a @a -> Bool) -> Flat-Contract
  ;; produces a flat contract that recognizes lists whose elements are unique
  ;; with respect to equ?
  ;; FIXME: take a contract that also applies to each element, like listof?
  (define listof-unique/c
    (lambda (equ?)
      (flat-named-contract "list of unique elements"
        (lambda (elems)
          (recur scan ([elems elems])
            (if (null? elems)
                #t
                (let* ([elem (car elems)]
                       [rest (cdr elems)])
                  (and (andmap (lambda (other) (not (equ? elem other))) rest)
                       (scan rest)))))))))

  ;; listof-unique-compare/c :: (@a @a -> (Union -1 0 1)) -> Flat-Contract
  ;; produces a flat contract that recognizes lists whose elements are unique
  ;; with respect to cmp.
  (define listof-unique-compare/c
    (lambda (cmp)
      (flat-named-contract "list of unique elements"
        (lambda (elems)
          (apply chain<? cmp (mergesort elems (<? cmp)))))))

  ;; nelistof/c :: Pred-Flat-Contract -> Flat-Contract
  ;; produces a contract that recognizes a non-empty list of elements
  ;; which satisfy the contract c.
  (define nelistof/c
    (lambda (c)
      (and/c (listof c) (not/c null?))))

  ;; sexp/c :: Flat-Contract
  ;; recognizes arbitrary s-expressions.
  (define sexp/c
    (flat-rec-contract sexp
                       (cons/c sexp sexp)
                       null?
                       number?
                       symbol?
                       string?
                       boolean?
                       char?))

  ;; predicate/c :: Contract
  ;; recognizes unary predicates
  (define predicate/c (any/c . -> . boolean?))

  ;; binary-predicate/c :: Contract
  ;; recognizes binary predicates
  (define binary-predicate/c (any/c any/c . -> . boolean?))

  ;; equality/c :: Contract
  ;; recognizes (binary) equality predicates
  (define equality/c (any/c any/c . -> . boolean?))

  ;; comparison/c :: Contract
  ;; recognizes comparison functions as used by SRFI 67
  (define comparison/c (any/c any/c . -> . (integer-in -1 1)))

  ;; hash-fn/c :: Contract
  ;; recognizes hash functions
  (define hash-fn/c (any/c . -> . integer?))

  ;; optional/c :: Pred-Contract -> Contract
  ;; produces a contract that recognizes both #f and all values recognized
  ;; by the argument
  (define optional/c (lambda (contract) (union contract false/c)))

  ;; positive-int/c :: Flat-Contract
  ;; recognizes all positive integers
  (define positive-int/c
    (and/c natural-number/c (lambda (x) (> x 0))))

  ;; contract/c :: Contract
  ;; recognizes contracts and predicates
  (define contract/c (union contract? predicate/c))

  ;; flat-contract/c :: Contract
  ;; recognizes flat contracts and predicates
  (define flat-contract/c (union flat-contract? predicate/c))

  ;; contract-of :: Pred-Contract -> Contract
  ;; wraps a predicate in a flat contract; idempotent
  (define contract-of
    (lambda (c/p)
      (if (contract? c/p) c/p (flat-contract c/p))))

  ;; predicate-of :: Pred-Flat-Contract -> Predicate
  ;; extracts a flat contract's predicate if necessary.  Idempotent.
  (define predicate-of
    (lambda (c/p)
      (if (flat-contract? c/p) (flat-contract-predicate c/p) c/p)))

  (define-syntax eta
    (syntax-rules ()
      [(_ f) (lambda args (apply f args))]))

  (provide/contract [sexp/c flat-contract?]
                    [predicate/c contract?]
                    [binary-predicate/c contract?]
                    [equality/c contract?]
                    [comparison/c contract?]
                    [hash-fn/c contract?]
                    [optional/c (contract/c . -> . contract?)]
                    [positive-int/c flat-contract?]
                    [listof-unique/c (equality/c . -> . flat-contract/c)]
                    [listof-unique-compare/c (comparison/c . -> .
                                                           flat-contract/c)]
                    [nelistof/c (contract/c . -> . flat-contract?)]
                    [contract/c contract?]
                    [flat-contract/c contract?]
                    [contract-of (contract/c . -> . contract?)]
                    [predicate-of (flat-contract/c . -> . predicate/c)])

  (provide eta))
