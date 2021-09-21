#lang racket
(require "reqs.rkt")

(provide (all-defined-out))

(define (basic-pat-rec stx state ops)
  (define (operators-for s) (filter ops (λ (op) (operator-identifies? op s))))
  (let rec ([stx stx] [state state])
    (define (do-operator op v state)
      (if (operator-identifies? op v)
          (operator-parse-syntax op v state)
          (values v state)))
    (foldl/state do-operator stx state ops)))

;; builds a cmplr:node:case stx object by using pat-builder and body-builder values
;;  also runs the genral node-syntax-operator at the end on the built case with cmplr:node:state
;;  algorithm keeps a cmplr:node:state around which gets updated by both of these functions
;; pat-builder: pat-spec cmplr-node-state -> pat-stx cmplr-node-state
;; body-builder: body-spec cmplr-node-state -> body-stx
;; (struct basic-node-builder [pat-builder body-builder]
;;   #:methods gen:cmplr-node-builder
;;   [(define (build-cmplr-node bnb stxc cspec gspec nspec)
;;      (match-define (basic-node-builder patb bodyb) bnb)
;;      (match-define (cmplr header groups info) cspec)
;;      (match-define (cmplr:header cid cargs ctyp) header)
;;      (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
;;      (match-define (cmplr:type gfrom gto) gtyp)
;;      (match-define (cmplr:node pats bodys) nspec)
;;      (define initial-state (cmplr:state:node cspec gspec nspec '()))
;;      (define-values (pat-stx pat-state) (patb pats initial-state))
;;      (define body-stx (bodyb bodys pat-state))

;;      (define all-operators (flatten (info-values 'operators info)))
;;      (foldr cmplr-operator-node-syntax
;;             (cmplr:node:case pat-stx body-stx)
;;             (filter cmplr-node-operator? all-operators)))])

;; (struct basic-group-builder [def-builder body-builder]
;;   #:methods gen:cmplr-group-builder
;;   [(define (build-cmplr-group bgb stxc cspec gspec)
;;      (match-define (basic-group-builder def-builder body-builder) bgb)
;;      (match-define (cmplr header groups info) cspec)
;;      (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
;;      (match-define (cmplr:group gid gtyp gnodes ginfo) gspec)
;;      (define group-input #`group-input)
;;      (def-builder gid (cons group-input (map car cmplr-args)) (body-builder group-input stxc))
;;      ;; #`(define (#,gid #,group-input #,@(map car cmplr-args))
;;      ;;     #,(body-builder group-input stxc)
;;      ;;     ;; (#,match-stx group-inp #,@stxc)
;;      ;;     )
;;      )])

;; (struct basic-top-builder []
;;   #:methods gen:cmplr-top-builder
;;   [(define (build-cmplr-top btb stxc cspec)
;;      (match-define (cmplr header groups info) cspec)
;;      (match-define (cmplr:header cmplr-id cmplr-args cmplr-type) header)
;;      (match-define (cmplr:type cfrom cto) cmplr-type)
;;      #`(define (#,cmplr-id cmplr-inp #,@(map list (map car cmplr-args) (map cdr cmplr-args)))
;;          #,@stxc
;;          (#,(cmplr:group-id (car groups)) cmplr-inp #,@(map car cmplr-args))))])
