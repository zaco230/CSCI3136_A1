;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; rush-hour/solver.ss
;;;
;;; Rush Hour puzzle solver
;;; (C) 2019 Norbert Zeh (nzeh@cs.dal.ca)
;;;
;;; Implementation of the search for a shortest move sequence that solves a
;;; given Rush Hour puzzle.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOT ALLOWED TO USE set! but you can use hashtable-set!, let, let*, and letrec
;; NOT ALLOWED TO USE define nested inside function definitions

;; [_[]_[]_current_level_[]_[]_]
;;   |  | map neighbours to "current level"
;;   V  V
;; [[_]_[_]_[_]_[_]_]
;;        | apply append
;;        V
;; [_____________]
;;     filter
;; [__next_level__]

(library (rush-hour solver)
  (export solve-puzzle)
  (import (rnrs (6))
          (rush-hour state)
          (rush-hour utils))

  ;; Solve a Rush Hour puzzle represented as a 36-character string listing the
  ;; 6x6 cells of the board in row-major order.  Empty cells are marked with
  ;; "o".  Occupied cells are marked  with letters representing pieces.  Cells
  ;; occupied by the same piece carry the same letter.

(define (solve-puzzle puzzle)
	; Initialize the state of the solver
	(let* ([start_state (state-from-string-rep puzzle)]
		   [_current (list (list start_state ))]
		   [_seen (make-eq-hashtable)])
		(hashtable-set! _seen start_state #t)
		(run _seen _current)))

; Run the solver from the start state
(define (run _seen seq) 
	(let ([sol (_search seq)])
		(if sol
			(begin (cdr sol))
			(begin (run _seen (new_state _seen seq))))))

; See whether the given path (sequence of moves) can be extended with
; a single move to obtain a solution.  If so, return this solution.
; Otherwise, return false
(define (_search path)
	(if (null? path)
		#f
		(let ([next_state (car path)]
			  [state (cdr path)])	
			(if (state-is-solved? (car next_state))
				(next_state) (_search state)))))

; map neighbours to 'current level'
(define (new_state _seen next_state) 
	(begin (map (lambda (state) (hashtable-set! _seen (car state) #t)) next_state)
		(filter (lambda (state) (hashtable-contains? _seen (car state)))
			(apply append (map _moves next_state)))))

; Generate all valid moves from the given state.
(define (_moves state)
	(filter (lambda (x) x) 
		(apply append 
			(map (lambda (pos) ;from each end position
			(if (state-is-horizontal? (car state) pos)
				(map (lambda (k) ; move horizontally
						(if (state-horizontal-move (car state) pos k)							
							(list (state-horizontal-move (car state) pos k) (list (state-make-move pos k) (cdr state)))
							#f))
					(list -4 -3 -2 -1 1 2 3 4))
				(map (lambda (k) ; move vertically
						(if (state-vertical-move (car state) pos k)						
							(list (state-vertical-move (car state) pos k) (list (state-make-move pos k) (cdr state)))
							#f))
					(list -4 -3 -2 -1 1 2 3 4))))
				(filter (lambda (pos)
						(state-is-end? (car state) pos))
	(list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63))))))

)
