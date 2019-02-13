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
    ;; initialize state of solver
    (let ([start_state (state-from-string-rep puzzle)] ;; start_state = st.from_string_rep(string_rep) // line 33
        [_current (cons '() start_state)] ;; self._current = [([], start_state)] line 34
        [_next '()] ;; self._next = [] // line 35
        [_seen make-eqv-hashtable] ;; self._seen = {start_state} // line 36
        (hashtable-set! ([_seen 'start_state start_state]))
        (hashtable-entries _seen)) ;; check hashtable entries
      (let (run (lambda)) ;; def run(self): // line 38
        (let ([seq (car _current)]) ;; for seq in self._current: // line 41
          ;; no recursion // fix
          (let ([state (cdr seq)]) ;; state = path[1] // line 57
            (let-values ([(next_state move) (state)]) ;; for (next_state, move) in self._moves(state): // line 58
              ;; no recursion // fix
              ;; _move method
              (if (state-is-solved? next_state)
               (+ (car seq) (hashtable-ref _seen move ))) ;; return path[0] + [move] // line 60
              (if (hashtable-contains? _seen next_state) ;; if next_state not in self._seen: // line 61
                (apply append (+ (car seq) (hashtable-ref _seen move )) (next_state));; self._next.append((path[0] + [move], next_state)) // line 62
                (hashtable-set! _seen 'next_state next_state)))) ;; self._seen.add(next_state) // line 63
          (if (_next) ;; if self._next: // line 45
            (map _current _next) ;; self._current = self._next // line 46
            (_next '()) ;; self._next = [] // line 47
            else #f)) ;; else: return None // lines 48 & 49
          run )) ;; call recursive funtion
    ))

;; NOT ALLOWED TO USE "set!"" but you can use "hashtable-set!" and "let", "let*", and "letrec".
;; NOT ALLOWED TO USE "define" nested inside function definitions

;; [_[]_[]_current_level_[]_[]_]
;;   |  | map neighbours to "current level"
;;   V  V
;; [[_]_[_]_[_]_[_]_]
;;        | apply append
;;        V
;; [_____________]
;;     filter
;; [__next_level__]
