
; This file is part of the sr-convert sample rate conversion utility
; Copyright (c) 2005 by Edward Kiser

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(define for (lambda (start end proc)
    (let loop ((i start))
      (if (= i end) #t
        (begin
          (proc i)
          (loop (+ i 1)))))))

(define frand (let* (
      (m (string->number "FFFFFFFB" 16))
      (a (string->number "9E3779B1" 16))
      (c (string->number "61C88639" 16))
      (advance (lambda (x) (modulo (+ (* a x) c) m)))
      (state 0)
      (krand (lambda () (set! state (advance state)) state))
      (krandn (lambda (n) (floor (/ (* n (krand)) m))))
      (kseed (lambda (s) (set! state s)))
      (frand (lambda () (exact->inexact (/ (krand) m)))))
    (vector krand krandn kseed frand)))

(define krand (vector-ref frand 0))
(define krandn (vector-ref frand 1))
(define kseed (vector-ref frand 2))
(set! frand (vector-ref frand 3))

(define with-word-handler
  (lambda (line-length proc)
    (let* (
        (buffer (make-string 32768 #\space))
        (buf-end 0)
        (chars-on-line 0)
        (add-char (lambda (ch)
            (string-set! buffer buf-end ch)
            (set! buf-end (+ buf-end 1))
            (if (eq? ch #\newline)
              (set! chars-on-line 0)
              (set! chars-on-line (+ 1 chars-on-line)))))
        (add-string (lambda (str)
            (for 0 (string-length str) (lambda (i)
                (add-char (string-ref str i))))))
        (at-begin #t)
        (prespace (lambda (word-length)
            (cond
              (at-begin
                (set! at-begin #f))
              ((> (+ chars-on-line word-length 1) line-length)
                (add-char #\newline))
              (else (add-char #\space)))))
        (add-word (lambda (word)
            (prespace (string-length word))
            (add-string word)))
        (add-newline (lambda () (add-char #\newline)))
        (retrieve (lambda () (substring buffer 0 buf-end))))
      (proc add-word add-newline retrieve))))

(define denom (lambda (x)
    (if (rational? x) (denominator x) 1)))

(define fgcd (lambda (a b)
    (let* (
        (bigdenom (lcm (denom a) (denom b))))
      (/ (gcd (* a bigdenom) (* b bigdenom)) bigdenom))))

(define flcm (lambda (a b) (* a (/ b (fgcd a b)))))

(define c-number (lambda (value)
    (number->string (round value) 10)))

(define make-c-int-list (lambda (name values)
    (with-word-handler 118 (lambda (add-word add-newline retrieve)
        (for-each add-word `("const" "int" ,name "[]" "=" "{"))
        (for-each
          (lambda (value)
            (add-word
              (string-append (c-number value) ",")))
          values)
        (for-each add-word `("-1" "};"))
        (add-newline)
        (retrieve)))))

(define make-c-constant (lambda (name value)
    (apply string-append `("const int " ,name " = "
        ,(c-number value) ";\n"))))

(define major-rates '(32000 64000 128000 44100 88200 176400 48000 96000 192000))

(define rates
  (let* (
      (fracs '(1/6 1/4 1/3 1/2 2/3 3/4 5/6 1))
      (fracs (append fracs
          (map (lambda (i) (+ 1 i)) fracs)
          (map (lambda (i) (+ 2 (* 2 i))) fracs))))
    (append
      '(11025/2 6000 6896)
      (map (lambda (i) (* 44100 i)) fracs)
      (map (lambda (i) (* 48000 i)) fracs)
      (map (lambda (i) (* 999/1000 i)) major-rates)
      (map (lambda (i) (* 1000/999 i)) major-rates))))

(define ratecount (length rates))

(define with-accumulator
  (lambda (proc)
    (let* (
        (lst (cons 'dummy '()))
        (cur lst)
        (add! (lambda (i)
            (set-cdr! cur (cons i '()))
            (set! cur (cdr cur))
            #t))
        (result (lambda () (cdr lst))))
      (proc add! result))))

(define with-priority-queue
  (lambda (maxsize gt? proc)
    (let* (
        (vec (make-vector maxsize #f))
        (size 0)
        (parent (lambda (x) (floor (/ (- x 1) 2))))
        (child1 (lambda (x) (- (* 2 (+ x 1)) 1)))
        (child2 (lambda (x) (* 2 (+ x 1))))
        (swap! (lambda (x y)
            (let* ((a (vector-ref vec x)))
              (vector-set! vec x (vector-ref vec y))
              (vector-set! vec y a) #t)))
        (has-no-children? (lambda (x) (>= (child1 x) size)))
        (has-no-second-child? (lambda (x) (>= (child2 x) size)))
        (ref (lambda (x) (vector-ref vec x)))
        (exceeds-parent? (lambda (x)
            (if (= x 0) #f
              (gt? (ref x) (ref (parent x))))))
        (first-child-exceeds? (lambda (x)
            (cond
              ((has-no-children? x) #f)
              ((has-no-second-child? x)
                (gt? (ref (child1 x)) (ref x)))
              (else
                (and (gt? (ref (child1 x)) (ref x))
                  (gt? (ref (child1 x)) (ref (child2 x))))))))
        (second-child-exceeds? (lambda (x)
            (if (has-no-second-child? x) #f
              (and (gt? (ref (child2 x)) (ref x))
                (not (gt? (ref (child1 x)) (ref (child2 x))))))))
        (updown (letrec (
              (upheap (lambda (x)
                  (if (exceeds-parent? x)
                    (begin 
                      (swap! x (parent x))
                      (upheap (parent x)))
                    #t)))
              (downheap (lambda (x)
                  (cond
                    ((first-child-exceeds? x)
                      (swap! x (child1 x))
                      (downheap (child1 x)))
                    ((second-child-exceeds? x)
                      (swap! x (child2 x))
                      (downheap (child2 x)))
                    (else #t)))))
            (cons upheap downheap)))
        (upheap (car updown))
        (downheap (cdr updown))
        (push! (lambda (x)
            (let* ((index size))
              (set! size (+ size 1))
              (vector-set! vec index x)
              (upheap index))))
        (pop! (lambda ()
            (let* (
                (result (ref 0)))
              (set! size (- size 1))
              (vector-set! vec 0 (ref size))
              (vector-set! vec size #f)
              (downheap 0)
              result)))
        (top (lambda () (vector-ref vec 0)))
        (sizeq (lambda () size))
        (->list (lambda ()
            (let* (
                (result '()))
              (let loop ()
                (if (= size 0) result
                  (begin
                    (set! result (cons (pop!) result))
                    (loop))))))))
      (proc push! pop! top sizeq ->list))))

(define invert-sense (lambda (proc)
    (lambda k (apply proc (reverse k)))))

(define make-greater-map (lambda (proc)
    (lambda k (apply > (map proc k)))))

(set! rates (with-priority-queue (+ ratecount 1) >
    (lambda (push! pop! top size ->list)
      (for-each
        (lambda (i) (push! i))
        rates)
      (->list))))

(define apparent-rates (with-priority-queue
    (+ ratecount 1)
    (make-greater-map car)
    (lambda (push! pop! top size ->list)
      (for-each
        (lambda (i)
          (if (= 0 (- i (floor i)))
            (push! `(,i " appears as itself"))
            (push! `(,(floor i) "+" ,(- i (floor i)) " appears as " ,(c-number i)))))
        rates)
      (->list))))

(define with-top-n (lambda (n gt? proc)
    (with-priority-queue
      (+ n 1)
      (invert-sense gt?)
      (lambda (push! pop! top size ->list)
        (let* (
            (mypush! (lambda (k)
                (push! k)
                (if (> (size) n) (pop!))
                #t)))
          (proc mypush! ->list))))))

(define irlength (lambda (skipout skipin)
    (+ 1 (* 500 (max skipout skipin)))))

(define top-skips '())

(define with-skips (lambda (inrate outrate proc)
    (let* (
        (thegcd (fgcd inrate outrate))
        (skipout (/ inrate thegcd))
        (skipin (/ outrate thegcd))
        (the-irlen (irlength skipout skipin)))
      (proc thegcd skipout skipin the-irlen))))

(define position-of (lambda (item things)
    (let loop (
        (index 0)
        (things things))
      (cond
        ((null? things) -)
        ((= item (car things)) index)
        (else (loop (+ index 1) (cdr things)))))))

(define ir-length-limit 2097152)

(define tableval (lambda (inrate outrate add!)
    (with-skips inrate outrate
      (lambda (gcd1 skipout1 skipin1 irlen1)
        (if (<= irlen1 ir-length-limit)
          (begin
            (add! `(,irlen1 "--" ,(c-number inrate)
                " to " ,(c-number outrate)))
            skipin1)
          (let* (
              (best-mid 0)
              (best-mid2 0)
              (best-score irlen1)
              (score (lambda (midrate)
                  (with-skips inrate midrate
                    (lambda (gcd-inmid skipout-inmid skipin-inmid irlen-inmid)
                      (with-skips midrate outrate
                        (lambda (gcd-midout skipout-midout skipin-midout irlen-midout)
                          (+ irlen-inmid irlen-midout)))))))
              (updatescore1 (lambda (midrate)
                  (let* (
                      (the-score (score midrate)))
                    (if (< the-score best-score)
                      (begin
                        (set! best-mid midrate)
                        (set! best-score the-score))))))
              (updatescore (lambda (midrate)
                  (if (> midrate (min inrate outrate))
                    (updatescore1 midrate))))
              (score2 (lambda (midrate1 midrate2)
                  (with-skips inrate midrate1
                    (lambda (gcd-inmid1 skipout-inmid1 skipin-inmid1 irlen-inmid1)
                      (with-skips midrate1 midrate2
                        (lambda (gcd-mid1mid2 skipout-mid1mid2 skipin-mid1mid2 irlen-mid1mid2)
                          (with-skips midrate2 outrate
                            (lambda (gcd-mid2out skipout-mid2out skipin-mid2out irlen-mid2out)
                              (+ irlen-inmid1 irlen-mid1mid2 irlen-mid2out)))))))))
              (updatescore12 (lambda (midrate1 midrate2)
                  (let* (
                      (the-score (score2 midrate1 midrate2)))
                    (if (< the-score best-score)
                      (begin
                        (set! best-mid midrate1)
                        (set! best-mid2 midrate2)
                        (set! best-score the-score))))))
              (updatescore2 (lambda (midrate1 midrate2)
                  (if (let* (
                        (minrate (min inrate outrate)))
                      (and (> midrate1 minrate) (> midrate2 minrate)))
                    (updatescore12 midrate1 midrate2)))))
            (for-each updatescore rates)
            (if (<= best-score ir-length-limit)
              (begin
                (add! `(,best-score "--" ,(c-number inrate)
                    " to " ,(c-number best-mid) " to " ,(c-number outrate)
                    " (was " ,(c-number irlen1) ")"))
                (- -1 (position-of best-mid rates)))
              (begin
                (set! best-score irlen1)
                (for-each (lambda (mid1)
                    (for-each (lambda (mid2)
                        (updatescore2 mid1 mid2))
                      rates))
                  rates)
                (add! `(,best-score "--" ,(c-number inrate)
                    " to " ,(c-number best-mid) " to " ,(c-number best-mid2)
                    " to " ,(c-number outrate) " (was " ,(c-number irlen1) ")"))
                (- -1 (+ (position-of best-mid rates)
                    (* ratecount (+ 1 (position-of best-mid2 rates)))))))))))))

(define skips
  (with-accumulator (lambda (add! result)
      (with-top-n (+ 1 (* ratecount ratecount)) (make-greater-map car)
        (lambda (top-add! top-list)
          (for-each (lambda (outrate)
              (for-each (lambda (inrate)
                  (add! (tableval inrate outrate top-add!)))
                rates))
            rates)
          (set! top-skips (top-list))))
      (result))))

(with-output-to-file "skips.out"
  (lambda ()
    (newline)
    (display (make-c-int-list "rates" rates))
    (newline)
    (display (make-c-constant "rate_count" (length rates)))
    (newline)
    (display (make-c-int-list "skips" skips))
    (newline)))

(define max-of-list (lambda (lst)
    (let loop (
        (maxsofar (car lst))
        (lst (cdr lst)))
      (if (null? lst) maxsofar
        (loop (max maxsofar (car lst)) (cdr lst))))))

(for-each display `("Maximum skip value: " ,(max-of-list skips) "\n"))

(with-output-to-file "topskips.txt"
  (lambda ()
    (display "Top IR Lengths:\n")
    (for-each (lambda (i)
        (for-each display i)
        (newline))
      top-skips)
    (newline)
    (display "Apparent Rates:\n")
    (for-each (lambda (i)
        (for-each display i)
        (newline))
      apparent-rates)))
