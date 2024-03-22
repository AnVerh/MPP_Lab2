#lang racket

(require math)
(require racket/string)

(define (sort-data data)
  (sort data <))

(define (fit-beta sorted-data)
  (define n (length sorted-data))
  (define sum-x (apply + sorted-data))
  (define sum-log-x (apply + (map log sorted-data)))
  
  (define alpha (/ (+ (* n sum-log-x) (* sum-x (- n 1) (log (last sorted-data))))
                   (- (* n (log (last sorted-data))) sum-log-x)))
  
  (define beta (/ (+ (* n (log (last sorted-data))) (- n 1) sum-log-x)
                  (- (* n (log (last sorted-data))) sum-log-x)))
  
  (define loc (first sorted-data))
  (define scale (- (last sorted-data) loc))
  
  (values (abs alpha) (abs beta) loc scale))


;;BETA-PARTITION FUNCTION
(define (beta-interval-partition data alphabet)
  (define alphabet-size (length alphabet))
  ;; Sort the numerical series
  (define sorted-data (sort-data data))
  
  ;; Determine the parameters of the beta distribution based on the data
  (define-values (alpha beta loc scale) (fit-beta sorted-data))
  (printf "Alpha: ")
  (display alpha)
  (newline)
  (printf "Beta: ")
  (display beta)
  (newline)
  
  ;; Compute the cumulative probability for each value
  (define cumulative-probabilities
    (map (λ (x) (cdf (beta-dist alpha beta) x)) sorted-data))
  (printf "Cumulative Probabilities:")
  (for-each (lambda (x) (display x) (display " ")) cumulative-probabilities)
  (newline)
  
  ;; Partition intervals according to cumulative probability and the selected alphabet
  (define interval-indices
  (map (λ (x) (inexact->exact (ceiling (real-part (* x alphabet-size))))) cumulative-probabilities))
  (printf "Interval indices:")
  (for-each (lambda (x) (display x) (display " ")) interval-indices)
  (newline)

  (define interval-dict
    (let loop ([indices interval-indices] [values sorted-data] [dict (hash)])
      (cond
        [(null? indices) dict]
        [else
         (let* ([idx (car indices)]
                [value (car values)]
                [rest-indices (cdr indices)]
                [rest-values (cdr values)]
                [updated (hash-update dict idx (λ (lst) (cons value lst)) (λ () null))])
             (loop rest-indices rest-values updated))])))

  (newline)

  ;; Assign symbols from the alphabet to interval numbers
  (define assigned-symbols (make-hash))
  (for ([key (in-hash-keys interval-dict)])
    ;(display key)
    ;(display (list-ref alphabet (- key 1)))
    ;(newline)
    (hash-set! assigned-symbols key (list-ref alphabet (- key 1))))
  
  ;; Output the result - replace interval numbers with alphabet letters
  (define result (make-hash))
  (for ([key (in-hash-keys interval-dict)])
  (define value (hash-ref interval-dict key))
  (hash-set! result (hash-ref assigned-symbols key) value))

  result)

;;
(define (print-dict hash-table)
  (hash-for-each
   hash-table
   (lambda (key value)
     (display key)
     (display ": ")
     (display value)
     (newline))))


(define data (sample (beta-dist 5 1) 10))
(newline)

(define alphabet-size 3)
(define alphabet (map integer->char (range 65 (+ 65 alphabet-size))))
;(for-each display alphabet)
(define intervals (beta-interval-partition data alphabet))
(for ([key (in-dict-keys intervals)]
      [values (in-dict-values intervals)])
  (displayln (format "Interval ~a: ~a\n" key values)))
;(print-hash-table intervals)
;(print-dict intervals)
;;;

(define letter-data (string-append))
(for ([number data])
  (for ([interval (in-hash-pairs intervals)])
    (define letter (car interval))
    (define numbers (cdr interval))
    (cond
      [(member number numbers)
       (set! letter-data (string-append letter-data (string letter)))]
    (else '()))))



(displayln "Initial data:")
(for ([num data])
  (display num)
  (display " "))
(newline)
(displayln letter-data)

(define ling-matrix (make-vector alphabet-size))
(for ([i (in-range alphabet-size)])
  (vector-set! ling-matrix i (make-vector alphabet-size)))
(for ([i (in-range alphabet-size)])
  (for ([j (in-range alphabet-size)])
    (define row-letter (list-ref alphabet i))
    (define col-letter (list-ref alphabet j))
    (define seeking-letters (string-append (string row-letter) (string col-letter)))
    (define appear 0)
    (for ([l (in-range (- (string-length letter-data) 1))])
      (if (string=? (substring letter-data l (+ l 2)) seeking-letters)
          (set! appear (+ appear 1))
          (void)))
    (vector-set! (vector-ref ling-matrix i) j appear)))

(displayln (vector*->matrix ling-matrix))
