;; This file counts on simply-scheme environment being loaded.
;; Copied from chapter 22, with modifications as I work throught the text.

;; A Scheme expression of the classic "Input Process Output" loop.

(define (file-map fn inname outname)
  (let ((inport (open-input-file inname))
        (outport (open-output-file outname)))
    (file-map-helper fn inport outport)
    (close-input-port inport)
    (close-output-port outport)
    'done))

(define (file-map-helper fn inport outport)
  (let ((line (read-line inport)))
    (if (eof-object? line)
      'done
      (begin (show-line (fn line) outport)
             (file-map-helper fn inport outport)))))

;; Justify a line of text, as in right justify to have even
;; left and right margins instead of ragged right.

(define (justify line width)
  (if (< (count line) 2)
    line
    (se (pad line
             (- (count line) 1)
             (extra-spaces width (char-count line))))))

(define (char-count line)
  (+ (accumulate + (every count line))      ; letters within words
     (- (count line) 1)))                   ; plus spaces between words

(define (extra-spaces width chars)
  (if (> chars width)
    0                                     ; none if already too wide
    (- width chars)))

(define (pad line chances needed)
  (if (= chances 0)                         ; only one word in line
    (first line)
    (let ((extra (quotient needed chances)))
      (word (first line)
            (spaces (+ extra 1))
            (pad (bf line) (- chances 1) (- needed extra))))))

(define (spaces n)
  (if (= n 0)
    ""
    (word " " (spaces (- n 1)))))

;; If you run this against a file (say the "r5rs" from the text) and then do
;; a 'print-file' the text will not appear to be changed. That's because
;; spaces are not significant to 'print-file'.

;; The following is a function that will preserve the spacing but as it
;; alters the standard environment's 'print-file-helper' I've given in a
;; different name. Do:
;;
;; (define print-file-helper string-print-file-helper)
;;
;; to use this.

(define (string-print-file-helper port)
  (let ((stuff (read-string port)))
    (if (eof-object? stuff)
      'done
      (begin (show stuff)
             (print-file-helper port)))))

;; Here's a prodedure to merge files instead of sentences from the
;; text.

(define (filemerge file1 file2 outfile)
  (let ((p1 (open-input-file file1))
        (p2 (open-input-file file2))
        (outp (open-output-file outfile)))
    (filemerge-helper p1 p2 outp (read-string p1) (read-string p2))
    (close-output-port outp)
    (close-input-port p1)
    (close-input-port p2)
    'done))

(define (filemerge-helper p1 p2 outp line1 line2)
  (cond ((eof-object? line1) (merge-copy line2 p2 outp))
        ((eof-object? line2) (merge-copy line1 p1 outp))
        ((before? line1 line2)
         (show line1 outp)
         (filemerge-helper p1 p2 outp (read-string p1) line2))
        (else (show line2 outp)
              (filemerge-helper p1 p2 outp line1 (read-string p2)))))

(define (merge-copy line inp outp)
  (if (eof-object? line)
    #f
    (begin (show line outp)
           (merge-copy (read-string inp) inp outp))))


