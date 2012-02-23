#lang racket/base

(require racket/match)
(require racket/class)
(require racket/draw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax clamp-channel
  (syntax-rules ()
    ((_ high-mask clamped-mask channel-keep channel-discard pixel)
     (match (bitwise-and pixel high-mask)
       [0            (bitwise-ior (bitwise-and pixel channel-discard) clamped-mask)]
       [clamped-mask pixel]
       [_            (bitwise-ior (bitwise-and pixel channel-discard) channel-keep)]))))

(define-syntax-rule (clamp-pixel pixel)
  ;; Clamp required because JPG occasionally sends a delta too
  ;; high or too low, leaving us with out-of-range pixels.
  ;; Clamp each channel to [40, 7F].
  (- (if (= (bitwise-and pixel #xc0c0c0) #x404040)
	 pixel
	 (let* ((pixel (clamp-channel #xc00000 #x400000 #x7f0000 #x00ffff pixel))
		(pixel (clamp-channel #x00c000 #x004000 #x007f00 #xff00ff pixel))
		(pixel (clamp-channel #x0000c0 #x000040 #x00007f #xffff00 pixel)))
	   pixel))
     #x404040))

(define (kernel-encode old-pixel new-pixel)
  (+ (reduce-precision new-pixel)
     (negate-pixel (reduce-precision old-pixel))
     #x404040))

(define-syntax-rule (kernel-decode base-pixel delta-pixel)
  (let ((clampable (- delta-pixel (negate-pixel (reduce-precision base-pixel)))))
    (bitwise-and #xffffff (arithmetic-shift (clamp-pixel clampable) 2))))

(define (reduce-precision pixel)
  (bitwise-and (arithmetic-shift pixel -2) #x3f3f3f))

(define (negate-pixel pixel)
  (+ #x010101 (bitwise-xor pixel #x3f3f3f)))

(define w 160)
(define h 120)

(define (integrate-delta base-pixels delta-pixels)
  (if (not base-pixels)
      #f
      (begin
	(for* ([y (in-range h)] [x (in-range w)])
	  (define i (* 4 (+ x (* y w))))
	  (integer->integer-bytes
	   (kernel-decode (integer-bytes->integer base-pixels #f #t i (+ i 4))
			  (integer-bytes->integer delta-pixels #f #t i (+ i 4)))
	   4 #f #t base-pixels i))
	base-pixels)))

(define (jpeg-bytes->pixels bs)
  (define bm (make-object bitmap% (open-input-bytes bs) 'jpeg))
  (when (or (not (= w (send bm get-width)))
	    (not (= h (send bm get-height))))
    (error 'wrong-dimensions "Wrong dimensions: ~v"
	   (list w h (send bm get-width) (send bm get-height))))
  (define pixels (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h pixels)
  pixels)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(write 'decompressing) (newline) (flush-output)
(define frames0
  (time
   (map (lambda (f) (cons (car f) (jpeg-bytes->pixels (cdr f))))
	(read))))
(write 'decoding) (newline) (flush-output)
(time
 (let loop ((frames frames0) (frame #f))
   (if (null? frames)
       'done
       (match (car frames)
	 [`(i . ,jpeg)
	  (loop (cdr frames) jpeg)]
	 [`(p . ,jpeg)
	  (loop (cdr frames) (integrate-delta frame jpeg))]))))
