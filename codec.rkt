#lang typed/racket/base

(require racket/match)

(provide kernel-encode kernel-decode integrate-argb!)

(: kernel-encode (Integer Integer -> Integer))
(define (kernel-encode old-pixel new-pixel)
  (+ (reduce-precision new-pixel)
     (negate-pixel (reduce-precision old-pixel))
     #x404040))

(: kernel-decode (Integer Integer -> Integer))
(define (kernel-decode base-pixel delta-pixel)
  (bitwise-and #xffffff
	       (arithmetic-shift (clamp-pixel (- delta-pixel
						 (negate-pixel (reduce-precision base-pixel))))
				 2)))

(: reduce-precision (Integer -> Integer))
(define (reduce-precision pixel)
  (bitwise-and (arithmetic-shift pixel -2) #x3f3f3f))

(: negate-pixel (Integer -> Integer))
(define (negate-pixel pixel)
  (+ #x010101 (bitwise-xor pixel #x3f3f3f)))

(define-syntax clamp-channel
  (syntax-rules ()
    ((_ high-mask clamped-mask channel-keep channel-discard pixel)
     (match (bitwise-and pixel high-mask)
       [0            (bitwise-ior (bitwise-and pixel channel-discard) clamped-mask)]
       [clamped-mask pixel]
       [_            (bitwise-ior (bitwise-and pixel channel-discard) channel-keep)]))))

(: clamp-pixel (Integer -> Integer))
(define (clamp-pixel pixel)
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

;; Updates base-pixels in place
(: integrate-argb! (Integer Integer Bytes Bytes -> Void))
(define (integrate-argb! w h base-pixels delta-pixels)
  (for*: ([y : Exact-Nonnegative-Integer (in-range h)]
	  [x : Exact-Nonnegative-Integer (in-range w)])
    (define: i : Integer (* 4 (+ x (* y w))))
    (void
     (integer->integer-bytes
      (kernel-decode (integer-bytes->integer base-pixels #f #t i (+ i 4))
		     (integer-bytes->integer delta-pixels #f #t i (+ i 4)))
      4 #f #t base-pixels i))))
