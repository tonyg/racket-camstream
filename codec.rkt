#lang typed/racket/base

(require racket/match)

(provide kernel-encode kernel-decode integrate-argb!)

(define-syntax clamp-channel
  (syntax-rules ()
    ((_ high-mask clamped-mask channel-keep channel-discard pixel)
     (let ((v (bitwise-and pixel high-mask)))
       (cond
         [(= v 0) (bitwise-ior (bitwise-and pixel channel-discard) clamped-mask)]
         [(= v clamped-mask) pixel]
         [else (bitwise-ior (bitwise-and pixel channel-discard) channel-keep)])))))

(: clamp-pixel (Index -> Fixnum))
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

(: kernel-encode (Index Index -> Index))
(define (kernel-encode old-pixel new-pixel)
  (assert
   (+ (reduce-precision new-pixel)
      (negate-pixel (reduce-precision old-pixel))
      #x404040)
   index?))

(: kernel-decode (Index Index -> Index))
(define (kernel-decode base-pixel delta-pixel)
  (bitwise-and #xffffff
	       (arithmetic-shift (clamp-pixel (assert (- delta-pixel
                                                         (negate-pixel (reduce-precision base-pixel)))
                                                      index?))
				 2)))

(: reduce-precision (Fixnum -> Index))
(define (reduce-precision pixel)
  (bitwise-and (arithmetic-shift pixel -2) #x3f3f3f))

(: negate-pixel (Index -> Fixnum))
(define (negate-pixel pixel)
  (+ #x010101 (bitwise-xor pixel #x3f3f3f)))

;; Updates base-pixels in place
(: integrate-argb! (Integer Integer Bytes Bytes -> Void))
(define (integrate-argb! w h base-pixels delta-pixels)
  (for*: ([y : Exact-Nonnegative-Integer h]
	  [x : Exact-Nonnegative-Integer w])
    (define: i : Integer (* 4 (+ x (* y w))))
    (void
     (integer->integer-bytes
      (kernel-decode (assert (integer-bytes->integer base-pixels #f #t i (+ i 4)) index?)
		     (assert (integer-bytes->integer delta-pixels #f #t i (+ i 4)) index?))
      4 #f #t base-pixels i))))
