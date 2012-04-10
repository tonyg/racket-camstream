#lang racket/base

(require racket/pretty)
(require racket/class)
(require racket/gui/base)
(require racket/draw)
(require racket/match)
(require (only-in web-server/private/gzip gunzip/bytes))

(require rnrs/bytevectors-6)

(require (planet tonyg/stomp))
(require (planet tonyg/bitsyntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define s (stomp-connect "rallyx.ccs.neu.edu" "guest" "guest" "/"))
(stomp-subscribe s "/exchange/plt/#" "s1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "codec.rkt")

(define (integrate-delta base0 delta)
  (define w (send delta get-width))
  (define h (send delta get-height))
  (define base (or base0 (let* ((b (make-object bitmap% w h))
				(dc (make-object bitmap-dc% b)))
			   (send dc set-brush "black" 'solid)
			   (send dc draw-rectangle 0 0 w h)
			   b)))
  (define base-pixels (make-bytes (* w h 4)))
  (define delta-pixels (make-bytes (* w h 4)))
  (send base get-argb-pixels 0 0 w h base-pixels)
  (send delta get-argb-pixels 0 0 w h delta-pixels)
  (integrate-argb! w h base-pixels delta-pixels)
  (define result (make-object bitmap% w h))
  (send result set-argb-pixels 0 0 w h base-pixels)
  result)

(define (jpeg-bytes->bitmap bs)
  (make-object bitmap% (open-input-bytes (bit-string->bytes bs)) 'jpeg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define video-window%
  (class* object% ()
    (init-field label)
    (super-new)

    (define frame (new frame% [label (string-append label " (racket-camstream)")]
		       [width 160] [height 120]))
    (define displayed-picture #f)
    (define canvas
      (new canvas%
	   [parent frame]
	   [paint-callback
	    (lambda (canvas dc)
	      (when displayed-picture
		(send dc draw-bitmap displayed-picture 0 0)))]))

    (define/public (set-picture new-picture)
      (set! displayed-picture new-picture)
      (send frame show #t)
      (send canvas refresh-now))

    (define/public (get-picture)
      displayed-picture)))

(define (read-main)
  (with-handlers ([exn:stomp?
		   (lambda (e)
		     (pretty-print (exn:stomp-frame e))
		     (raise e))]
		  [exn:break?
		   (lambda (e)
		     (stomp-unsubscribe s "s1")
		     (stomp-disconnect s)
		     (pretty-print 'clean-disconnect-complete))])
    (define windows (make-hash))
    (let loop ()
      (match-define (stomp-frame 'MESSAGE headers body) (stomp-next-message s "s1"))
      (define label (cond [(assq 'destination headers) => cadr] [else "???"]))
      (define window (hash-ref windows label
			       (lambda ()
				 (define w (make-object video-window% label))
				 (hash-set! windows label w)
				 w)))
      (match (cond [(assq 'content-type headers) => cadr] [else "application/octet-stream"])
	["video/x-camcapture"
	 (bit-string-case body
	   ([ (= 3 :: bits 8) (gzipped-frame :: binary) ]
	    (bit-string-case (gunzip/bytes (bit-string->bytes gzipped-frame))
	      ([ (frame-time-stamp :: bits 64) (= (char->integer #\I) :: bits 8) (jpeg :: binary) ]
	       (send window set-picture (jpeg-bytes->bitmap jpeg)))
	      ([ (frame-time-stamp :: bits 64) (= (char->integer #\P) :: bits 8) (jpeg :: binary) ]
	       (send window set-picture
		     (time (integrate-delta (send window get-picture)
					    (jpeg-bytes->bitmap jpeg))))))))]
	[content-type
	 (pretty-print `(message ,content-type))])
      (loop))))

(thread read-main)
(sleep 2)
