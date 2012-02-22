#lang racket/base

(require racket/pretty)
(require racket/class)
(require racket/gui/base)
(require racket/draw)
(require racket/match)
(require (only-in web-server/private/gzip gunzip/bytes))

(require (planet tonyg/stomp))
(require (planet tonyg/bitsyntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define s (stomp-connect "rallyx.ccs.neu.edu" "guest" "guest" "/"))
(stomp-subscribe s "/exchange/plt/#" "s1")

(with-handlers ([exn:break?
		 (lambda (e)
		   (stomp-unsubscribe s "s1")
		   (stomp-disconnect s)
		   (pretty-print 'clean-disconnect-complete))])
  (define windows (make-hash))
  (let loop ((frame-count 0)
	     (frames-rev '()))
    (if (< frame-count 600)
	(let ()
	  (match-define (stomp-frame 'MESSAGE headers body) (stomp-next-message s "s1"))
	  (match (cond [(assq 'content-type headers) => cadr] [else "application/octet-stream"])
	    ["video/x-camcapture"
	     (loop (+ frame-count 1)
		   (cons (bit-string-case body
			   ([ (= 3 :: bits 8) (gzipped-frame :: binary) ]
			    (bit-string-case (gunzip/bytes (bit-string->bytes gzipped-frame))
			      ([ (frame-time-stamp :: bits 64) (= (char->integer #\I) :: bits 8)
				 (jpeg :: binary) ]
			       (cons 'i (bit-string->bytes jpeg)))
			      ([ (frame-time-stamp :: bits 64) (= (char->integer #\P) :: bits 8)
				 (jpeg :: binary) ]
			       (cons 'p (bit-string->bytes jpeg))))))
			 frames-rev))]
	    [content-type
	     (loop frame-count frames-rev)]))
	(begin
	  (write (reverse frames-rev))
	  (newline)
	  (flush-output)))))
