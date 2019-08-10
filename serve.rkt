#lang racket

(require xml net/url)

(define (go)
 'yep-it-works)

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread
     (lambda ()
       (handle in out)
       (close-input-port in)
       (close-output-port out))))
  ; implement a timeout after which we'll kill this thread
  ; via a "watcher thread"
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  ; discard the request header (up to blank line)
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ; send reply
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-type: text/html\r\n\r\n" out)
  (display "<html><body>Hellow, orld!<body><html>" out))
