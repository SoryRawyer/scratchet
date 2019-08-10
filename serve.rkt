#lang racket

(require xml net/url racket/control)

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
  ; create a new custodian and create the network resources within it
  ; apparently you just need to map `current-custodian` to your new custodian
  ; to do this?
  ; I'm still not totally sure how that works. I'll leave that for later, I guess
  (define cust (make-custodian))
  ; limit the memory available to the custodian to 50MB
  (custodian-limit-memory cust (* 50 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread
     (lambda ()
       (handle in out)
       (close-input-port in)
       (close-output-port out))))
  ; implement a timeout after which we'll kill this thread
  ; via the built-in custodian cleanup
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  (define req
    ; match the first line to get the request
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ; discard the rest of the header
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ; dispatch
    (let ([xexpr (dispatch (list-ref req 1))])
      ; send reply
        (display "HTTP/1.0 200 Okay\r\n" out)
        (display "Server: k\r\nContent-type: text/html\r\n\r\n" out)
        (display (xexpr->string xexpr) out))))

; use dispatch-table to lookup a handler function for the url path
(define (dispatch str-path)
  ; parse the request as a url
  (define url (string->url str-path))
  ; get the path
  (define path (map path/param-path (url-path url)))
  ; find a handler in dispatch-table
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ; call a handler
      (h (url-query url))
      ; no handler!
      `(html (head (title "oops"))
             (body (font ((color "red"))
                         "Unknown page: "
                         ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "hellow, orld!"))))

; builds a page that has a form and takes the user to next-url
(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a number to add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"] [value ""]))
                (input ([type "hidden"] [name "hidden"] [value ,hidden]))
                (input ([type "submit"] [name "enter"] [value "Enter"]))))))

(define (many query)
  (build-request-page "Number of greetings:" "/reply" ""))

(define (reply query)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))

(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)
