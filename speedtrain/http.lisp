;; speedtrain/http.lisp

;; This file defines some helper functions and structures for crafting HTTP responses
;; It doesn't cover HTTP requests, as Mongrel2 will be handling those.

(in-package :speedtrain)

;; Map of standard HTTP status codes to more friendly strings.  This
;; list was created by poking around http://httpstat.us/
(defparameter *http-status-codes*
  (alist-hash-table '((200 . "OK")
                      (201 . "Created")
                      (202 . "Accepted")
                      (203 . "Non-Authoritative Information")
                      (204 . "No Content")
                      (205 . "Reset Content")
                      (206 . "Partial Content")
                      (300 . "Multiple Choices")
                      (301 . "Moved Permanently")
                      (302 . "Found")
                      (303 . "See Other")
                      (304 . "Not Modified")
                      (305 . "Use Proxy")
                      (306 . "Unused")
                      (307 . "Temporary Redirect")
                      (400 . "Bad Request")
                      (401 . "Unauthorized")
                      (402 . "Payment Required")
                      (403 . "Forbidden")
                      (404 . "Not Found")
                      (405 . "Method Not Allowed")
                      (406 . "Not Acceptable")
                      (407 . "Proxy Authentication Required")
                      (408 . "Request Timeout")
                      (409 . "Conflict")
                      (410 . "Gone")
                      (411 . "Length Required")
                      (412 . "Precondition Required")
                      (413 . "Request Entity Too Large")
                      (414 . "Request-URI Too Long")
                      (415 . "Unsupported Media Type")
                      (416 . "Requested Range Not Satisfiable")
                      (417 . "Expectation Required")
                      (418 . "I'm a Teapot")
                      (500 . "Internal Server Error")
                      (501 . "Not Implemented")
                      (502 . "Bad Gateway")
                      (503 . "Service Unavailable")
                      (504 . "Gateway Timeout")
                      (505 . "HTTP Version Not Supported"))))

(defparameter *http-default-headers*
  (alist-hash-table '(("Content-Type" . "text/html"))))

(defun http-status-line (code)
  (format nil "HTTP/1.1 ~d ~a" code (gethash code *http-status-codes*)))

(defun http-end-line (stream)
  (princ #\Return stream)
  (princ #\Newline stream))

(defun make-http-response (code content &key (headers nil) (cookies nil))
  (cond ((listp headers)
         ; Caller provided headers as an alist, convert it to a hash
         ; table
         (setf headers (alist-hash-table headers :test 'equalp)))
        ((hash-table-p headers)
         ; Caller provided headers as a hash table; copy it so that we
         ; can safely modify it
         (setf headers (copy-hash-table headers :test 'equalp)))
        (t
         (setf headers (make-hash-table :test 'equalp))))
  (setf (gethash "Content-Length" headers) (length content))
  (with-output-to-string (s)
    (format s "~a" (http-status-line code))
    (http-end-line s)
    (maphash (lambda (key value)
               (format s "~a: ~a" key value)
               (http-end-line s)) headers)
    (when cookies
      (maphash (lambda (key value)
                 (format s "Set-cookie: ~a=~a" key value)
                 (http-end-line s)) cookies))
    (http-end-line s)
    (format s "~a" content)
    (http-end-line s)))
