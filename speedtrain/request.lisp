;; speedtrain/request.lisp

(in-package :speedtrain)

(defclass request ()
  ((path    :accessor request-path)
   (query   :accessor request-query)
   (body    :accessor request-body)
   (cookies :accessor request-cookies)
   (headers :accessor request-headers)))

;; Given the json text from a mongrel2 message headers field, return a
;; hash table containing all of the headers passed.
(defun make-header-table (header-json)
  (let ((headers  (make-hash-table :test 'equalp))
        (json-obj (read-json header-json)))
    (mapjso (lambda (key value)
              (setf (gethash key headers) value))
            json-obj)
    headers))

;; Parse a query string out into the proper key, values pairs.
;; Returns a hash table with those entries.
(defun make-query-table (str)
  (let ((q (make-hash-table :test 'equalp))
        (params (split-sequence #\& str)))
    (loop for item in params do
         (let ((fields (split-sequence #\= item)))
           (setf (gethash (first fields) q) (second fields))))
    q))

(defun make-request (message)
  ;; Take a mongrel2-message and turn it into an easy to use request
  ;; object.
  (let* ((request  (make-instance 'request))
         (headers  (make-header-table (message-headers message)))
         (querystr (gethash "query" headers "")))
    (setf (request-path request) (message-path message))
    (setf (request-query request) (make-query-table querystr))
    (setf (request-body request) (message-body message))
    (setf (request-cookies request) (read-cookies (gethash "cookie" headers)))
    (setf (request-headers request) headers)
    request))