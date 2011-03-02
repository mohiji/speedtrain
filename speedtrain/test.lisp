(in-package :speedtrain)

(defparameter *test-cookies*
  (alist-hash-table '(("chocolate" . "Yes, please")
                      ("choreos" . "Goodness no."))))

(defun generate-response (request)
  (let ((headers nil)
        (cookies nil)
        (params nil))
    (maphash (lambda (key value)
               (push (list :name key :value value) headers)) (request-headers request))
    (maphash (lambda (key value)
               (push (list :name key :value value) cookies)) (request-cookies request))
    (maphash (lambda (key value)
               (push (list :name key :value value) params)) (request-query request))
    (with-output-to-string (s)
      (html-template:fill-and-print-template #P"/Users/jfischer/Dropbox/Lisp/Projects/speedtrain/test.html"
                                             (list :path (request-path request)
                                                   :query params
                                                   :body (request-body request)
                                                   :headers headers
                                                   :cookies cookies)
                                             :stream s))))
    
(defun request-loop (conn)
  (do () ()
      (let ((msg (receive conn)))
        (send conn
              (message-id msg)
              (make-http-response 200 (generate-response msg) :cookies *test-cookies*)))))

(defmethod run-threaded (conn)
  (ccl:process-run-function "mongrel2-acceptor" #'request-loop conn))

(defun test-connection ()
  (make-instance 'mongrel2-connection
                 :recv-addr "tcp://127.0.0.1:9999"
                 :sub-addr "tcp://127.0.0.1:9998"
                 :sender-id "9308B4E5-DB6B-4461-91D4-3E0AB9E85552"))

(defun do-n-requests (conn n)
  (dotimes (i n)
    (format t "Waiting for a request.~%")
    (let ((msg (receive conn)))
      (format t "Received message: ~a~%" (message-headers msg))
      (let* ((request  (make-request msg))
             (response (generate-response request)))
        (format t "Answering request from uuid ~a, id ~a~%" (message-uuid msg) (message-id msg))
        (send conn
              (message-id msg)
              (make-http-response 200 response :cookies *test-cookies*)))
      msg)))
