;; speedtrain/mongrel2.lisp

;; Mongrel2/ZeroMQ connector code.

(in-package :speedtrain)

(defvar *zmq-context* nil)
;; A helper class to hold on to Mongrel2 message values.
(defclass mongrel2-message ()
  ((uuid    :reader message-uuid    :initarg :uuid
            :documentation "The UUID of the Mongrel2 instance
            answering this request.")
   (id      :reader message-id      :initarg :id
            :documentation "The client ID making this particular
            request.")
   (path    :reader message-path    :initarg :path
            :documentation "The path portion of the URL requested,
            minus any query components.  E.g., given
            http://server/path/to/object?query=1, request-path will be
            /path/to/object")
   (headers :reader message-headers :initarg :headers
            :documentation "A json object containing the headers that
            were passed along with the request.")
   (body    :reader message-body    :initarg :body
            :documentation "Raw string containing the body of the
            request, if any.")))

;; Primary class for talking back and forth with Mongrel2
(defclass mongrel2-connection ()
  ((sender-id       :accessor sender-id :initarg :sender-id :initform (error "Must supply a sender-id"))
   (sub-addr        :accessor sub-addr  :initarg :sub-addr  :initform (error "Must supply a sub-addr"))
   (recv-addr       :accessor recv-addr :initarg :recv-addr :initform (error "Must supply a recv-addr"))
   (request-socket  :accessor request-socket)
   (response-socket :accessor response-socket)))

;; API for working with mongrel2-connections
(defgeneric receive (conn)
  (:documentation "Read a message from Mongrel2, returning a mongrel2-message object.  Blocks until a message is available."))

(defgeneric send (conn idents data)
  (:documentation "Send a message back to the clients identfied by 'idents'"))

(defgeneric close-connection (conn)
  (:documentation "Close down the ZeroMQ sockets that a connection holds open.  Can't rely on garbage collection for this."))

;; Set up connections to Mongrel2 when a connection object is created
(defmethod initialize-instance :after ((instance mongrel2-connection) &key)
  (unless *zmq-context*
    (setf *zmq-context* (zmq:init 1)))
  (with-slots (request-socket response-socket sender-id) instance
    (setf request-socket (zmq:socket *zmq-context* zmq:pull))
    (setf response-socket (zmq:socket *zmq-context* zmq:pub))
    (zmq:setsockopt response-socket zmq:identity sender-id)
    (zmq:connect request-socket (recv-addr instance))
    (zmq:connect response-socket (sub-addr instance))))

(defmethod receive ((conn mongrel2-connection))
  (let ((msg (make-instance 'zmq:msg)))
    (zmq:recv (request-socket conn) msg)
    (parse-message (zmq:msg-data-as-string msg))))

(defmethod send ((conn mongrel2-connection) idents data)
  (unless (listp idents)
    (setf idents (list idents)))
  (let* ((ident-string (string-right-trim " " (format nil "~{~a ~}" idents)))
         (content (format nil "~a ~a ~a" (sender-id conn) (make-netstring ident-string) data))
         (msg (make-instance 'zmq:msg :data content)))
    (zmq:send (response-socket conn) msg)))

(defmethod close-connection ((conn mongrel2-connection))
  (with-slots (request-socket response-socket) conn
    (zmq:close request-socket)
    (zmq:close response-socket)))

;; From the Mongrel 2 documentation:
;; "A netstring is a very simple way to encode a block of data such
;; that any language can read the block and know how big it is. A
;; netstring is, simply, SIZE:DATA,. So, to send “HI”, you would do
;; 2:HI,, and it is incredibly easy to parse in every language, even
;; C. It is also a fast format and you can read it even if you're a
;; human.

;; Take a string, "I am a string", and turn it into a netstring:
;; "13:I am a string,"
(defun make-netstring (s)
  (let ((len (length s)))
    (format nil "~a:~a," len s)))

;; Given an input that consists of 1 or more netstrings concatenated
;; togther, parse out the first netstring and return a list with that
;; string and the remainder of the input.
;;
;; E.g., given "10:One little,13:, two little,,24:three little netstrings.,",
;; return ("One little" "13:, two little,,24:three little netstrings.,")
(defun read-one-netstring (ns)
  (let* ((split  (position #\: ns))
         (slen   (subseq ns 0 split))
         (len    (parse-integer slen))
         (parsed (subseq ns (1+ split) (+ 2 split len)))
         (rest   (subseq ns (+ 2 split len))))
    (assert (eq #\, (elt parsed len)))
    (list (subseq parsed 0 len) rest)))

;; Using the above function, take an input that consists of 1 or more netstrings
;; concatenated togther and return a list of all of those netstrings.
;;
;; E.g., given "10:One little,13:, two little,,24:three little netstrings.,",
;; return ("One little" ", two little," "three little netstrings.")
(defun read-netstrings (ns)
  (if (or (eq ns nil) (equalp ns ""))
      nil
      (let ((result (read-one-netstring ns)))
        (cons (car result) (read-netstrings (cadr result))))))

;; A Mongrel2 message is formatted as:
;; UUID ID PATH SIZE:HEADERS,SIZE:BODY,
;; This function reads that out into a mongrel2-request.
(defun parse-message (msg)
  (multiple-value-bind (fields offset) (split-sequence #\Space msg :count 3)
    (let* ((uuid       (first fields))
           (id         (second fields))
           (path       (third fields))
           (netstrings (read-netstrings (subseq msg offset)))
           (headers    (first netstrings))
           (body       (second netstrings)))
      (make-instance 'mongrel2-message
                     :uuid uuid :id id
                     :path path :headers headers
                     :body body))))

