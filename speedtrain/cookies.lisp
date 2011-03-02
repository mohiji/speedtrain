;; speedtrain/cookies.lisp
;; Cookie handling code
(in-package :speedtrain)

(defun format-cookie (name value &key (domain nil) (path nil) (secure nil) (httponly nil))
  (with-output-to-string (s)
    (format s "Set-Cookie: ~a=~a;" name value)
    (when domain
      (format s " Domain=~a;" domain))
    (when path
      (format s " Path=~a;" path))
    (when secure
      (format s " Secure;"))
    (when httponly
      (format s " HttpOnly;"))
    s))

(defun read-cookies (cookie-str)
  (let ((strings (split-sequence #\; cookie-str))
        (cookies (make-hash-table :test 'equalp)))
    (loop for string in strings do
         (let* ((trimmed (string-trim " " string))
                (fields (split-sequence #\= trimmed)))
           (setf (gethash (first fields) cookies) (second fields))))
    cookies))
