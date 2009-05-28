;;; ------------------------------------------------- -*- Mode: LISP -*-
;;; CL-ZEROCONF -- A Lisp library for Zeroconf service discovery.
;;;
;;; Copyright 2005
;;; John Wiseman (jjwiseman@yahoo.com)
;;; 2005-02-10
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; This file deals with condition classes and the translation of mDNS
;;; API errors to Lisp errors.

(in-package "DNS-SD")


;; Try to map error constants to useful error classes.

(defstruct error-info
  class
  code
  description)

(defparameter *dns-sd-errors* (make-hash-table :test #'eql)
  "A table of DNS SD API error codes, corresponding Lisp error classes, and text descriptions of the errors.")

(defmacro def-dns-sd-error (class code description)
  "Defines a DNS SD error based on a DNS SD API error code."
  `(eval-when (:load-toplevel :execute)
     (progn
       (setf (gethash ,code *dns-sd-errors*)
	     (make-error-info :class ',class :code ,code :description ,description))
       (define-condition ,class (dns-sd-result-error)
	 ()))))

(defun dns-sd-error (code)
  "Given a DNS SD API error code, raises the corresponding Lisp
  error.  If the code is unknown, an error of type
  DNS-SD-RESULT-ERROR is raised."
  (let ((info (gethash code *dns-sd-errors*)))
    (if info
	(error (error-info-class info) :code code)
	;; We couldn't find any info, which is weird, but at least we
	;; can signal a generic error.
	(error 'dns-sd-result-error :code code))))

(defun error-code->error-class (code)
  (let ((info (gethash code *dns-sd-errors*)))
    (error-info-class info)))
  

(define-condition dns-sd-error (simple-error)
  ()
  (:documentation "All errors specific to DNS SD are of this
  type."))

(define-condition dns-sd-result-error (simple-error)
  ((code :initarg :code :initform nil :accessor dns-service-error-code))
  (:report
   (lambda (condition stream)
     (let* ((code (dns-service-error-code condition))
	    (info (gethash code *dns-sd-errors*)))
       (if info
	   (format stream "The following DNS SD error occurred: ~A (code ~A)"
		   (error-info-description info)
		   code)
	   (format stream "An unknown DNS SD error occurred, with code ~A." code)))))
  (:documentation "Signaled if a foreign DNS SD API function returns an error code."))


;; Totally stealing this idiom of setting an implicitly created
;; function's documentation slot from Edi Weitz.

(setf (documentation 'dns-service-error-code 'function)
      "Returns the lowlevel DNS SD API error code associated with an error.")

;; Now let's define some error classes.

(def-dns-sd-error dns-sd-unknown-error dns-service-err-unknown
  "Unknown error (is the DNS Service Discovery system daemon running?)")
(def-dns-sd-error dns-sd-no-such-name-error dns-service-err-no-such-name
  "No such name error")
(def-dns-sd-error dns-sd-no-memory-error dns-service-err-no-memory
  "No memory error")
(def-dns-sd-error dns-sd-bad-param-error dns-service-err-bad-param
  "Bad parameter error")
(def-dns-sd-error dns-sd-bad-reference-error dns-service-err-bad-reference
  "Bad reference error")
(def-dns-sd-error dns-sd-bad-state-error dns-service-err-bad-state
  "Bad state error")
(def-dns-sd-error dns-sd-bad-flags-error dns-service-err-bad-flags
  "Bad flags error")
(def-dns-sd-error dns-sd-unsupported-error dns-service-err-unsupported
  "Unsupported error")
(def-dns-sd-error dns-sd-not-initialized-error dns-service-err-not-initialized
  "Not initialized error")
(def-dns-sd-error dns-sd-already-registered-error dns-service-err-already-registered
  "Already registered error")
(def-dns-sd-error dns-sd-name-conflict-error dns-service-err-name-conflict
  "Name conflict error")
(def-dns-sd-error dns-sd-invalid-error dns-service-err-invalid
  "Invalid error")
(def-dns-sd-error dns-sd-incompatible-error dns-service-err-incompatible
  "Incompatible error")
(def-dns-sd-error dns-sd-bad-interface-index-error dns-service-err-bad-interface-index
  "Bad interface index error")


(define-condition service-already-published-error (dns-sd-error)
  ((service :initarg :service :initform nil :accessor service-already-published-error-service))
  (:report
   (lambda (condition stream)
     (format stream "The service ~S is already being published."
	     (service-already-published-error-service condition)))))

(define-condition service-not-published-error (dns-sd-error)
  ((service :initarg :service :initform nil :accessor service-not-published-error-service))
  (:report
   (lambda (condition stream)
     (format stream "The service ~S has not been published."
	     (service-not-published-error-service condition)))))

(define-condition socket-fd-error (dns-sd-error)
  ((oid :initarg :oid :initform nil :accessor socket-fd-error-oid))
  (:report
   (lambda (condition stream)
     (format stream "DNS-SD OID ~S has no socket file descriptor." (socket-fd-error-oid condition)))))


