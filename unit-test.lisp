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
;;; Example code.  Er, I mean, unit tests.

(defpackage "DNS-SD-TEST"
  (:use "COMMON-LISP")
  (:export #:run-tests))


(in-package "DNS-SD-TEST")


;; Some simple unit test utilities

(defvar *passed-tests* '())
(defvar *failed-tests* '())

(defmacro test (name expr expected-value &optional (comparator '(function equal))
		failure-code)
  `(unless (test-aux ',name ',expr ,expr ,expected-value ,comparator)
    ,failure-code))

(defmacro condition-test (name expr expected-condition &optional (comparator '(function typep))
			  failure-code)
  (let ((completed-var (gensym "COMPLETED"))
	(condition-var (gensym "CONDITION"))
	(value-var (gensym "VALUE")))
    `(let ((,completed-var NIL))
       (multiple-value-bind (,value-var ,condition-var)
	   (ignore-errors
	     ,expr
	     (setf ,completed-var T))
	 (unless (condition-test-aux ',name ',expr ,value-var (not ,completed-var)
				     ,condition-var ,expected-condition ,comparator)
	   ,failure-code)))))

(defun condition-test-aux (name expr value error-p error expected-error comparator)
  (if error-p
      (let ((got-expected-p (funcall comparator error expected-error)))
	(if got-expected-p
	    (test-success name expr error expected-error)
	    (test-failure name expr error expected-error))
	got-expected-p)
      (test-failure name expr value expected-error)))

(defun test-aux (name expr value expected-value comparator)
  (let ((got-expected-p (funcall comparator value expected-value)))
    (if got-expected-p
	(test-success name expr value expected-value)
	(test-failure name expr value expected-value))
    got-expected-p))

(defun test-failure (name expr value expected-value)
  (assert (not (assoc name *failed-tests*)))
  (assert (not (assoc name *passed-tests*)))
  (push (cons name (list expr value expected-value)) *failed-tests*)
  (warn "FAILURE: Test ~S: ~S evaluated to ~S instead of ~S."
	name expr value expected-value)
  nil)

(defun test-success (name expr value expected-value)
  (assert (not (assoc name *failed-tests*)))
  (assert (not (assoc name *passed-tests*)))
  (push (cons name (list expr value expected-value)) *passed-tests*)
  (format T "~&Test ~S passed.~%" name))

(defun begin-tests ()
  (setf *passed-tests* '())
  (setf *failed-tests* '()))

(defun end-tests ()
  (let ((num-failed (length *failed-tests*))
	(num-passed (length *passed-tests*)))
    (format T "~&-----~&Testing complete, ~S of ~S tests failed (~,2F)"
	    num-failed
	    (+ num-failed num-passed)
	    (/ num-failed (+ num-failed num-passed)))))


;; Define our own subclasses and methods for slightly useful stuff.

(defclass test-observer ()
  ((events :initform '() :accessor events)
   (resolve-p :initform T :initarg :resolve-p :accessor resolve-p)))

(defmethod record-event ((self test-observer) event)
  (format T "~%RECORDING EVENT ~S~%" event)
  (setf (events self) (append (events self) (list event))))

(defmethod clear-events ((self test-observer))
  (setf (events self) '()))

(defmethod dns-sd:publish-success ((self test-observer) service name)
  (record-event self (cons :published (list service name))))

(defmethod dns-sd:publish-error ((self test-observer) service error)
  (record-event self (cons :error (list service error))))

(defmethod dns-sd:browse-add-service ((self test-observer) service interface-index more-coming-p)
  (record-event self (cons :added (list service interface-index more-coming-p)))
  (when (resolve-p self)
    (dns-sd:resolve service
		    self)))

(defmethod dns-sd:browse-resolved-service ((self test-observer) service)
  (record-event self (cons :resolved (list service))))

(defmethod dns-sd:browse-remove-service ((self test-observer) service interface-index more-coming-p)
  (record-event self (cons :removed (list service interface-index more-coming-p))))

(defmethod dns-sd:browse-error ((self test-observer) error)
  (record-event self (cons :error (list error))))


(defvar *test-observer* (make-instance 'test-observer))


(defclass test-service (dns-sd:service)
  ())

(defmethod dns-sd:publish :around ((self test-service) observer &rest args)
  (declare (ignore args))
  (prog1 (call-next-method)
    (format T "~&Published ~S~%" self)))

(defmethod dns-sd:cancel :around ((self test-service))
  (format T "~&Cancelling ~S~%" self)
  (call-next-method))

(defmacro with-auto-resolve ((observer auto-resolve-p) &body body)
  (let ((observer-var (gensym "OBSERVER"))
	(resolve-var (gensym "ORIGINAL-RESOLVE-P")))
    `(let* ((,observer-var ,observer)
	    (,resolve-var (resolve-p ,observer-var)))
      (unwind-protect
	   (progn
	     (setf (resolve-p ,observer-var) ,auto-resolve-p)
	     ,@body)
	(setf (resolve-p ,observer-var) ,resolve-var)))))


(defun process-events ()
  (sleep 2.0)
  (labels ((process ()
	     (when (dns-sd:process-dns-sd-events 3.0)
	       (process))))
    (process))
  (values))


(defun run-tests ()
  (begin-tests)
  (unwind-protect
      (progn
	(test-string-encoding)
	(test-txt-record-building)
	(test-txt-record-parsing)
	(test-publish-conditions)
	(test-resolving-txt-records)
	(test-browsing-resolving)
	(test2)
	(test4)
	)
    (end-tests)))

(defun browse (type &optional domain (observer *test-observer*))
  (format T "Browsing for ~S" type)
  (dns-sd:browse type domain observer))

(defun test-browsing-resolving ()
  (let* ((random (random 100000))
	 (service-name (format nil "test service ~A" random))
	 (service (make-instance 'test-service
				 :name service-name
				 :type "_test._tcp."
				 :port 5000)))
    (with-auto-resolve (*test-observer* T)
      (let ((s (dns-sd:publish service *test-observer*)))
	(process-events)
	(clear-events *test-observer*)
	(test browse-1-12 s 'dns-sd:service #'typep
	      (progn (format T "~&TYPE-OF: ~S" (type-of s))
		     (format T "~&TYPEP: ~S" (typep s 'dns-sd:service)))))
      (let ((browser (browse "_test._tcp")))
	(test browse-1-13 browser 'dns-sd:browser #'typep)
	(process-events)
	(let ((c (dns-sd:cancel service)))
	  (test browse-1-14 c service))
	(process-events)
	(let ((c (dns-sd:cancel browser)))
	  (test browse-1-15 c browser)))
      (process-events)
      (let ((events (events *test-observer*)))
	(test browse-1-1 (length events) 3 #'eql
	      (progn (format T "EVENTS: ~S" events)))
	(let ((ev1 (first events))
	      (ev2 (second events))
	      (ev3 (third events)))
	  (test browse-1-2 (car ev1) :added)
	  (let ((s (cadr ev1)))
	    (test browse-1-3 (dns-sd:service-name s) service-name)
	    (test browse-1-4 (dns-sd:service-type s) "_test._tcp.")
	    (test browse-1-5 (dns-sd:service-domain s) "local.")
	    (test browse-1-6 (dns-sd:service-host s) "Johns-Computer.local." #'equal
		  (format T "~&This test won't work well for you, will it?")))
	  (test browse-1-7 (car ev2) :resolved)
	  (test browse-1-8 (dns-sd:service-port (cadr ev2)) 5000)
	  (test browse-1-9 (car ev3) :removed)
	  (test browse-1-10 (cadr ev1) (cadr ev2) #'eq)
	  (test browse-1-11 (cadr ev1) (cadr ev3) #'eq))))))


(defun test2 ()
  (let* ((random (random 100000))
	 (service-name (format nil "test service ~A" random))
	 (service (make-instance 'test-service
				 :name service-name
				 :type "_test._tcp."
				 :port 5000)))
    (clear-events *test-observer*)
    (with-auto-resolve (*test-observer* NIL)
      (dns-sd:publish service nil)
      (let ((browser (browse "_test._tcp")))
	(process-events)
	(dns-sd:cancel service)
	(process-events)
	(dns-sd:cancel browser))
      (let ((events (events *test-observer*)))
	(test browse-2-1 (length events) 2)
	(let ((ev1 (first events))
	      (ev2 (second events)))
	  (test browse-2-2 (car ev1) :added)
	  (let ((s (cadr ev1)))
	    (test browse-2-3 (dns-sd:service-name s) service-name)
	    (test browse-2-4 (dns-sd:service-type s) "_test._tcp.")
	    (test browse-2-5 (dns-sd:service-domain s) "local."))
	  (test browse-2-6 (car ev2) :removed)
	  (test browse-2-7 (cadr ev1) (cadr ev2) #'eq))))))

(defun test4 ()
  (let ((s1 (make-instance 'test-service
			   :name "test 1"
			   :type "_test._tcp."
			   :port 5000))
	(s2 (make-instance 'test-service
			   :name "test 2"
			   :type "_test._tcp."
			   :port 5000))
	(s3 (make-instance 'test-service
			   :name "test 3"
			   :type "_test._tcp."
			   :port 5000)))
    (dns-sd:publish s1 *test-observer*)
    (dns-sd:publish s2 *test-observer*)
    (dns-sd:publish s3 *test-observer*)
    (process-events)
    (clear-events *test-observer*)
    (with-auto-resolve (*test-observer* NIL)
      (let ((browser (browse "_test._tcp")))
	(process-events)
	(let ((events (events *test-observer*)))
	  (test browse-4-1 (length events) 3 #'eql
		(format T "~&EVENTS: ~S" events))
	  (test browse-4-2 (mapcar #'car events) '(:added :added :added))
	  (test browse-4-3 (mapcar #'(lambda (e) (third (cdr e))) events) '(T T NIL)))
	(dns-sd:cancel s1)
	(dns-sd:cancel s2)
	(dns-sd:cancel s3)
	(process-events)
	(dns-sd:cancel browser)))))

(defun test-publish-conditions ()
  (let ((s1 (make-instance 'test-service))
	(s2 (make-instance 'test-service
			   :name 5))
	(s3 (make-instance 'test-service
			   :name "foo 3"))
	(s4 (make-instance 'test-service
			   :name "foo 4"
			   :type 5))
	(s5 (make-instance 'test-service
			   :name "foo 5"
			   :type "_test._tcp."))
	(s6 (make-instance 'test-service
			   :name "foo 6"
			   :type "_test._tcp."
			   :port "port"))
	(s7 (make-instance 'test-service
			   :name "foo 7"
			   :type "_test._tcp."
			   :port "port"))
	(s8 (make-instance 'test-service
			   :name "foo 8"
			   :type "_test._tcp."
			   :port 10))
	(s9 (make-instance 'test-service
			   :name "foo 9"
			   :type "_test._tcp."
			   :port 10))
	(s10 (make-instance 'test-service
			    :name "foo 10"
			    :type "_test._tcp."
			    :port 10)))
    (condition-test publish-5-1 (dns-sd:publish s1 *test-observer*) 'type-error)
    (condition-test publish-5-2 (dns-sd:publish s2 *test-observer*) 'type-error)
    (condition-test publish-5-3 (dns-sd:publish s3 *test-observer*) 'type-error)
    (condition-test publish-5-4 (dns-sd:publish s4 *test-observer*) 'type-error)
    (condition-test publish-5-5 (dns-sd:publish s5 *test-observer*) 'type-error)
    (condition-test publish-5-6 (dns-sd:publish s6 *test-observer*) 'type-error)
    (condition-test publish-5-7 (dns-sd:publish s7 *test-observer*) 'type-error)
    (condition-test publish-5-8 (dns-sd:publish s8  *test-observer* :if-exists nil) 'type-error)
    (condition-test publish-5-9 (dns-sd:publish s9  *test-observer* :if-exists :asdfsf) 'type-error)
    (condition-test publish-5-10 (dns-sd:publish s10  *test-observer* :interface (vector 1 2)) 'type-error)
    (dns-sd:publish s10 *test-observer*)
    (process-events)
    (condition-test publish-5-11 (dns-sd:publish s10 *test-observer*) 'dns-sd:service-already-published-error)
    (dns-sd:cancel s10)
    (process-events))
#|

It seems that I don't understand yet what Bonjour is allowed to do
when you try to register two services with the same name, neither of
which is allowed to be renamed.  What I've seen in Linux: First
service publishes successfully, second service publishes successfully,
second service gets name conflict error.

  (clear-events *test-observer*)
  (let ((s1 (make-instance 'test-service
			   :name "foo"
			   :type "_test._tcp."
			   :port 10))
	(s2 (make-instance 'test-service
			   :name "foo"
			   :type "_test._tcp."
			   :port 11)))
    (dns-sd:publish s1 *test-observer* :if-exists :error)
    (process-events)
    (let ((events (events *test-observer*)))
      (test publish-5-12
	    (length events)
	    1)
      (test publish-5-13
	    (car (first events)) :published)
      (test publish-5-14
	    (cadr (first events)) s1)
      (test publish-5-15
	    (caddr (first events)) "foo")
      (clear-events *test-observer*))
    (dns-sd:publish s2 *test-observer* :if-exists :error)
    (process-events)
    (let ((events (events *test-observer*)))
      (test publish-5-16
	    (length events)
	    1)
      (test publish-5-17
	    (car (first events)) :error)
      (test publish-5-18
	    (cadr (first events)) s2)
      (test publish-5-19
	    (caddr (first events))
	    'dns-sd:dns-sd-name-conflict-error
	    #'typep))
    (dns-sd:cancel s1)
    (dns-sd:cancel s2))
|#
)


	    


(defun test3 ()
  (let* ((random-1 (random 100000))
	 (service-1-name (format nil "test service ~A" random-1))
	 (service-1 (make-instance 'test-service
				   :name service-1-name
				   :type "_test._tcp."
				   :port 5000))
	 (service-2 (make-instance 'test-service
				   :name service-1-name
				   :type "_test._tcp."
				   :port 5001)))
    (clear-events *test-observer*)
    (dns-sd:publish service-1 *test-observer*)
    (dns-sd:publish service-2 *test-observer*)
    (events *test-observer*)))


(defun test-string-encoding ()
  (test string-encoding-1
	(dns-sd::string-to-bytes "name")
	#(110 97 109 101)
	#'equalp)
  (test string-encoding-2
	(dns-sd::bytes-to-string (dns-sd::string-to-bytes "some string"))
	"some string"
	#'equalp))


(defun test-txt-record-parsing ()
  (test parse-txt-record-1
	(dns-sd:parse-txt-record #(8 110 97 109 101 61 1 2 3))
	'(("name" . #(1 2 3)))
	#'equalp)
  (let ((r '(("prop1" . #(1 2 3)))))
    (test parse-txt-record-2
	  (dns-sd:parse-txt-record (dns-sd:build-txt-record r))
	  r
	  #'equalp))
  (let ((r '(("prop1" . #(1 2 3))
	     ("prop2" . #(4 5 6)))))
    (test parse-txt-record-3
	  (dns-sd:parse-txt-record (dns-sd:build-txt-record r))
	  r
	  #'equalp))
  (let ((r '(("prop1" . #()))))
    (test parse-txt-record-4
	  (dns-sd:parse-txt-record (dns-sd:build-txt-record r))
	  r
	  #'equalp))
  (let ((r '(("prop1"))))
    (test parse-txt-record-5
	  (dns-sd:parse-txt-record (dns-sd:build-txt-record r))
	  r
	  #'equalp)))


	      
(defun test-txt-record-building ()
  (test build-txt-record-1
	(dns-sd:build-txt-record '(("name" . #(1 2 3))))
	#(8 110 97 109 101 61 1 2 3)
	#'equalp)
  (test build-txt-record-2
	(dns-sd:build-txt-record '(("name"  . #(1 2 3))
			       ("robot" . #(4 5 6 7))))
	#(8 110 97 109 101 61 1 2 3 10 114 111 98 111 116 61 4 5 6 7)
	#'equalp)
  (test build-txt-record-3
	(dns-sd:build-txt-record '(("name")))
	#(4 110 97 109 101)
	#'equalp)
  (test build-txt-record-4
	(dns-sd:build-txt-record '(("one")
			       ("two")))
	#(3 111 110 101 3 116 119 111)
	#'equalp)
  (test build-txt-record-5
	(dns-sd:build-txt-record '(("one" . #())))
	#(4 111 110 101 61)
	#'equalp)
  (test build-txt-record-6
	(dns-sd:build-txt-record '(("one" . #())
			       ("two" . #())
			       ("three" . #(3))))
	#(4 111 110 101 61 4 116 119 111 61 7 116 104 114 101 101 61 3)
	#'equalp)
  (test build-txt-record-7
	(dns-sd:build-txt-record '(("Prop1" . #(1 2 3))
			       ("Prop2")
			       ("Prop3" . #())
			       ("Prop4" . "a value")))
	#(9 80 114 111 112 49 61 1 2 3 5 80 114 111 112 50 6 80 114 111
	  112 51 61 13 80 114 111 112 52 61 97 32 118 97 108 117 101)
	#'equalp))
			       

(defun test-resolving-txt-records ()
  (clear-events *test-observer*)
  (let ((service (make-instance 'test-service
				:name "service with a text record"
				:type "_test._tcp."
				:port 1234
				:txt-record (dns-sd:build-txt-record '(("prop1" . "value1")
								   ("prop2" . #(1 2 3)))))))
    (dns-sd:publish service *test-observer*)
    (with-auto-resolve (*test-observer* T)
      (let ((browser (dns-sd:browse "_test._tcp." nil *test-observer*)))
	(process-events)
	(dns-sd:cancel service)
	(dns-sd:cancel browser)
	(process-events)
	(let* ((events (events *test-observer*))
	       (resolved-event (car (member :resolved events :key #'car)))
	       (resolved-service (second resolved-event)))
	  (test txt-record-1-1 (not (null resolved-event)) T #'eql
		(format T "~&Events: ~S" events))
	  (test txt-record-1-2 (not (null (dns-sd:service-txt-record resolved-service))) T #'eql
		(format T "~&TXT record data: ~S" (dns-sd:service-txt-record resolved-service)))
	  (test txt-record-1-3 
		(dns-sd:service-txt-record resolved-service)
		(dns-sd:build-txt-record '(("prop1" . "value1")
				       ("prop2" . #(1 2 3))))
		#'equalp)
	  (test txt-record-1-4
		(dns-sd:parse-txt-record (dns-sd:service-txt-record resolved-service))
		'(("prop1" . #(118 97 108 117 101 49))
		  ("prop2" . #(1 2 3)))
		#'equalp
		(format T "~&Parsed TXT record: ~S" (dns-sd:parse-txt-record (dns-sd:service-txt-record resolved-service)))))))))
						      


;; See http://lists.apple.com/archives/rendezvous-dev/2005/Jan/msg00018.html

(defun test-sub-types ()
  (let ((type "_test._tcp,_mysubtype"))
    ))
