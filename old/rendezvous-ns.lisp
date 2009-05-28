(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :objc-support))


(defmacro with-nsstrs (init-forms &body body)
  "Establishes a lexical context with a bunch of stack allocated
   NSConstantStrings."
  (flet ((gen (s) #'(lambda (sym)
		      (declare (ignore sym))
		      (gensym s))))
    (let ((cstr-vars (mapcar (gen "CSTR") init-forms))
	  (string-vars (mapcar (gen "STRING") init-forms)))
      `(let ,(mapcar #'(lambda (init-form sv)
			 `(,sv ,(second init-form)))
	      init-forms string-vars)
	 (with-cstrs ,(mapcar #'(lambda (cv sv)
				  `(,cv ,sv))
			      cstr-vars string-vars)
	   (rlet
	    ,(mapcar
	      #'(lambda (init-form cstr-var string-var)
		  `(,(first init-form) :<NSC>onstant<S>tring
				       :isa ccl::*NSConstantString-class*
				       :bytes ,cstr-var
				       :num<B>ytes (length ,string-var)))
	      init-forms cstr-vars string-vars)
	    ,@body))))))


(defmacro @@ (string)
  "Creates a persistent, interned NSConstantString."
  `(ccl::objc-constant-string-nsstringptr (ccl::ns-constant-string ,string)))

(defvar *bonjour-loop* nil)
(defvar *bonjour-process* nil)


(defun run-loop ()
  (ccl::with-autorelease-pool
      (do ()
	  (nil)
	(let ((date (ccl::send (ccl::@class ns-date) :date-with-time-interval-since-now 5.0d0)))
	  (format T "~&+~%")
	  (ccl::send *bonjour-loop* :run-until-date date)
	  (ccl::send date 'release)))))


(defun ensure-run-loop ()
  (unless *bonjour-process*
    (setf *bonjour-process*
	  (process-run-function
	   "Bonjour Thread"
	   #'(lambda ()
	       (setf *bonjour-loop*
		     (ccl::send (ccl::@class ns-run-loop) 'current-run-loop))
	       (let ((timer (make-run-loop-timer)))
		 (ccl::send *bonjour-loop*
			    :add-timer timer
			    :for-mode #?NSDefaultRunLoopMode))
	       (run-loop)))))
  *bonjour-loop*)


(defclass timer-callback (ns:ns-object)
  ()
  (:metaclass ns:+ns-object))

(ccl::define-objc-method ((:void :invoke timer)
			  timer-callback)
    )

(defun make-run-loop-timer ()
  (let ((callback (ccl::make-objc-instance 'timer-callback)))
    (make-instance 'ns:ns-timer
		   :with-fire-date (ccl::send (ccl::@class ns-date) 'distant-past)
		   :interval 1.0d0
		   :target callback
		   :selector (ccl::@selector "invoke:")
		   :user-info (%null-ptr)
		   :repeats T)))


(defun advertise-service (domain type name port &key properties)
  "Publishes the availability of a service via Bonjour.

   The DOMAIN argument must be a string containing the domain name of
   the machine hosting the service (pass nil for localhost).  The TYPE
   argument is a string specifying the type of service being
   advertised (e.g., \"_http._tcp\" or \"_daap._tcp\").  NAME is a
   string naming the service.  PORT is the port associated with the
   service.  The PROPERTIES argument, if given, should be a property
   list containing additional service-specific information that will
   be part of the published information about the service. For
   example, ((\"path\" \"~/guest/\") (\"screenname\" \"clowny\")).

   This function returns a macptr that points to the Cocoa
   NetService instance representing the advertised service."
  (let ((net-service (ccl::make-objc-instance 'ns-net-service)))
    (with-nsstrs ((nsdomain (or domain ""))
		  (nstype type)
		  (nsname name))
      (ccl::send net-service
		 :init-with-domain nsdomain
		 :type nstype
		 :name nsname
		 :port port))
    (when properties
      ;; Hm, kind of odd that we have to use a constant NSString
      ;; here... (it fails if we use a stack-allocated one).
      (ccl::send net-service :set-protocol-specific-information
		 (@@ (build-text-record properties))))
    (ccl::send net-service :set-delegate
	       (ccl::make-objc-instance 'net-service-delegate))
    (ccl::send net-service
	       :schedule-in-run-loop (ensure-run-loop)
	       :for-mode #?NSDefaultRunLoopMode)
    (ccl::send net-service 'publish)
    net-service))


;; Sadly, this doesn't seem to work.

(defun cancel-service (net-service)
  "Stops advertising a service.

   SERVICE must be a macptr returned by ADVERTISE-SERVICE."
  ;;(ccl::send net-service
  ;;	     :remove-from-run-loop *bonjour-loop*
  ;;	     :for-mode #?NSDefaultRunLoopMode)
;;  (let ((delegate (ccl::send net-service 'delegate)))
;;    (ccl::send net-service :set-delegate (%null-ptr))
;;    (ccl::send delegate 'release))
  (ccl::send net-service 'stop)
 ; (sleep 3)
;  (ccl::send net-service 'release)
  )



(defparameter *record-separator* (code-char 1))

(defun build-text-record (properties)
  (reduce
   #'(lambda (p1 p2)
       (format nil "~A~A~A" p1 *record-separator* p2))
   (mapcar #'(lambda (property)
	       (format nil "~A=~A" (first property) (second property)))
	   properties)))


;; Here we define an Objective C class that acts as the callback
;; handler for NetService.

;(ccl::def-objc-class net-service-delegate ns-object)
(defclass net-service-delegate (ns:ns-object)
  ()
  (:metaclass ns:+ns-object))


(ccl::define-objc-method ((:void :net-service sender
				 :did-not-publish error-dict)
			  net-service-delegate)
    (debug-log "Did not publish service ~S." sender))

(ccl::define-objc-method ((:void :net-service sender
				 :did-not-resolve error-dict)
			  net-service-delegate)
    (debug-log "Did not resolve service ~S." sender))

(ccl::define-objc-method ((:void :net-service-did-resolve-address sender)
			  net-service-delegate)
    (debug-log "Did not resolve address for service ~S." sender))

(ccl::define-objc-method ((:void :net-service-did-stop sender)
			  net-service-delegate)
  (debug-log "Did stop service ~S." sender)
  (ccl::send sender 'release))

(ccl::define-objc-method ((:void :net-service-will-publish sender)
			  net-service-delegate)
    (debug-log "Will publish service ~S." sender))

(ccl::define-objc-method ((:void :net-service-will-resolve sender)
			  net-service-delegate)
    (debug-log "Will resolve service ~S." sender))


(defun debug-log (fmt &rest args)
  (apply #'format T fmt args)
  (terpri T))


(defun test ()
  (let ((services '()))
    (dotimes (i 100)
      (push (advertise-service "" "_http._tcp." (format nil "test ~S" i) (+ i 9000))
	    services))
    (dolist (service services)
      (cancel-service service))))

