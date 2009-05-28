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
;;; This is an interface to Apple's mDNS API.

(in-package "DNS-SD")


;; You can think of OIDs ("Operation ID"?) as sessions, a way of
;; keeping track of ongoing operations.  An OID entry consists of a
;; distinct DNSServiceRef and the type of operation (:PUBLISH,
;; :BROWSE, :RESOLVE or :QUERY), and optionally an observer object
;; associated with the operation.  Finally, some OID entries list a
;; service associated with the operation.  E.g., when an attempt is
;; made to RESOLVE a service the service being resolved will be stored
;; with the OID.

;; DNSServiceRefs are used as keys into our OID table, and we store
;; them in the OID slot of the following structure.

(defstruct oid-entry
  oid
  type
  observer
  services)

(defun oid= (o1 o2)
  "Checks two OIDs for equality (underlying foreign object identity)."
  (eql (uffi:pointer-address o1) (uffi:pointer-address o2)))


;; It seems unlikely that we're going to keep track of many OIDs (more
;; than a dozen?), so we just use an alist.

(defvar *oid-table* '()
  "Association list of active OID entries.")

(defun get-oid-entry (oid)
  "Returns the OID entry associated with the specified OID."
  (cdr (assoc oid *oid-table* :test #'oid=)))

(defun set-oid-entry (oid type observer services)
  "Creates an OID entry with the specified OID, TYPE, OBSERVER
  and SERVICES list.  Overwrites the existing entry if the OID is
  already active."
  (let ((record (make-oid-entry :oid oid :type type :observer observer :services services)))
    (let ((pair (assoc oid *oid-table* :test #'oid=)))
      (if pair
	  (setf (cdr pair) record)
	  (push (cons oid record) *oid-table*)))))

(defun clear-oid-entry (oid)
  "Removes the OID entry for the specified OID."
  (setf *oid-table* (remove oid *oid-table* :test #'oid= :key #'car)))

	  
(defvar *oid-lock* (make-lock "DNS-SD OID Lock")
  "Mutex serializing access to OID entries.")

(defvar *active-oids* '()
  "List of OIDs in the event loop.")

(defun all-oids ()
  "Returns a list of all OIDs in the event loop."
  (with-lock (*oid-lock*)
    (%all-oids)))

(defun %all-oids ()
  (mapcar #'car *active-oids*))

(defun force-close-all-oids ()
  "Deallocates all active OIDs and removes them from the event
  loop.  Might be useful for cleanup during development and
  debugging."
  (with-lock (*oid-lock*)
    (dolist (oid (%all-oids))
      (%remove-oid-from-event-loop oid)
      (%deallocate-oid oid)
      (let ((oid-entry (get-oid-entry oid)))
	(dolist (service (oid-entry-services oid-entry))
	  (setf (service-oid service) nil)))))
    (values))

(defun deallocate-oid (oid)
  "Deallocates an OID (threadsafe)."
  (with-lock (*oid-lock*)
    (%deallocate-oid oid)))

(defun %deallocate-oid (oid)
  "Deallocates an OID (not threadsafe)."
  (debug-log "Deallocating DNS-SD OID ~S (fd ~S)" oid (dns-service-ref-sock-fd oid))
  (dns-service-ref-deallocate oid))

(defun active-fds ()
  "Returns a list of the socket file descriptors associated with active OIDs (threadsafe)."
  (with-lock (*oid-lock*)
    (%active-fds)))

(defun %active-fds ()
  "Returns a list of the socket file descriptors associated with active OIDs (not threadsafe)."
  (mapcar #'cdr *active-oids*))

(defun process-dns-sd-events (&optional timeout)
  "Gets a response from the mDNS responder daemon and notifies
  the appropriate event observers.  Events for all active OIDs
  may be processed.  This function blocks until it gets a
  response from the daemon unless an optional TIMEOUT argument
  has been specified.  TIMEOUT must be the maximum number of
  seconds to wait for a response (the default value of nil means
  \"wait forever\".  This function returns T if one or more
  events were handled, NIL otherwise.  It is threadsafe."
  (with-lock (*oid-lock*)
    (let ((interesting-fd.oids (mapcar #'(lambda (fd)
					   (cons fd (%oid-for-fd fd)))
				       (fds-input-available-p (%active-fds) timeout))))
      (dolist (fd.oid interesting-fd.oids)
	(destructuring-bind (fd . oid) fd.oid
	  (debug-log "Processing DNS-SD events for fd ~S, OID ~S" fd oid)
	  (dns-service-process-result oid)))
      (not (null interesting-fd.oids)))))

(defun add-oid-to-event-loop (oid)
  "Adds an OID to the event loop (threadsafe)."
  (with-lock (*oid-lock*)
    (debug-log "Adding OID ~S (fd ~S) to DNS-SD event loop." oid (dns-service-ref-sock-fd oid))
    (push (cons oid (dns-service-ref-sock-fd oid))
	  *active-oids*)))

(defun remove-oid-from-event-loop (oid)
  "Removes an OID from the event loop (threadsafe)."
  (with-lock (*oid-lock*)
    (%remove-oid-from-event-loop oid)))

(defun %remove-oid-from-event-loop (oid)
  "Removes an OID from the event loop (not threadsafe)."
  (debug-log "Removing OID ~S (fd ~S) from DNS-SD event loop." oid (dns-service-ref-sock-fd oid))
  (setf *active-oids*
	(remove oid *active-oids* :key #'car :test #'oid=)))


(defun oid-for-fd (fd)
  "When passed a socket file descriptor, returns the active OID
  with which the descriptor is associated (threadsafe)."
  (with-lock (*oid-lock*)
    (%oid-for-fd fd)))

(defun %oid-for-fd (fd)
  "When passed a socket file descriptor, returns the active OID
  with which the descriptor is associated (not threadsafe)."
  (car (rassoc fd *active-oids*)))


(defparameter *browse-flags*
  `((:more-coming		. ,dns-service-flags-more-coming)
    (:finished			. ,dns-service-flags-finished)
    (:add			. ,dns-service-flags-add)
    (:remove			. ,dns-service-flags-remove)))

(defun browse-flags->symbols (flags)
  (let ((syms '()))
    (dolist (flag.val *browse-flags*)
      (when (not (zerop (logand flags (cdr flag.val))))
	(push (car flag.val) syms)))
    (when (not (member :more-coming syms))
      (push :finished syms))
    (when (not (member :add syms))
      (push :remove syms))
    syms))


;; ----------
;; Callbacks (*not* implementation-specific)
;; ----------

(defun publish-callback-trampoline (oid flags error-code name type domain context)
  (debug-log "DNS-SD publish callback: ~S ~S ~S ~S ~S ~S ~S"
	     oid flags error-code name type domain context)
  (let* ((record (get-oid-entry oid))
	 (observer (oid-entry-observer record))
	 (services (oid-entry-services record)))
    (when observer
      (assert (null (cdr services)))
      (let ((service (car services)))
	(if (not (= error-code 0))
	    (publish-error observer service (make-condition (error-code->error-class error-code)
							    :code error-code))
	    (publish-success observer service name))))))
    

(defun browse-callback-trampoline (oid flags interface-index error-code name type domain context)
  (debug-log "DNS-SD browse callback: ~S ~S ~S ~S ~S ~S ~S ~S"
	     oid flags interface-index error-code name type domain context)
  (let* ((record (get-oid-entry oid))
	 (observer (oid-entry-observer record)))
    (if (not (= error-code 0))
	(browse-error observer (make-condition (error-code->error-class error-code)
					       :code error-code))
	(let* ((flag-syms (browse-flags->symbols flags))
	       (more-coming-p (and (member :more-coming flag-syms) T)))
	  (format T "~&BROWSE FLAGS: ~S" flag-syms)
	  (cond ((member :add flag-syms)
		 (let ((service (make-instance 'service
					       :name name
					       :type type
					       :domain domain)))
		   (push service (oid-entry-services record))
		   (browse-add-service observer service interface-index more-coming-p)))
		((member :remove flag-syms)
		 (let ((service (find-if (service-finder :name name :type type :domain domain)
					 (oid-entry-services record))))
		   (unless service
		     (setf service (make-instance 'service
						  :name name
						  :type type
						  :domain domain)))
		   (browse-remove-service observer service interface-index more-coming-p)))
		(T
		 (error "How did this happen?")))))))


(defun resolve-callback-trampoline (oid flags interface-index error-code full-name
				    host-target port
                                    txt-record-len txt-record context)
  (debug-log "DNS-SD resolve callback: ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S"
	   oid flags interface-index error-code full-name host-target port
           txt-record-len txt-record context)
  (let* ((record (get-oid-entry oid))
         (observer (oid-entry-observer record))
         (services (oid-entry-services record)))
    (assert (null (cdr services)))
    (if (not (= error-code 0))
	(browse-error observer (make-condition (error-code->error-class error-code)
					       :code error-code))
	(let ((service (car services)))
	  (setf (service-host service) host-target)
	  (setf (service-port service) port)
	  (setf (service-txt-record service) (txt-record-to-lisp-array txt-record txt-record-len))
	  (browse-resolved-service observer service)
	  (remove-oid-from-event-loop oid)
	  (clear-oid-entry oid)
	  (deallocate-oid oid)))))


(defun query-callback-trampoline (oid flags interface-index error-code full-name
				  rrtype rrclass rdlen rdata ttl context)
  (debug-log "DNS-SD query callback: ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S"
	     oid flags interface-index error-code full-name rrtype rrclass rdlen rdata ttl context)
  (when (and (= rrtype 1) (= rdlen 4))
    (debug-log "~S resolved to IP ~S.~S.~S.~S"
	       full-name
	       (uffi:deref-array rdata '(:array :unsigned-char) 0)
	       (uffi:deref-array rdata '(:array :unsigned-char) 1)
	       (uffi:deref-array rdata '(:array :unsigned-char) 2)
	       (uffi:deref-array rdata '(:array :unsigned-char) 3))))


(defun service-finder (&key name type domain)
  #'(lambda (service)
      (and (or (null name) (equal (service-name service) name))
           (or (null type) (equal (service-type service) type))
           (or (null domain) (equal (service-domain service) domain)))))


(defclass service ()
  ((name       :initform nil :initarg :name       :accessor service-name)
   (type       :initform nil :initarg :type       :accessor service-type)
   (domain     :initform nil :initarg :domain     :accessor service-domain)
   (host       :initform nil :initarg :host       :accessor service-host)
   (port       :initform nil :initarg :port       :accessor service-port)
   (txt-record :initform nil :initarg :txt-record :accessor service-txt-record)
   (oid        :initform nil :initarg :oid        :accessor service-oid)))

(defmethod print-object ((self service) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "name: ~S type: ~S domain: ~S host: ~S port: ~S txt-record: ~S"
	    (service-name self)
	    (service-type self)
	    (service-domain self)
	    (service-host self)
	    (service-port self)
	    (service-txt-record self))
    (when (service-oid self)
      (format stream " OID: ~S" (service-oid self)))))


(defgeneric publish (service observer &key if-exists interface synchronous))
  
(defgeneric cancel (service-or-browser))

(defgeneric resolve (service observer))


#|
(defmethod publish-sync ((self service) &key (if-exists :rename) (interface 0))
  (publish self *synchronous-observer* :if-exists if-exists :interface interface))
|#

(defmethod publish ((self service) observer &key (if-exists :rename) (interface 0) synchronous)
  (check-type (service-name self) string)
  (check-type (service-type self) string)
  (check-type (service-port self) (integer 0 65535))
  (check-type if-exists (member :rename :error))
  (check-type interface integer)
  (unless (null (service-oid self))
    (error 'service-already-published-error :service self))
  (let ((oid-ptr (uffi:allocate-foreign-object 'dns-service-ref))
	(flags (if (eq if-exists :rename)
		   dns-service-flags-auto-rename
		   dns-service-flags-no-auto-rename))
	(interface-index interface))
    (let* ((txt-record-sequence (service-txt-record self))
	   (txt-record-ptr (if (> (length txt-record-sequence) 0)
			       (uffi:allocate-foreign-object :unsigned-char (length txt-record-sequence))
			       (uffi:make-null-pointer :unsigned-char))))
      (unwind-protect
	   (progn
	     (copy-txt-record-bytes txt-record-sequence txt-record-ptr)
	     (uffi:with-cstrings ((cname (service-name self))
				  (ctype (service-type self))
				  (cdomain (service-domain self))
				  (chost (service-host self)))
	       (dns-service-register oid-ptr flags interface-index cname ctype
				     cdomain
				     chost
				     (service-port self)
				     (length txt-record-sequence)
				     txt-record-ptr
				     %publish-callback-trampoline
				     (uffi:make-null-pointer :void))
	       (let ((oid (uffi:deref-pointer oid-ptr dns-service-ref)))
		 (setf (service-oid self) oid)
		 (set-oid-entry oid :publish observer (list self))
		 (add-oid-to-event-loop oid)
		 (if synchronous
		     (dns-service-process-result oid)))))
	(uffi:free-foreign-object txt-record-ptr))))
  self)

(defun copy-txt-record-bytes (sequence txt-record-ptr)
  (dotimes (i (length sequence))
    (setf (uffi:deref-array txt-record-ptr '(:array :unsigned-char) i) (elt sequence i)))
  txt-record-ptr)

(defmethod cancel ((self service))
  (let ((oid (service-oid self)))
    (unless oid
      (error 'service-not-published-error :service self))
    (remove-oid-from-event-loop oid)
    (clear-oid-entry oid)
    (setf (service-oid self) nil)
    (deallocate-oid oid))
  self)


(defclass browser ()
  ((oid :initform nil :initarg :oid :accessor browser-oid)))

(defun browse (type domain observer)
  (let ((oid-ptr (uffi:allocate-foreign-object 'dns-service-ref))
	(flags 0)
	(interface-index 0))
    (uffi:with-cstring (ctype type)
      (uffi:with-cstring (cdomain domain)
	(dns-service-browse oid-ptr
			    flags
			    interface-index
			    ctype
			    cdomain
			    %browse-callback-trampoline
			    (uffi:make-null-pointer :void))))
    (let ((oid (uffi:deref-pointer oid-ptr dns-service-ref)))
      (set-oid-entry oid :browse observer '())
      (add-oid-to-event-loop oid)
      (make-instance 'browser
		     :oid oid))))


(defmethod cancel ((self browser))
  (let ((oid (browser-oid self)))
    (unless oid
      (error "Browser ~S has already been canceled." self))
    (remove-oid-from-event-loop oid)
    (clear-oid-entry oid)
    (setf (browser-oid self) nil)
  (deallocate-oid oid)
  self))


(defmethod resolve ((self service) observer)
  (let ((oid-ptr (uffi:allocate-foreign-object 'dns-service-ref))
	(flags 0)
	(interface-index 0))
    (uffi:with-cstring (cname (service-name self))
      (uffi:with-cstring (ctype (service-type self))
	(uffi:with-cstring (cdomain (service-domain self))
	  (dns-service-resolve oid-ptr
			       flags
			       interface-index
			       cname
			       ctype
			       cdomain
			       %resolve-callback-trampoline
			       (uffi:make-null-pointer :void)))))
    (let* ((oid (uffi:deref-pointer oid-ptr dns-service-ref)))
      (set-oid-entry oid :resolve observer (list self))
      (add-oid-to-event-loop oid))))


;; This is not finished.
(defun query-record (type class full-name observer)
  (declare (ignore class))
  (let ((oid-ptr (uffi:allocate-foreign-object 'dns-service-ref))
	(flags 0)
	(interface-index 0))
    (uffi:with-cstring (cname full-name)
      (dns-service-query-record oid-ptr
				flags
				interface-index
				cname
				type
				1
				%query-callback-trampoline
				(uffi:make-null-pointer :void)))
    (let* ((oid (uffi:deref-pointer oid-ptr dns-service-ref)))
      (set-oid-entry oid :query observer '())
      (add-oid-to-event-loop oid))))

  

(defvar *debug-logging-p* NIL)
(defvar *debug-log-stream* *terminal-io*)

(defun debug-log (fmt &rest args)
  (when *debug-logging-p*
    (format *debug-log-stream* "~&~A~%" (apply #'format nil fmt args))))


;; Observer events.

(defgeneric publish-error (observer service error))
(defgeneric publish-success (observer service name))

(defgeneric browse-add-service (observer service interface-index more-coming-p))
(defgeneric browse-remove-service (observer service interface-index more-coming-p))
(defgeneric browse-error (observer error))
(defgeneric browse-resolved-service (observer service))


#|
(defclass synchronous-observer ()
  ())

(defmethod browse-add-service ((self synchronous-observer) service interface-index more-coming-p)
  (declare (ignore self service interface-index more-coming-p)))

(defmethod browse-remove-service ((self synchronous-observer) service interface-index more-coming-p)
  (declare (ignore self service interface-index more-coming-p)))

(defmethod browse-error ((self synchronous-observer) error)
  (error error))

(defmethod browse-resolve-service ((self synchronous-observer) service)
  (declare (ignore self service)))
|#


;; Section 6.1 of
;; http://files.dns-sd.org/draft-cheshire-dnsext-dns-sd.txt claims
;; that "when using Multicast DNS [mDNS] the maximum packet size is
;; 9000 bytes, which imposes an upper limit on the size of TXT records
;; of about 8800 bytes."  Maybe I should be checking for that.  See
;; also section 6.3 of the above document for more recommendations on
;; TXT record size.
;;
;; "As a general rule, attribute names that contain no dots are
;; defined as part of the open-standard definition written by the
;; person or group defining the DNS-SD profile for discovering that
;; particular service type. Vendor-specific extensions should be given
;; names of the form "keyname.company.com=value", using a domain name
;; legitimately registered to the person or organization creating the
;; vendor-specific key. This reduces the risk of accidental conflict
;; if different organizations each define their own vendor-specific
;; keys."

(defun build-txt-record (properties)
  (flet ((concat (&rest args)
	   (apply #'concatenate 'vector args)))
    (reduce #'concat
	    (mapcar #'(lambda (property)
			(let ((sub-record (build-property-sub-record property)))
			  (assert (<= (length sub-record) 255))
			  (concatenate 'vector
				       (vector (length sub-record))
				       sub-record)))
		    properties))))



(defparameter *key-value-separator* 61) ;; #\= in ASCII.

(defun build-property-sub-record (property)
  (let ((key (car property))
	(value (cdr property)))
    (if (null value)
	(string-to-bytes key)
	(if (null value)
	    (string-to-bytes (format nil "~A=" key))
	    (if (stringp value)
		(string-to-bytes (format nil "~A=~A" key value))
		(concatenate 'vector
			     (string-to-bytes key)
			     (list *key-value-separator*)
			     value))))))


;; Section 6.4 of
;; http://files.dns-sd.org/draft-cheshire-dnsext-dns-sd.txt says the
;; following:
;;
;;  * "Strings beginning with an '=' character (i.e. the name is
;;    missing) SHOULD be silently ignored"
;;
;;  * "Case is ignored when interpreting a name, so 'papersize=A4',
;;   'PAPERSIZE=A4' and 'Papersize=A4' are all identical."
;;
;;  * "Unless specified otherwise by a particular DNS-SD profile, a
;;    given attribute name may appear at most once in a TXT record. If
;;    a client receives a TXT record containing the same attribute
;;    name more than once, then the client SHOULD silently ignore all
;;    but the first occurrence of that attribute."

(defun parse-txt-record (record)
  "Parses a Zeroconf-style TXT record (see
  <http://www.zeroconf.org/Rendezvous/txtrecords.html>) into an
  association list of \(KEY . VALUE) pairs.  TXT record strings
  of the form \"KEY\" result in a \(KEY . NIL) pair, strings of
  the form \"KEY=\" result in a \(KEY . <empty vector>) pair,
  and strings of the form \"KEY=VALUE\" result in a \(KEY .
  VALUE) pair.  KEY is always a string, and VALUE, if present, is
  a vector with elements of type \(unsigned-byte 8) \(Zeroconf
  TXT records can contain binary data)."
  (let ((properties '()))
    (map-txt-record #'(lambda (key value)
			(push (cons key value) properties))
		    record)
    (reverse properties)))

(defun map-txt-record (fn record)
  (labels ((safe-min (a b)
	     (cond ((null a) b)
		   ((null b) a)
		   (T (min a b))))
	   (parse (pos)
	     (if (>= pos (length record))
		 (values)
		 (let* ((len (elt record pos)))
		   (when (> len 0)
		     (let ((key-end-pos (position *key-value-separator* record :start (+ pos 1))))
		       (let ((key (bytes-to-string record :start (+ pos 1) :end (safe-min key-end-pos (+ pos 1 len))))
			     (value (if key-end-pos
					(subseq record (+ key-end-pos 1) (+ pos len 1))
					nil)))
			 (funcall fn key value))))
		   (parse (+ pos len 1))))))
    (parse 0)))


(defun bytes-to-string (sequence &key (start 0) (end (length sequence)))
  "Converts a sequence of bytes (unsigned-byte 8) to a string using
   the implementation's default character encoding."
  (let ((s (make-string (- end start))))
    (dotimes (i (- end start))
      (setf (char s i) (code-char (elt sequence (+ i start)))))
    s))


(defun string-to-bytes (string)
  "Converts a string to a sequence of bytes (unsigned-byte 8) using
   the implementation's default character encoding."
  (let ((s (make-array (list (length string)))))
    (dotimes (i (length string))
      (setf (elt s i) (char-code (char string i))))
    s))


(defun txt-record-to-lisp-array (txt-record-ptr txt-record-len)
  (let ((a (make-array (list txt-record-len))))
    (dotimes (i txt-record-len)
      (setf (aref a i) (uffi:deref-array txt-record-ptr '(:array :unsigned-char) i)))
    a))


;; Port #'s are specified in network byte order!
