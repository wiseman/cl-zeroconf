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
;;; This file contains implementation-specific code (for locks,
;;; select-like functionality for sockets, callbacks from foreign
;;; code).

(in-package "DNS-SD")


;; Each Lisp implementation needs to provide a MAKE-LOCK function to
;; create a mutex, and a WITH-LOCK macro to use the mutex.  The mutex
;; must be recursive.

#+lispworks
(defun make-lock (name)
  (mp:make-lock :name name))

#+lispworks
(defmacro with-lock ((lock) &body body)
  `(mp:with-lock (,lock)
     ,@body))

#+allegro
(defun make-lock (name)
  (mp:make-process-lock :name name))

#+allegro
(defmacro with-lock ((lock) &body body)
  `(mp:with-process-lock (,lock)
     ,@body))

#+sbcl
(defun make-lock (name)
  (sb-thread:make-mutex :name name))

#+sbcl
(defmacro with-lock ((lock) &body body)
  `(sb-thread:with-recursive-lock (,lock)
     ,@body))


;; Each implementation needs to define a function
;; FDS-INPUT-AVAILABLE-P which acts a little like the UNIX select(2)
;; system call.  It must take a list of file descriptors and an
;; optional timeout, and return the subset of the descriptors for
;; which input is available (or the empty list if the timeout expires
;; without any descriptor being ready for input).
;;
;; The following implementations of this function for different Lisps
;; fall into two categories: They either use the UNIX select(2) system
;; call or they poll all descriptors, sleep for a short time, then
;; loop.  The select(2) method should be more efficient, but is less
;; portable (it will probably have to be changed for Windows).

;; We need this in order to do the (ccl::syscall os::select ...) stuff.
#+openmcl
(eval-when (:load-toplevel :compile-toplevel)
  #+linuxppc-target
  (require "LINUX-SYSCALLS")
  #+darwinppc-target
  (require "DARWIN-SYSCALLS"))

;; The OpenMCL implementation uses select(2).  This code is based on
;; the CCL::FD-INPUT-AVAILABLE-P function that's internal to OpenMCL
;; and handles single file descriptors.

#+openmcl
(defun fds-input-available-p (fd-list &optional timeout)
  (if (null fd-list)
      '()
      (ccl:rletZ ((tv :timeval))
        (let ((ticks (if timeout (ceiling (* timeout ccl::*ticks-per-second*)) nil)))
	  (ccl::ticks-to-timeval ticks tv))
	(ccl:%stack-block ((infds ccl::*fd-set-size*)
			   (errfds ccl::*fd-set-size*))
	  (ccl::fd-zero infds)
	  (ccl::fd-zero errfds)
	  (dolist (fd fd-list)
	    (ccl::fd-set fd infds)
	    (ccl::fd-set fd errfds))
	  (let ((result (ccl::syscall syscalls::select
				      (1+ (reduce #'max fd-list)) infds
				      (ccl:%null-ptr) errfds
				      (if timeout tv (ccl:%null-ptr)))))
	    (cond ((eql result 0)
		   ;; The select timed out.
		   '())
		  ((and result (> result 0))
		   ;; There's activity on at least one fd.
		   (remove-if-not #'(lambda (fd)
				      (or (ccl::fd-is-set fd infds)
					  (ccl::fd-is-set fd errfds)))
				  fd-list))
		  ((eql result #.(read-from-string "#$EINTR"))
		   ;; Got an interrupt, try again.  I'm no UNIX
		   ;; expert, is this check really required?
		   (fds-input-available-p fd-list timeout))
		  (T
		   (error "select returned the error code ~S." result))))))))
		   

;; The LispWorks implementation uses the polling approach, using the
;; SOCKET-LISTEN function internal to the COMM package.

#+lispworks
(require "comm")

#+lispworks
(defun fds-input-available-p (fd-list &optional timeout)
  (if (and timeout (<= timeout 0))
      '()
      (if (null fd-list)
	  '()
	  (let ((ready-fds (remove-if-not #'comm::socket-listen fd-list)))
	    (if ready-fds
		ready-fds
		(progn
		  (sleep 0.1)
		  (fds-input-available-p fd-list (if timeout (- timeout 0.1) nil))))))))
					    
	
;; The ACL implementation uses the polling approach.

#+allegro
(defun fds-input-available-p (fd-list &optional timeout)
  (if (or (and timeout (<= timeout 0)) (null fd-list))
      '()
      (let ((ready-fds (remove-if-not #'excl:stream-listen fd-list)))
	(if ready-fds
	    ready-fds
	    (progn
	      (sleep 0.1)
	      (fds-input-available-p fd-list (if timeout (- timeout 0.1) nil)))))))


;; The SBCL version uses select(2).  This is based on some old version
;; of CMUCL's SUB-SERVE-EVENT I had lying around.

#+sbcl
(defun fds-input-available-p (fd-list &optional timeout)
  (if (null fd-list)
      '()
      (multiple-value-bind (secs usecs)
	  (sb-impl::decode-timeout (/ timeout 1000.0))
	(sb-alien:with-alien ((read-fds (sb-alien:struct sb-unix:fd-set))
			      (write-fds (sb-alien:struct sb-unix:fd-set))
			      (error-fds (sb-alien:struct sb-unix:fd-set)))
	  (sb-unix:fd-zero read-fds)
	  (sb-unix:fd-zero write-fds)
	  (sb-unix:fd-zero error-fds)
	  (dolist (fd fd-list)
	    (sb-unix:fd-set fd read-fds)
	    (sb-unix:fd-set fd error-fds))
	  (multiple-value-bind (value error)
	      (sb-unix:unix-fast-select (1+ (reduce #'max fd-list))
					(sb-alien:addr read-fds)
					(sb-alien:addr write-fds)
					(sb-alien:addr error-fds)
					secs usecs)
	    (cond ((eql value 0)
		   ;; The select timed out.
		   '())
		  ((and value (> value 0))
		   ;; There's activity on at least one fd.
		   (remove-if-not #'(lambda (fd)
				      (or (sb-unix:fd-isset fd read-fds)
					  (sb-unix:fd-isset fd error-fds)))
				  fd-list))
		  ((eql error sb-posix:eintr)
		   ;; Got an interrupt, try again.  We do need to
		   ;; check for this, right?
		   (fds-input-available-p fd-list timeout))
		  (T
		   (error "unix-fast-select returned the error code ~S." error))))))))
		 

#+openmcl
(defun make-lock (name)
  (ccl:make-lock name))

#+openmcl
(defmacro with-lock ((lock) &body body)
  `(ccl:with-lock-grabbed (,lock)
     ,@body))


;; ----------
;; Callbacks (implementation-specific)
;; ----------


;; Lispworks

#+lispworks
(fli:define-foreign-callable (%%publish-callback-trampoline :result-type :void)
			     ((oid dns-service-ref)
			      (flags dns-service-flags)
			      (error-code dns-service-error-type)
			      (name :pointer)
			      (type :pointer)
			      (domain :pointer)
			      (context :pointer))
  (publish-callback-trampoline oid flags error-code
			       (fli:convert-from-foreign-string name)
			       (fli:convert-from-foreign-string type)
			       (fli:convert-from-foreign-string domain)
			       context))

#+lispworks
(defparameter %publish-callback-trampoline
  (fli:make-pointer :symbol-name '%%publish-callback-trampoline :type 'dns-service-register-reply))

#+lispworks
(fli:define-foreign-callable (%%browse-callback-trampoline :result-type :void)
    ((oid dns-service-ref)
     (flags dns-service-flags)
     (interface-index :long :unsigned)
     (error-code dns-service-error-type)
     (name :pointer)
     (type :pointer)
     (domain :pointer)
     (context :pointer))
  (browse-callback-trampoline oid flags interface-index error-code
			      (fli:convert-from-foreign-string name)
			      (fli:convert-from-foreign-string type)
			      (fli:convert-from-foreign-string domain)
			      context))

#+lispworks
(defparameter %browse-callback-trampoline
  (fli:make-pointer :symbol-name '%%browse-callback-trampoline :type 'dns-service-browse-reply))
    

#+lispworks
(fli:define-foreign-callable (%%resolve-callback-trampoline :result-type :void)
    ((oid dns-service-ref)
     (flags dns-service-flags)
     (interface-index :long :unsigned)
     (error-code dns-service-error-type)
     (full-name :pointer)
     (host-target :pointer)
     (port :short :unsigned)
     (txt-len :short :unsigned)
     (txt-record (:pointer (:unsigned :char)))
     (context :pointer))
  (resolve-callback-trampoline oid flags interface-index error-code
			       (fli:convert-from-foreign-string full-name)
			       (fli:convert-from-foreign-string host-target)
			       port txt-len txt-record context))

#+lispworks
(defparameter %resolve-callback-trampoline
  (fli:make-pointer :symbol-name '%%resolve-callback-trampoline :type 'dns-service-resolve-reply))


;; SBCL

;;#+sbcl
;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (setf sb-alien::*values-type-okay* T))

#+sbcl
(sb-alien::define-alien-callback %%publish-callback-trampoline
    void ((oid dns-service-ref)
	  (flags dns-service-flags)
	  (error-code dns-service-error-type)
	  (name c-string)
	  (type c-string)
	  (domain c-string)
	  (context (* t)))
  (publish-callback-trampoline oid flags error-code name type domain context))

#+sbcl
(defparameter %publish-callback-trampoline %%publish-callback-trampoline)

#+sbcl
(sb-alien::define-alien-callback %%browse-callback-trampoline
    void ((oid dns-service-ref)
	  (flags dns-service-flags)
	  (interface-index unsigned-long)
	  (error-code dns-service-error-type)
	  (name c-string)
	  (type c-string)
	  (domain c-string)
	  (context (* T)))
  (browse-callback-trampoline oid flags interface-index error-code
			      name type domain context))

#+sbcl
(defparameter %browse-callback-trampoline %%browse-callback-trampoline)

#+sbcl
(sb-alien::define-alien-callback %%resolve-callback-trampoline
    void ((oid dns-service-ref)
	  (flags dns-service-flags)
	  (interface-index unsigned-long)
	  (error-code dns-service-error-type)
	  (full-name c-string)
	  (host-target c-string)
	  (port unsigned-short)
	  (txt-len unsigned-short)
	  (txt-record (* unsigned-char))
	  (context (* T)))
  (resolve-callback-trampoline oid flags interface-index error-code
			       full-name host-target port txt-len txt-record context))

#+sbcl
(defparameter %resolve-callback-trampoline %%resolve-callback-trampoline)


;; ACL

#+allegro
(ff:defun-foreign-callable %%publish-callback-trampoline ((oid dns-service-ref)
							  (flags dns-service-flags)
							  (error-code dns-service-error-type)
							  (name (* :char))
							  (type (* :char))
							  (domain (* :char))
							  (context (* :void)))
  (publish-callback-trampoline oid flags error-code
			       (excl:native-to-string name)
			       (excl:native-to-string type)
			       (excl:native-to-string domain)
			       context))

#+allegro
(defparameter %publish-callback-trampoline (ff:register-foreign-callable '%%publish-callback-trampoline))

#+allegro
(ff:defun-foreign-callable %%browse-callback-trampoline ((oid dns-service-ref)
							 (flags dns-service-flags)
							 (interface-index :unsigned-long)
							 (error-code dns-service-error-type)
							 (name (* :char))
							 (type (* :char))
							 (domain (* :char))
							 (context (* :void)))
  (browse-callback-trampoline oid flags interface-index error-code
			       (excl:native-to-string name)
			       (excl:native-to-string type)
			       (excl:native-to-string domain)
			       context))

#+allegro
(defparameter %browse-callback-trampoline (ff:register-foreign-callable '%%browse-callback-trampoline))

#+allegro
(ff:defun-foreign-callable %%resolve-callback-trampoline ((oid dns-service-ref)
							  (flags dns-service-flags)
							  (interface-index :unsigned-long)
							  (error-code dns-service-error-type)
							  (full-name (* :char))
							  (host-target (* :char))
							  (port :unsigned-short)
							  (txt-len :unsigned-short)
							  (txt-record (* :unsigned-char))
							  (context (* :void)))
  (resolve-callback-trampoline oid flags interface-index error-code
			       (excl:native-to-string full-name)
			       (excl:native-to-string host-target)
			       port txt-len txt-record context))

#+allegro
(defparameter %resolve-callback-trampoline (ff:register-foreign-callable '%%resolve-callback-trampoline))

#+allegro
(ff:defun-foreign-callable %%query-callback-trampoline ((oid dns-service-ref)
							(flags dns-service-flags)
							(interface-index :unsigned-long)
							(error-code dns-service-error-type)
							(full-name (* :char))
							(rrtype :unsigned-short)
							(rrclass :unsigned-short)
							(rdlen :unsigned-short)
							(rdata (* :void))
							(ttl :unsigned-long)
							(context (* :void)))
  (query-callback-trampoline oid flags interface-index error-code
			     (excl:native-to-string full-name)
			     rrtype rrclass rdlen rdata ttl context))

#+allegro
(defparameter %query-callback-trampoline (ff:register-foreign-callable '%%query-callback-trampoline))


;; OpenMCL

#+openmcl
(ccl:defcallback %publish-callback-trampoline (dns-service-ref oid
					       dns-service-flags flags
					       dns-service-error-type error-code
					       (:* :char) name
					       (:* :char) type
					       (:* :char) domain
					       (:* :void) context)
  (publish-callback-trampoline oid flags error-code
			       (ccl:%get-cstring name)
			       (ccl:%get-cstring type)
			       (ccl:%get-cstring domain)
			       context))

#+openmcl
(ccl:defcallback %browse-callback-trampoline (dns-service-ref oid
					      dns-service-flags flags
					      :unsigned-long interface-index
					      dns-service-error-type error-code
					      (:* :char) name
					      (:* :char) type
					      (:* :char) domain
					      (:* :void) context)
  (browse-callback-trampoline oid flags interface-index error-code
			      (ccl:%get-cstring name)
			      (ccl:%get-cstring type)
			      (ccl:%get-cstring domain)
			      context))

#+openmcl
(ccl:defcallback %resolve-callback-trampoline (dns-service-ref oid
                                               dns-service-flags flags
                                               :unsigned-long interface-index
					       dns-service-error-type error-code
                                               (:* :char) full-name
                                               (:* :char) host-target
                                               :unsigned-short port
					       :unsigned-short txt-len
					       (:* :unsigned-char) txt-record
					       (:* :void) context)
  (resolve-callback-trampoline oid flags interface-index error-code
			       (ccl:%get-cstring full-name)
			       (ccl:%get-cstring host-target)
			       port
                               txt-len txt-record context))

#+openmcl
(ccl:defcallback %query-callback-trampoline (dns-service-ref oid
					     dns-service-flags flags
					     :unsigned-long interface-index
					     dns-service-error-type error-code
					     (:* :char) full-name
					     :unsigned-short rrtype
					     :unsigned-short rrclass
					     :unsigned-short rdlen
					     (:* :void) rdata
					     :unsigned-long ttl
					     (:* :void) context)
  (query-callback-trampoline oid flags interface-index error-code
			     (ccl:%get-cstring full-name)
			     rrtype rrclass rdlen rdata ttl context))


