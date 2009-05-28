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
;;; This file contains the UFFI definitions for Apple's mDNS API.

(in-package "DNS-SD")

;; ----------
;; FFI definitions and utilities
;; ----------

;; If it's not OS X, then we need to load the libdns_sd library
;; manually.
#+(and unix (not darwin))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (uffi:load-foreign-library "/usr/lib/libdns_sd.so"))


;; Some convenience macros.

(defmacro def-dnssd-type (type foreign-type)
  "Defines a foreign (UFFI) type TYPE and defines a Lisp type
  corresponding to the foreign type."
  `(progn
     (uffi:def-foreign-type ,type ,foreign-type)
     (uffi:def-type ,type ,type)))

(defmacro def-dnssd-function (name args external-name)
  "Declares a foreign (UFFI) function that returns a value of
  type DNSServiceErrorType and defines a wrapper around the
  foreign function that raises an error of type
  DNS-SD-RESULT-ERROR if the foreign function returns a value
  indicating that an error occurred."
  (let ((unwrapped-name (intern (format nil "%~A" name)))
	(result-var (gensym "RESULT")))
    `(progn
       (uffi:def-function (,external-name ,unwrapped-name)
	   ,args
	 :returning dns-service-error-type)
       (defun ,name ,(mapcar #'car args)
	 (let ((,result-var (,unwrapped-name ,@(mapcar #'car args))))
	   (if (= ,result-var 0)
	       ,result-var
	       (handle-dns-sd-error ,result-var)))))))

(defun handle-dns-sd-error (code)
  (dns-sd-error code))


;; Types

;; UFFI/SBCL has some issues with void pointers.
(def-dnssd-type voidptr (* :void))

(def-dnssd-type dns-service-ref (* :void))
(def-dnssd-type dns-service-ref-ptr (* dns-service-ref))
(def-dnssd-type dns-service-flags :unsigned-long)
(def-dnssd-type dns-service-error-type :long)
(def-dnssd-type dns-service-register-reply (* :void))
(def-dnssd-type dns-service-browse-reply (* :void))
(def-dnssd-type dns-service-resolve-reply (* :void))
(def-dnssd-type dns-service-query-record-reply (* :void))


;; Functions

(uffi:def-function ("DNSServiceRefSockFD" %dns-service-ref-sock-fd)
    ((sd-ref dns-service-ref))
  :returning :long)

;; A wrapper around DNSServiceRefSockFD that raises a SOCKET-FD-ERROR
;; if the returned value is -1.  It seems that if the mDNS daemon
;; isn't running, the only indication we get is a -1 when we call
;; DNSServiceRefSockFD.

(defun dns-service-ref-sock-fd (ref)
  (let ((sock (%dns-service-ref-sock-fd ref)))
    (if (eql sock -1)
	(error 'socket-fd-error :oid ref)
	sock)))

(uffi:def-function ("DNSServiceRefDeallocate" dns-service-ref-deallocate)
    ((sd-ref dns-service-ref))
  :returning :void)

(def-dnssd-function dns-service-process-result ((sd-ref dns-service-ref))
  "DNSServiceProcessResult")

(def-dnssd-function dns-service-browse ((sd-ref-ptr (* dns-service-ref))
					(flags dns-service-flags)
					(interface-index :unsigned-long)
					(reg-type :cstring)
					(domain :cstring)
					(callback (* dns-service-browse-reply))
					(context voidptr))
  "DNSServiceBrowse")

(def-dnssd-function dns-service-resolve ((sd-ref-ptr (* dns-service-ref))
					  (flags dns-service-flags)
					  (interface-index :unsigned-long)
					  (name :cstring)
					  (reg-type :cstring)
					  (domain :cstring)
					  (callback (* dns-service-resolve-reply))
					  (context voidptr))
  "DNSServiceResolve")

(def-dnssd-function dns-service-query-record ((sd-ref-ptr (* dns-service-ref))
					  (flags dns-service-flags)
					  (interface-index :unsigned-long)
					  (full-name :cstring)
					  (rrtype :unsigned-short)
					  (rrclass :unsigned-short)
					  (callback (* dns-service-query-record-reply))
					  (context voidptr))
  "DNSServiceQueryRecord")

(def-dnssd-function dns-service-register ((sd-ref-ptr (* dns-service-ref))
					  (flags dns-service-flags)
					  (interface-index :unsigned-long)
					  (name :cstring)
					  (reg-type :cstring)
					  (domain :cstring)
					  (host :cstring)
					  (port :unsigned-short)
					  (txt-len :unsigned-short)
					  (txt-record (* :unsigned-char))
					  (callback (* dns-service-register-reply))
					  (context voidptr))
  "DNSServiceRegister")


;; Constants

(uffi:def-constant dns-service-err-no-err 0)
(uffi:def-constant dns-service-err-unknown -65537)
(uffi:def-constant dns-service-err-no-such-name -65538)
(uffi:def-constant dns-service-err-no-memory -65539)
(uffi:def-constant dns-service-err-bad-param -65540)
(uffi:def-constant dns-service-err-bad-reference -65541)
(uffi:def-constant dns-service-err-bad-state -65542)
(uffi:def-constant dns-service-err-bad-flags -65543)
(uffi:def-constant dns-service-err-unsupported -65544)
(uffi:def-constant dns-service-err-not-initialized -65545)
(uffi:def-constant dns-service-err-already-registered -65547)
(uffi:def-constant dns-service-err-name-conflict -65548)
(uffi:def-constant dns-service-err-invalid -65549)
(uffi:def-constant dns-service-err-incompatible -65551)
(uffi:def-constant dns-service-err-bad-interface-index -65552)

(uffi:def-constant dns-service-flags-more-coming  1)
(uffi:def-constant dns-service-flags-finished  0)
(uffi:def-constant dns-service-flags-add  2)
(uffi:def-constant dns-service-flags-default  4)
(uffi:def-constant dns-service-flags-remove  0)
(uffi:def-constant dns-service-flags-no-auto-rename  8)
(uffi:def-constant dns-service-flags-auto-rename  0)
(uffi:def-constant dns-service-flags-shared  16)
(uffi:def-constant dns-service-flags-unique  32)
(uffi:def-constant dns-service-flags-browse-domains  64)
(uffi:def-constant dns-service-flags-registration-domains  128)


