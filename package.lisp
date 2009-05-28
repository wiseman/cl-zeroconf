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
;;; This is the package definition.

(cl:defpackage "DNS-SD"
  (:use #:common-lisp #+sbcl #:sb-alien)
  (:export #:publish
	   #:browse
	   #:resolve
	   #:cancel

	   #:service
	   #:service-name
	   #:service-type
	   #:service-port
	   #:service-domain
	   #:service-host
	   #:service-hostname
	   #:service-txt-record

	   #:publish-success
	   #:publish-error

	   #:browser

	   #:browse-add-service
	   #:browse-remove-service
	   #:browse-error
	   #:browse-resolved-service

	   #:build-txt-record
	   #:parse-txt-record

	   #:process-dns-sd-events
	   
	   #:dns-sd-error

	   #:dns-sd-result-error
	   #:dns-sd-no-memory-error
	   #:dns-sd-not-initialized-error
	   #:dns-sd-incompatible-error
	   #:dns-sd-no-such-name-error
	   #:dns-sd-unsupported-error
	   #:dns-sd-unknown-error
	   #:dns-sd-bad-flags-error
	   #:dns-sd-invalid-error
	   #:dns-sd-bad-state-error
	   #:dns-sd-name-conflict-error
	   #:dns-sd-bad-reference-error
	   #:dns-sd-already-registered-error
	   #:dns-sd-bad-param-error
	   #:dns-sd-bad-interface-index-error

	   #:service-already-published-error
	   #:service-not-published-error
	   #:browser-already-canceled-error
	   #:socket-fd-error))

