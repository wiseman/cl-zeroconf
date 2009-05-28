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
;;; This is the ASDF system definition.

(in-package :asdf)

(defsystem #:cl-zeroconf
    :name "CL-ZEROCONF"
    :author "John Wiseman <jjwiseman@yahoo.com>"
    :maintainer "John Wiseman <jjwiseman@yahoo.com>"
    :version "0.2"
    :licence "MIT"
    :description ""
    :long-description ""

    :depends-on (:uffi)
    
    :components ((:file "package")
		 (:file "mdns-ffi"    :depends-on ("package"))
		 (:file "conditions"  :depends-on ("package"))
		 (:file "sysdeps"     :depends-on ("package" "mdns-ffi"))
		 (:file "bonjour-apple-mdns" :depends-on ("mdns-ffi" "conditions" "sysdeps"))))

(defmethod perform ((o test-op) (c (eql (find-system '#:cl-zeroconf))))
  (oos 'load-op '#:cl-zeroconf-tests)
  (oos 'test-op '#:cl-zeroconf-tests :force t))



(defsystem #:cl-zeroconf-tests
  :depends-on (#:cl-zeroconf)
  :components ((:file "unit-test")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system '#:cl-zeroconf-tests))))
  (or (funcall (intern (symbol-name '#:run-tests)
                       (find-package '#:dns-sd-test)))
      (error "test-op failed")))
