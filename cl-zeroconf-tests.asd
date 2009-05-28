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
;;; This is the ASDF system definition for the unit tests.

(in-package :asdf)

(asdf:operate 'asdf:load-op :cl-zeroconf)


(asdf:defsystem :cl-zeroconf-tests
    :depends-on (:cl-zeroconf)
    :components ((:file "unit-test")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :cl-zeroconf-tests))))
  (or (funcall (intern (symbol-name '#:run-tests)
                       (find-package '#:dns-sd-test)))
      (error "test-op failed")))
