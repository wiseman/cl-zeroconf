(in-package "SD")


(eval-when (:compile-toplevel :load-toplevel :execute)
  #+darwin ;; OpenMCL and SBCL on OS X
  (progn
    (let ((howl-lib-path (uffi:find-foreign-library "libhowl" '("/users/wiseman/lib/"))))
      (assert howl-lib-path (howl-lib-path) "Unable to find Howl library.")
      (uffi:load-foreign-library howl-lib-path :supporting-libraries '("c")))
    (uffi:load-foreign-library "/System/Library/Frameworks/Carbon.framework/Carbon")
    (uffi:load-foreign-library "/usr/lib/libSystem.B"))
  #+(and unix (not darwin))
  (progn
    #+sbcl
    ;; This is annoying.
    (let ((pthread-lib (probe-file "/lib/tls/libpthread.so.0")))
      (assert pthread-lib)
      (uffi:load-foreign-library pthread-lib :supporting-libraries '("c")))
    (let ((howl-lib-path (uffi:find-foreign-library "libhowl" '("/usr/local/lib/"))))
      (assert howl-lib-path (howl-lib-path) "Unable to find Howl library.")
      (uffi:load-foreign-library howl-lib-path))))


;; Constants & Enums

(uffi:def-constant sw-true 1)
(uffi:def-constant sw-false 0)

(uffi:def-constant sw-okay 0)
(uffi:def-constant sw-e-core-base #x80000000)
(uffi:def-constant sw-e-unknown (+ sw-e-core-base 1))
(uffi:def-constant sw-e-init (+ sw-e-core-base 2))
(uffi:def-constant sw-e-mem (+ sw-e-core-base 3))
(uffi:def-constant sw-e-eof (+ sw-e-core-base 4))
(uffi:def-constant sw-e-no-impl (+ sw-e-core-base 5))
(uffi:def-constant sw-e-file-locked (+ sw-e-core-base 6))
(uffi:def-constant sw-e-protocol-not-found (+ sw-e-core-base 7))

(uffi:def-constant sw-discovery-e-base 900)
(uffi:def-constant sw-discovery-e-unknown (+ sw-discovery-e-base 2))
(uffi:def-constant sw-discovery-e-no-such-name (+ sw-discovery-e-base 3))
(uffi:def-constant sw-discovery-e-no-mem (+ sw-discovery-e-base 4))
(uffi:def-constant sw-discovery-e-bad-param (+ sw-discovery-e-base 5))
(uffi:def-constant sw-discovery-e-bad-reference (+ sw-discovery-e-base 6))
(uffi:def-constant sw-discovery-e-bad-state (+ sw-discovery-e-base 7))
(uffi:def-constant sw-discovery-e-bad-flags (+ sw-discovery-e-base 8))
(uffi:def-constant sw-discovery-e-not-supported (+ sw-discovery-e-base 9))
(uffi:def-constant sw-discovery-e-not-initialized (+ sw-discovery-e-base 10))
(uffi:def-constant sw-discovery-e-no-cache (+ sw-discovery-e-base 11))
(uffi:def-constant sw-discovery-e-already-registered (+ sw-discovery-e-base 12))
(uffi:def-constant sw-discovery-e-name-conflict (+ sw-discovery-e-base 13))
(uffi:def-constant sw-discovery-e-invalid (+ sw-discovery-e-base 14))

(uffi:def-constant sw-discovery-browse-invalid 0)
(uffi:def-constant sw-discovery-browse-release 1)
(uffi:def-constant sw-discovery-browse-add-domain 2)
(uffi:def-constant sw-discovery-browse-add-default-domain 3)
(uffi:def-constant sw-discovery-browse-remove-domain 4)
(uffi:def-constant sw-discovery-browse-add-service 5)
(uffi:def-constant sw-discovery-browse-remove-service 6)

(uffi:def-constant sw-discovery-publish-started 0)
(uffi:def-constant sw-discovery-publish-stopped 1)
(uffi:def-constant sw-discovery-publish-name-collision 2)
(uffi:def-constant sw-discovery-publish-invalid 3)


(defun constant->enum (table constant)
  (let ((entry (rassoc constant table :test #'eql)))
    (if entry
	(values (car entry) T)
	(values :unknown-value NIL))))

(defparameter *browse-status*
  `((:invalid			. ,sw-discovery-browse-invalid)
    (:release			. ,sw-discovery-browse-release)
    (:add-domain		. ,sw-discovery-browse-add-domain)
    (:add-default-domain	. ,sw-discovery-browse-add-default-domain)
    (:remove-domain		. ,sw-discovery-browse-remove-domain)
    (:add-service		. ,sw-discovery-browse-add-service)
    (:remove-service		. ,sw-discovery-browse-remove-service)))

(defparameter *publish-status*
  `((:started			. ,sw-discovery-publish-started)
    (:stopped			. ,sw-discovery-publish-stopped)
    (:name-collision		. ,sw-discovery-publish-name-collision)
    (:invalid			. ,sw-discovery-publish-invalid)))

(defparameter *sw-results*
  `((:ok			. ,sw-okay)
    (:unknown-error		. ,sw-e-unknown)
    (:init-error		. ,sw-e-init)
    (:memory-error		. ,sw-e-mem)
    (:eof			. ,sw-e-eof)
    (:not-implemented		. ,sw-e-no-impl)
    (:file-locked		. ,sw-e-file-locked)
    (:protocol-not-found	. ,sw-e-protocol-not-found)

    (:discovery-unknown-error	. ,sw-discovery-e-unknown)
    (:discovery-no-such-name	. ,sw-discovery-e-no-such-name)
    (:discovery-no-memory	. ,sw-discovery-e-no-mem)
    (:discovery-bad-parameter	. ,sw-discovery-e-bad-param)
    (:discovery-bad-reference	. ,sw-discovery-e-bad-reference)
    (:discovery-bad-state	. ,sw-discovery-e-bad-state)
    (:discovery-bad-flags	. ,sw-discovery-e-bad-flags)
    (:discovery-not-supported	. ,sw-discovery-e-not-supported)
    (:discovery-not-initialized	. ,sw-discovery-e-not-initialized)
    (:discovery-no-cache	. ,sw-discovery-e-no-cache)
    (:discovery-already-registered	. ,sw-discovery-e-already-registered)
    (:discovery-name-conflict	. ,sw-discovery-e-name-conflict)
    (:discovery-invalid		. ,sw-discovery-e-invalid)))



(define-condition bad-sw-result-error (simple-error)
  ((result :initarg :result :initform nil :accessor bad-sw-result-error-result))
  (:report
   (lambda (condition stream)
     (let ((code (bad-sw-result-error-result condition)))
       (multiple-value-bind (name found-p)
	   (constant->enum *sw-results* code)
	 (if found-p
	     (format stream "Bad sw-result: #x~X (~S)" code name)
	     (format stream "Bad sw-result: #x~X" code)))))))
  


;; Types

(defmacro def-howl-type (type foreign-type)
  `(progn
    (uffi:def-foreign-type ,type ,foreign-type)
    (uffi:def-type ,type ,type)))

(uffi:def-array-pointer sw-octets :unsigned-char)
(uffi:def-type sw-octets sw-octets)

(def-howl-type sw-discovery (* :void))
(def-howl-type sw-discovery-browse-reply (* :void))
(def-howl-type sw-discovery-browse-status :int)
(def-howl-type sw-discovery-oid :unsigned-long)
(def-howl-type sw-discovery-ptr (* sw-discovery))
(def-howl-type sw-discovery-publish-id :unsigned-long)
(def-howl-type sw-discovery-publish-reply (* :void))
(def-howl-type sw-discovery-publish-status :int)
(def-howl-type sw-discovery-resolve-reply (* :void))
(def-howl-type sw-ipv4-address :unsigned-long)
(def-howl-type sw-port :unsigned-short)
(def-howl-type sw-result :int)
(def-howl-type sw-salt (* :void))
(def-howl-type sw-salt-ptr (* sw-salt))
(def-howl-type sw-text-record (* :void))

(def-howl-type voidptr (* :void))


;; Functions

(defmacro def-howl-function (name args external-name)
  (let ((unwrapped-name (intern (format nil "%~A" name)))
	(result-var (gensym "RESULT")))
    `(progn
      (uffi:def-function (,external-name ,unwrapped-name)
	  ,args
	:returning sw-result)
      (defun ,name ,(mapcar #'car args)
	(let ((,result-var (,unwrapped-name ,@(mapcar #'car args))))
	  (declare (type sw-result ,result-var))
	  (unless (= ,result-var sw-okay)
	    (error 'bad-sw-result-error :result ,result-var))
	  ,result-var)))))

(def-howl-function sw-discovery-init ((discovery (* sw-discovery)))
  "sw_discovery_init")
  
(def-howl-function sw-text-record-init ((text-record-ptr (* sw-text-record)))
  "sw_text_record_init")

(def-howl-function sw-text-record-fina ((text-record sw-text-record))
  "sw_text_record_fina")

(def-howl-function sw-text-record-add-string ((text-record sw-text-record) (string :cstring))
  "sw_text_record_add_string")

(def-howl-function sw-text-record-add-key-and-opaque-value ((text-record sw-text-record) (key :cstring) (value sw-octets) (len :unsigned-long))
  "sw_text_record_add_key_and_opaque_value")

(def-howl-function sw-text-record-add-key-and-string-value ((text-record sw-text-record) (key :cstring) (value :cstring))
  "sw_text_record_add_key_and_string_value")

(uffi:def-function "sw_text_record_bytes" ((text-record sw-text-record))
  :returning sw-octets)

(uffi:def-function "sw_text_record_len" ((text-record sw-text-record))
  :returning :unsigned-long)

(def-howl-function sw-discovery-publish ((session sw-discovery)
					 (interface-index :unsigned-long)
					 (name :cstring)
					 (type :cstring)
					 (domain :cstring)
					 (host :cstring)
					 (port sw-port)
					 (text-record sw-text-record)
					 (text-record-length :unsigned-long)
					 (reply sw-discovery-publish-reply)
					 (extra voidptr)
					 (oid-ptr (* sw-discovery-oid)))
  "sw_discovery_publish")

(def-howl-function sw-discovery-cancel ((session sw-discovery)
					(oid sw-discovery-oid))
  "sw_discovery_cancel")

(uffi:def-function "sw_discovery_run"
    ((session sw-discovery))
  :returning sw-result)

(uffi:def-function "sw_discovery_salt"
  ((session sw-discovery)
   (salt (* sw-salt)))
  :returning sw-result)

(uffi:def-function "sw_discovery_socket" ((session sw-discovery))
  :returning :int)

(def-howl-function sw-discovery-read-socket ((session sw-discovery))
  "sw_discovery_read_socket")

(uffi:def-function "sw_salt_step"
  ((salt sw-salt)
   (msecs (* :unsigned-long)))
  :returning sw-result)


(def-howl-function sw-discovery-browse ((session sw-discovery)
					(interface-index :unsigned-long)
					(type :cstring)
					(domain :cstring)
					(reply sw-discovery-browse-reply)
					(extra voidptr)
					(oid (* sw-discovery-oid)))
  "sw_discovery_browse")

(def-howl-function sw-discovery-resolve ((session sw-discovery)
					 (interface-index :unsigned-long)
					 (name :cstring)
					 (type :cstring)
					 (domain :cstring)
					 (reply sw-discovery-resolve-reply)
					 (extra voidptr)
					 (oid (* sw-discovery-oid)))
  "sw_discovery_resolve")
