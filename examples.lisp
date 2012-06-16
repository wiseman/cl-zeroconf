;; This will advertise for an HTTP server on the local machine, at
;; port 80:

(dns-sd:publish
 (make-instance 'dns-sd:service
		:name "My CLiki"
		:type "_http._tcp"
		:port 80
		:txt-record (dns-sd:build-txt-record
			     '(("path" . "/"))))
 nil)

(dns-sd:process-dns-sd-events 3.0)


;; This will advertise an HTTP server that is actually on another
;; machine:

(dns-sd:publish
 (make-instance 'dns-sd:service
		:name "Their CLiki"
		:type "_http._tcp"
		:host "someone-else.my-local-domain"
		:port 80
		:txt-record (dns-sd:build-txt-record
			     '(("path" . "/LISP-ROCKS"))))
 nil)

(dns-sd:process-dns-sd-events 3.0)


;; Here's how you cancel a service:

(let* ((txt-record (dns-sd:build-txt-record '(("path" . "/CLiki"))))
       (service (make-instance 'dns-sd:service
			      :name "Their CLiki"
			      :type "_http._tcp"
			      :host "someone-else.my-local-domain"
			      :port 80
			      :txt-record txt-record)))
  (dns-sd:publish service nil)
  (dns-sd:process-dns-sd-events 3.0)
  (sleep 5)
  ;; At some point later in time...
  (dns-sd:cancel service)
  (dns-sd:process-dns-sd-events 3.0))


;; If you absolutely can't live with Bonjour's default handling of
;; service name conflicts (which is to rename a service, usually by
;; appending a number to the end of the name):

(dns-sd:publish (make-instance 'dns-sd:service
			       :name "Picky Service"
			       :type "_daap._tcp"
			       :port 3431)
		nil
		:if-exists :error)

(dns-sd:process-dns-sd-events 3.0)


;; If you can live with renaming, but you'd like to find out what new
;; name Bonjour gives your service:

(defclass my-observer ()
  ())

(defmethod dns-sd:publish-success ((self my-observer) service name)
  (format T "~&Service ~S was published with name ~S." service name))

(defmethod dns-sd:publish-error ((self my-observer) service error)
  (format T "~&The following error occurred while publishing ~S: ~S"
	  service error))

(dns-sd:publish (make-instance 'dns-sd:service
			       :name "Not So Picky Service"
			       :type "_daap._tcp"
			       :port 3431)
		(make-instance 'my-observer))

(dns-sd:process-dns-sd-events 3.0)


;; Browsing for services:

(defclass my-observer ()
  ())

(defmethod dns-sd:browse-add-service ((self my-observer) service interface-index &key more-coming-p)
  (declare (ignore interface-index more-coming-p))
  (format T "~&Found service ~S." service))

(defmethod dns-sd:browse-remove-service ((self my-observer) service &key more-coming-p)
  (declare (ignore more-coming-p))
  (format T "~&Service ~S went away." service))

(defmethod dns-sd:browse-error ((self my-observer) error)
  (format T "~&The following error occurred while browsing: ~S" error))

;; Look for iTunes servers.
(let ((browser (dns-sd:browse "_daap._tcp" nil (make-instance 'my-observer))))
  (dotimes (i 10)
    (dns-sd:process-dns-sd-events 3.0))
  (dns-sd:cancel browser))


;; Browsing *and* resolving services:

(defclass my-observer ()
  ())

(defmethod dns-sd:browse-add-service ((self my-observer) service interface-index &key more-coming-p)
  (declare (ignore interface-index more-coming-p))
  (format T "~&Found service ~S." service)
  ;; Now resolve it.
  (dns-sd:resolve service self))

(defmethod dns-sd:browse-remove-service ((self my-observer) service &key more-coming-p)
  (declare (ignore more-coming-p))
  (format T "~&Service ~S went away." service))

(defmethod dns-sd:browse-resolved-service ((self my-observer) service)
  (format T "~&Resolved service ~S." service))

(defmethod dns-sd:browse-error ((self my-observer) error)
  (format T "~&The following error occurred while browsing: ~S" error))

;; Look for iTunes servers.
(dns-sd:browse "_daap._tcp" nil (make-instance 'my-observer))
(dotimes (i 10)
  (dns-sd:process-dns-sd-events 3.0))


;; For more examples, see test-browse.lisp
