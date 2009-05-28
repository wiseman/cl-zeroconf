;; To try this, load Araneida, load this file, call START-SERVER, then
;; call DNS-SD:PROCESS-DNS-SD-EVENTS a few times.  Then visit
;; <http://127.0.0.1:8000/zeroconf>


(defvar *active-services* '())



(defclass browse-observer ()
  ())

(defmethod dns-sd:browse-add-service ((self browse-observer) service &key more-coming-p)
  (declare (ignore more-coming-p))
  (format T "~&Add service: ~S~%" service)
  (dns-sd:resolve service self))

(defmethod dns-sd:browse-remove-service ((self browse-observer) service &key more-coming-p)
  (declare (ignore more-coming-p))
  (format T "~&Remove service: ~S~%" service))

(defmethod dns-sd:browse-resolved-service ((self browse-observer) service)
  (format T "~&Resolved service: ~S~%" service))


(defun test-browse (type)
  (let ((observer (make-instance 'browse-observer)))
    (dns-sd:browse type nil observer)))


(defclass web-observer (browse-observer)
  ())

(defmethod dns-sd:browse-add-service :around ((self web-observer) service &key more-coming-p)
  (call-next-method)
  (push service *active-services*))

(defmethod dns-sd:browse-remove-service ((self web-observer) service &key more-coming-p)
  (call-next-method)
  (setf *active-services* (remove service *active-services* :count 1)))


(defvar *listener* nil)

(defun start-server ()
  (full-browse)
  (setf *listener* (make-instance 'araneida:threaded-http-listener
				  :port 8000))
  (araneida:install-handler (araneida:http-listener-handler *listener*)
			    (make-instance 'zeroconf-browser-handler)
			    "http://127.0.0.1:8000/zeroconf"
			    NIL)
  (araneida:start-listening *listener*))


(defclass zeroconf-browser-handler (araneida:handler)
  ())

(defmethod araneida:handle-request-response ((self zeroconf-browser-handler) method request)
  (declare (ignore method))
  (araneida:request-send-headers request)
  (let ((str (araneida:request-stream request)))
    (format str "<html><head>~%<title>Zeroconf Service Browser</title>~%")
    (format str "<link  rel=\"stylesheet\" href=\"http://www.cliki.net/admin/cliki.css\"></head>~%")
    (let ((rows '()))
      (dolist (service *active-services*)
	(when (dns-sd:service-txt-record service)
	  (push `(tr
		  ((td :colspan "2"))
		  ((td :colspan "3") ,(format-txt-record (dns-sd:service-txt-record service))))
		rows))
	(push `(tr
		(td ,(dns-sd:service-name service))
		(td ,(dns-sd:service-type service))
		(td ,(dns-sd:service-domain service))
		(td ,(dns-sd:service-host service))
		(td ,(dns-sd:service-port service)))
	      rows))
      (araneida:html-stream
       str
       `(body
	 (h1 "Services Currently Being Advertised")
	 (table
	  (tr
	   ((th :align "left") "Name")
	   ((th :align "left") "Type")
	   ((th :align "left") "Domain")
	   ((th :align "left") "Address")
	   ((th :align "left") "Port"))
	  ,@rows)
	 (p)
	 ((form :action "/zeroconf")
	  ((input :type "submit" :value "Refresh"))))))))


(defun format-txt-record (record)
  (with-output-to-string (s)
    (dolist (property (dns-sd:parse-txt-record record))
      (let ((key (car property))
	    (value (cdr property)))
	(cond ((null value)
	       (format s "~A<br>" key))
	      ((= (length value) 0)
	       (format s "~A=<br>" key))
	      (T
	       (format s "~A=~A<br>" key (dns-sd::bytes-to-string value))))))))


(defparameter *types*
  '("_afpovertcp._tcp."
    "_nfs._tcp."
    "_webdav._tcp."
    "_ftp._tcp."
    "_ssh._tcp."
    "_eppc._tcp."
    "_http._tcp."
    "_tftp._tcp."
    "_telnet._tcp."
    "_printer._tcp."
    "_ipp._tcp."
    "_pdl-datastream._tcp."
    "_riousbprint._tcp."
    "_daap._tcp."
    "_dpap._tcp."
    "_ichat._tcp."
    "_presence._tcp."
    "_ica-networking._tcp."
    "_airport._tcp."
    "_xserveraid._tcp."
    "_distcc._tcp."
    "_apple-sasl._tcp."
    "_workstation._tcp."
    "_servermgr._tcp."
    ;; "_MacOSXDupSuppress._tcp."
    "_raop._tcp."))

(defun full-browse ()
  (let ((observer (make-instance 'web-observer)))
    (dolist (type *types*)
      (dns-sd:browse type nil observer))))


