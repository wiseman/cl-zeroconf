(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-interface-dir :carbon))
  

(open-shared-library "/System/Library/Frameworks/Carbon.framework/Carbon")


(defmacro with-cf-str-1 (init-form &body body)
  (let ((cvar (gensym "CSTR"))
	(cfvar (gensym "CFSTR")))
    `(with-cstrs ((,cvar ,(second init-form)))
       (let* ((,cfvar (#_CFStringCreateWithCString (%null-ptr) ,cvar #$kCFStringEncodingUTF8))
	      (,(first init-form) ,cfvar))
	 (unwind-protect
	      (progn
		,@body)
	   (#_CFRelease ,cfvar))))))

(defmacro with-cf-strs (init-forms &body body)
  (cond ((null (cdr init-forms))
	 `(with-cf-str-1 ,(car init-forms) ,@body))
	(T
	 `(with-cf-str-1 ,(car init-forms)
	    (with-cf-strs ,(cdr init-forms)
	       ,@body)))))



(defvar *bonjour-process* nil)
(defvar *bonjour-run-loop* nil)


(defun run-loop ()
  (do ()
      (nil)
    (#_CFRunLoopRunInMode #?kCFRunLoopDefaultMode (float 1.0 1.0d0) 0)))

(defcallback timer-callback (:<CFR>un<L>oop<T>imer<R>ef timer
			     (:* :void) info)
  ;; Whee
  )


(defun make-run-loop-timer ()
  (let ((time (#_CFAbsoluteTimeGetCurrent)))
    (#_CFRunLoopTimerCreate
     (%null-ptr)
     time
     10.0d0
     0
     0
     timer-callback
     (%null-ptr))))
     

(defun ensure-run-loop ()
  (unless *bonjour-process*
    (setf *bonjour-process*
	  (process-run-function
	   "Bonjour Thread"
	   #'(lambda ()
	       (setf *bonjour-run-loop* (#_CFRunLoopGetCurrent))
	       (let ((timer (make-run-loop-timer)))
		 (#_CFRunLoopAddTimer *bonjour-run-loop*
				      timer
				      #?kCFRunLoopCommonModes))
	       (run-loop))))
    (sleep 1))
  *bonjour-run-loop*)
			

(defcallback client-callback (:<CFN>et<S>ervice<R>ef service
			     (:* :<CFS>tream<E>rror) error
			     (:* :void) info)
  (print "CLIENT CALLBACK"))


(defun register-service (name type domain port)
  (format T "~&Registering service...")
  (with-cf-strs ((cfdomain domain)
		 (cfname name)
		 (cftype type))
    (rlet ((context :<CFN>et<S>ervice<C>lient<C>ontext
		    :version 0
		    :info (%null-ptr)
		    :retain (%null-ptr)
		    :release (%null-ptr)
		    :copy<D>escription (%null-ptr)))
      (let ((service (#_CFNetServiceCreate (%null-ptr)
					   cfdomain
					   cftype
					   cfname
					   port)))
	(format T "~&Setting client.")
	(#_CFNetServiceSetClient service client-callback context)
	(format T "~&Scheduling in run loop")
	(#_CFNetServiceScheduleWithRunLoop service
					   (ensure-run-loop)
					   #?kCFRunLoopCommonModes)
	(format T "~&Registering")
	(print (#_CFNetServiceRegister service (%null-ptr)))
	(format T "~&Registered")
	service))))


(defun cancel-service (service)
  (#_CFNetServiceUnscheduleFromRunLoop service *bonjour-run-loop* #?kCFRunLoopCommonModes)
  (#_CFNetServiceSetClient service (%null-ptr) (%null-ptr))
  (#_CFNetServiceCancel service)
  (#_CFRelease service))
