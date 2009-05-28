(in-package :uffi)


(export 'defcallback)

#+allegro
(defun callback-definer (name args return-type body)
  (let ((internal-name (intern (format nil "%~A" name))))
    `(progn
      (ff:defun-foreign-callable ,internal-name ,args
	,@body)
      (defparameter ,name (ff:register-foreign-callable ,internal-name)))))


#+allegro
(defun process-uffi-args (args)
  args)


#+sbcl
(defun callback-definer (name args return-type body)
  (let ((int-name (intern (format nil "%~A" (symbol-name name)))))
    `(progn
       (sb-alien:define-alien-function ,int-name (,return-type ,@args) ,@body)
      (defparameter ,name (sb-alien:alien-function-sap ,int-name)))))

#+sbcl
(defun process-uffi-args (args)
  args)


#+openmcl
(defun callback-definer (name args return-type body)
  `(ccl:defcallback ,name ,(append args (list return-type))
    ,@body))

#+openmcl
(defun process-uffi-args (args)
  (reduce #'append (mapcar #'reverse args)))


(defmacro defcallback (name args return-type &body body)
  (callback-definer name (process-uffi-args args) return-type body))

