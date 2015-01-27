
(in-package :asdf-objects)

;;; generate-op
(defclass generate-op (asdf:downward-operation) ())

;;; generated-component for components that generate files
(defclass generated-component (asdf:component) ())

(defmethod perform ((op generate-op) (c component)))

(defmethod perform ((op generate-op) (c generated-component)))

(defmethod component-depends-on ((op compile-op) (c generated-component))
  (append (call-next-method)
          `((generate-op ,(component-name c)))))

(defmethod component-depends-on ((op load-op) (c generated-component))
  (append (call-next-method)
          `((generate-op ,(component-name c)))))

(defmethod perform :before ((operation generate-op) (c generated-component))
  (map nil #'ensure-directories-exist (output-files operation c)))

;;;
;;; generated-file - not all components will have files associated
;;; with them. for those that do, use this subclass of
;;; generated-component.
(defclass generated-file (generated-component source-file) ())

(defmethod asdf:component-relative-pathname ((component generated-file))
  (let ((relative-pathname (slot-value component 'asdf::relative-pathname)))
    (if relative-pathname
        relative-pathname
        (let ((*default-pathname-defaults*
               (asdf::component-parent-pathname component)))
          (make-pathname
           :name (component-name component))))))


(defun get-sibling-component (comp sib)
  (asdf:find-component (asdf:component-parent comp)
                       (asdf/find-system:coerce-name sib)))

(defclass object-component (generated-component)
  ((symbol :accessor object-symbol :initarg :symbol)))

(defmethod operation-done-p ((o generate-op) (c object-component))
  t)

(defmethod source-file-type ((c object-component) (s module)) nil)

(defun make-symbol-from-name (name)
  (intern (string (read-from-string name))))

(defmethod shared-initialize :after ((c object-component) slot-names
                                     &key force
                                     &allow-other-keys)
  (declare (ignore force))
  (when (slot-boundp c 'asdf::name)
    (unless (slot-boundp c 'symbol)
      (setf (object-symbol c)
            (make-symbol-from-name (asdf::component-name c))))))

(defmethod perform ((op compile-op) (c object-component)))

(defmethod perform ((op load-op) (c object-component))
  (setf (component-property c 'last-loaded)
        (get-universal-time)))

(defmethod operation-done-p ((o compile-op) (c object-component))
  t)

(defmethod operation-done-p ((o load-op) (comp object-component))
  (every #'identity
         (loop for (dep-op dep-comp) in
              (asdf:component-depends-on o comp)
              collect (asdf:operation-done-p
                       dep-op
                       (get-sibling-component comp dep-comp)))))

;;; An object-from-file is the file-based representation of an object. The
;;; load-op 
(defclass object-from-file (object-component source-file)
  ((load-date :accessor object-load-date :initarg :load-date)))

(defmethod perform ((op compile-op) (c object-from-file)))

(defmethod perform ((op load-op) (c object-from-file))
  (with-open-file (input-stream (component-pathname c))
    (setf (symbol-value (object-symbol c))
          (read input-stream))))

(defmethod perform ((op generate-op) (c object-from-file))
  (setf (asdf:component-property c 'last-generated)
        (get-universal-time)))

;;; this needs to check the file date!!!!
(defmethod operation-done-p ((o generate-op) (c object-from-file))
  (let ((on-disk-time
         (file-write-date (component-pathname c)))
        (my-last-load-time (asdf:component-property c 'last-loaded)))
    (and on-disk-time
         my-last-load-time
         (>= my-last-load-time on-disk-time))))


(defclass object-to-file (object-component)
  ((write-date :accessor object-write-date :initarg :write-date)))


(defclass object-from-variable (object-component)
  ((input-object :accessor object-input-object :initarg :input-object)))

(defmethod component-depends-on ((op generate-op) (c object-from-variable))
  (append (call-next-method)
          `((load-op , (asdf/find-system:coerce-name (object-input-object c))))))

(defmethod component-depends-on ((op compile-op) (c object-from-variable))
  (append (call-next-method)
          `((load-op ,(asdf/find-system:coerce-name (object-input-object c))))))

(defmethod operation-done-p ((o generate-op) (c object-from-variable))
  (let ((input-object-last-load-time
         (asdf:component-property
          (find-component (component-parent c)
                          (asdf/find-system:coerce-name (object-input-object c)))
          'last-loaded))
        (my-last-generate-time (asdf:component-property c 'last-generated)))
    (and input-object-last-load-time
         my-last-generate-time
         (>= my-last-generate-time input-object-last-load-time))))

(defmethod operation-done-p ((o compile-op) (c object-from-variable))
  (let ((my-last-generate-time (asdf:component-property c 'last-generated))
        (my-last-compile-time (asdf:component-property c 'last-compiled)))
    (and my-last-generate-time
         my-last-compile-time
         (>= my-last-compile-time my-last-generate-time))))

(defmethod operation-done-p ((o load-op) (c object-from-variable))
  (let ((my-last-compile-time (asdf:component-property c 'last-compiled))
        (my-last-load-time (asdf:component-property c 'last-loaded)))
    (and my-last-compile-time
         my-last-load-time
         (>= my-last-load-time my-last-compile-time))))

(defmethod perform ((op generate-op) (c object-from-variable))
  (setf (asdf:component-property c 'last-generated)
        (get-universal-time))
  (let ((sexp
       (symbol-value
        (object-symbol
         (find-component (component-parent c)
                         (asdf/find-system:coerce-name (object-input-object c)))))))
  (setf (symbol-value (object-symbol c)) sexp)))

(defmethod perform ((op compile-op) (c object-from-variable))
  (setf (asdf:component-property c 'last-compiled)
        (get-universal-time)))

(defmethod perform ((op load-op) (c object-from-variable))
    (setf (asdf:component-property c 'last-loaded)
        (get-universal-time))
    (call-next-method))

