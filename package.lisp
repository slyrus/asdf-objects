
(cl:defpackage #:asdf-objects
  (:use #:cl #:asdf)
  (:export #:generate-op
           #:generated-file
           #:generated-source-file

           #:object-component
           #:object-from-variable
           #:object-input-object
           #:object-from-file
           #:object-to-file
           #:object-symbol))
