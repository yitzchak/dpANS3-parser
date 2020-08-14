(defpackage #:dpANS3-parser
  (:use :cl)
  (:export
    #:define-tex-macro
    #:interpreter
    #:interpreter-output-stack
    #:interpreter-token-stack
    #:peek-token
    #:pop-token
    #:push-output-stack
    #:pop-output-stack
    #:evaluate-token
    #:evaluate
    #:freeze-tex-macro
    #:thaw-tex-macro))


(defpackage #:dpANS3-parser/core
  (:use :cl :dpANS3-parser)
  (:export
    #:|beginchapter|
    #:|def|))
