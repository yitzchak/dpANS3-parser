(ql:quickload :dpans3-parser)

(defparameter z (make-instance 'dpans3-parser:interpreter))

(dpans3-parser::tex-input z "dpANS3/chap-1.tex")


