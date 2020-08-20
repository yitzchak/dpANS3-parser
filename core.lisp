(in-package :dpANS3-parser/core)


(defun |def| (interpreter))


(defun |beginchapter| (interpreter)
  (let ((result (list :type :chapter)))
    (push-frame-stack interpreter)
    (setf (getf result :number) (evaluate interpreter))
    (write-token interpreter result)))

