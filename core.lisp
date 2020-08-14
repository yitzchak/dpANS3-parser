(in-package :dpANS3-parser/core)


(defun |def| (interpreter))


(defun |beginchapter| (interpreter)
  (let ((result (list :type :chapter)))
    (setf (getf result :number) (push-output-stack interpreter))
    (evaluate-token interpreter)
    (pop-output-stack interpreter)
    (vector-push-extend result (car (interpreter-output-stack interpreter)))))

