(in-package :dpANS3-parser)


(defclass interpreter ()
  ((level
     :accessor interpreter-level
     :initform 1)
   (tokenizer
     :reader interpreter-tokenizer
     :initform (make-instance 'tokenizer))
   (macros
     :reader interpreter-macros
     :initform (make-hash-table :test #'equal))
   (token-sequence
     :accessor interpreter-token-sequence
     :initform (make-array 32 :fill-pointer 0 :adjustable t))
   (token-position
     :accessor interpreter-token-position
     :initform 0)
   (stream-stack
     :accessor interpreter-stream-stack
     :initform nil)))


(defun def-tex-macro (interpreter control-sequence function &optional global)
  (let* ((level (if global 0 (interpreter-level interpreter)))
         (macros (gethash control-sequence (interpreter-macros interpreter)))
         (pair (assoc level macros)))
    (if pair
      (setf (cdr pair) function)
      (setf (gethash control-sequence (interpreter-macros interpreter))
            (if global
              (append macros (list (cons 0 function)))
              (acons level function macros))))))


(defun evaluate-token (interpreter)
  (incf (interpreter-token-position interpreter)))


(defun evaluate (interpreter)
  (with-slots (tokenizer token-sequence token-position stream-stack)
              interpreter
    (tagbody
     repeat
      (cond
        ((< token-position (length token-sequence))
          (evaluate-token interpreter)
          (go repeat))
        (stream-stack
          (let ((token (read-token tokenizer (car stream-stack))))
            (if token
              (vector-push-extend token token-sequence)
              (pop stream-stack)))
          (go repeat))))
    token-sequence))


(defun tex-load (interpreter stream))
