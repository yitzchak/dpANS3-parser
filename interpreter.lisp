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
   (token-queue
     :accessor interpreter-token-queue
     :initform nil)
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


(defun evaluate (interpreter)
  (with-slots (tokenizer token-queue stream-stack)
  interpreter
  (prog (results tail)
  (cond
    ((typep (car token-queue) 'control-sequence)
      (get)
    ((and token-queue tail)
      (setf (cdr tail) (cons (pop token-queue) nil))
      (setq tail (cdr tail)))
    (token-queue
      ()

    ((null stream-stack)
      results)
    (t
      (let ((token (read-token tokenizer (car stream-stack))))
        (if token
          (push token token-queue)
          (pop stream-stack))))

  (unless token-queue



(defun tex-load (interpreter stream))
