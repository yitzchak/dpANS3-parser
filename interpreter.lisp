(in-package :dpANS3-parser)


(defstruct tex-macro
  frozen
  definitions)


(defun do-define-tex-macro (macro function &optional global)
  (unless (tex-macro-frozen macro)
    (let* ((level (if global 0 (interpreter-level interpreter)))
           (pair (assoc level (tex-macro-definitions macro))))
      (if pair
        (setf (cdr pair) function)
        (setf (tex-macro-definitions macro)
              (if global
                (append (tex-macro-definitions macro) (list (cons 0 function)))
                (acons level function (tex-macro-definitions macro))))))))


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
   (token-stack
     :accessor interpreter-token-stack
     :initform nil)
   (output-stack
     :accessor interpreter-output-stack
     :initform (list (make-array 64 :adjustable t :fill-pointer 0)))
   (stream-stack
     :accessor interpreter-stream-stack
     :initform nil)))


(defun define-tex-macro (interpreter name function &optional global)
  (do-define-tex-macro
    (or (gethash name (interpreter-macros interpreter))
      (setf (gethash name (interpreter-macros interpreter))
            (make-tex-macro)))
          function global))


(defun freeze-tex-macro (interpreter name)
  (let ((macro (gethash name (interpreter-macros interpreter))))
    (if macro
      (setf (tex-macro-frozen macro) t)
      (setf (gethash name (interpreter-macros interpreter))
            (make-tex-macro :frozen t)))))


(defun thaw-tex-macro (interpreter name)
  (let ((macro (gethash control-sequence (interpreter-macros interpreter))))
    (if macro
      (setf (tex-macro-frozen macro) nil)
      (setf (gethash name (interpreter-macros interpreter))
            (make-tex-macro)))))


(defun fill-token-sequence (interpreter)
  (with-slots (tokenizer token-stack stream-stack)
              interpreter
    (prog (token)
     repeat
      (unless (or token-stack
                  (null stream-stack))
        (setq token (read-token tokenizer (car stream-stack)))
        (when token
          (push token token-stack)
          (return))
        (close (pop stream-stack))
        (go repeat)))))


(defun peek-token (interpreter)
  (fill-token-sequence interpreter)
  (car (interpreter-token-stack interpreter)))


(defun pop-token (interpreter)
  (fill-token-sequence interpreter)
  (pop (interpreter-token-stack interpreter)))


(defun push-output-stack (interpreter)
  (let ((value (make-array 64 :adjustable t :fill-pointer 0)))
    (push value (interpreter-output-stack interpreter))
    value))


(defun pop-output-stack (interpreter)
  (pop (interpreter-output-stack interpreter)))


(defun evaluate-control-sequence (interpreter token)
  (let ((macro (gethash (control-sequence-value token) (interpreter-macros interpreter))))
    (when macro
      (let ((fun (cdar (tex-macro-definitions macro))))
        (when fun
          (funcall fun interpreter))))))


(defun evaluate-token (interpreter)
  (with-slots (token-stack output-stack)
              interpreter
    (do ((token (pop-token interpreter) (pop-token interpreter)))
        ((null token))
      (cond
        ((typep token 'control-sequence)
          (evaluate-control-sequence interpreter token))
        ((listp token)
          (setf token-stack (nconc token token-stack)))
        (t
          (vector-push-extend token (car output-stack))
          (return))))))


(defun evaluate (interpreter)
  (with-slots (token-stack output-stack)
              interpreter
    (do ((token (peek-token interpreter) (peek-token interpreter)))
        ((null token))
      (evaluate-token interpreter))
    (car (last output-stack))))


(defun tex-input (interpreter path)
  (push (open path) (interpreter-stream-stack interpreter)))


(defmethod initialize-instance :after ((instance interpreter) &rest initargs &key &allow-other-keys)
  (do-external-symbols (sym 'dpANS3-parser/core)
    (when (fboundp sym)
      (define-tex-macro instance (symbol-name sym) (symbol-function sym) t))))
