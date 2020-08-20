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


(defclass frame ()
  ((token-stack
     :accessor frame-token-stack
     :initform nil)
   (output-sequence
     :accessor frame-output-sequence
     :initform (make-array 64 :adjustable t :fill-pointer 0))))


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
   (frame-stack
     :accessor interpreter-frame-stack
     :initform (list (make-instance 'frame)))
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
  (with-slots (tokenizer frame-stack stream-stack)
              interpreter
    (with-slots (token-stack)
                (car frame-stack)
      (prog (token)
       repeat
        (unless (or token-stack
                    (null stream-stack))
          (setq token (read-token tokenizer (car stream-stack)))
          (when token
            (push token token-stack)
            (return))
          (close (pop stream-stack))
          (go repeat))))))


(defun write-token (interpreter token)
  (vector-push-extend token (frame-output-sequence (car (interpreter-frame-stack interpreter)))))


(defun peek-token (interpreter)
  (unless (cdr (interpreter-frame-stack interpreter))
    (fill-token-sequence interpreter))
  (car (frame-token-stack (car (interpreter-frame-stack interpreter)))))


(defun pop-token (interpreter)
  (unless (cdr (interpreter-frame-stack interpreter))
    (fill-token-sequence interpreter))
  (pop (frame-token-stack (car (interpreter-frame-stack interpreter)))))


(defun push-frame-stack (interpreter)
  (let ((frame (make-instance 'frame)))
    (push (pop-token interpreter) (frame-token-stack frame))
    (push frame (interpreter-frame-stack interpreter))))


(defun pop-frame-stack (interpreter)
  (pop (interpreter-frame-stack interpreter)))


(defun evaluate-control-sequence (interpreter token)
  (let ((macro (gethash (control-sequence-value token) (interpreter-macros interpreter))))
    (when macro
      (let ((fun (cdar (tex-macro-definitions macro))))
        (when fun
          (funcall fun interpreter))))))


(defun assemble-text (text-sequence)
  (format t "~S~%" text-sequence)
  (let (result text tk)
    (dotimes (position (length text-sequence) (nreverse result))
      (setf tk (elt text-sequence position))
        (cond
          ((characterp tk)
          (unless text
            (setf text (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character))
            (push text result))
          (vector-push-extend tk text))
          (t
            (setf text nil)
            (push tk result))))))


(defun collect-text (interpreter type)
  (with-slots (output-sequence)
              (car (interpreter-frame-stack interpreter))
    (let ((pos (position-if (lambda (token)
                              (and (listp token)
                                   (eql type (getf token :type))))
                            output-sequence :from-end t)))
      (when pos
        (setf (cdr (last (elt output-sequence pos)))
              (list :text
                    (assemble-text (subseq output-sequence (1+ pos)))))
        (setf (fill-pointer output-sequence) (1+ pos))))))


(defun evaluate-token (interpreter)
  (with-slots (token-stack output-sequence)
              (car (interpreter-frame-stack interpreter))
    (do ((token (pop-token interpreter) (pop-token interpreter)))
        ((null token))
      (cond
        ((typep token 'control-sequence)
          (evaluate-control-sequence interpreter token))
        ((listp token)
          (setf token-stack (nconc token token-stack)))
        ((typep token 'comment))
        (t
          (vector-push-extend token output-sequence)
          (return))))))


(defun evaluate (interpreter)
  (with-slots (frame-stack)
              interpreter
    (with-slots (token-stack output-sequence)
                (car frame-stack)
      (do ((token (peek-token interpreter) (peek-token interpreter)))
          ((null token))
        (evaluate-token interpreter))
      (assemble-text (frame-output-sequence (pop frame-stack))))))


(defun push-and-evaluate (interpreter)
  (push-frame-stack interpreter)
  (evaluate interpreter))

(defun tex-input (interpreter path)
  (push (open path) (interpreter-stream-stack interpreter)))


(defmethod initialize-instance :after ((instance interpreter) &rest initargs &key &allow-other-keys)
  (do-external-symbols (sym 'dpANS3-parser/core)
    (when (fboundp sym)
      (define-tex-macro instance (symbol-name sym) (symbol-function sym) t))))
