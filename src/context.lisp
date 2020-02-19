(in-package :stegos/node-api)

;;;
;;; Finite queue
;;;

(defvar *default-finite-queue-max-size* 1024
  "The default maximum size for a finite-queue.")

(defclass finite-queue (mp:queue)
  ((count :documentation "The current size of the queue."
          :initform 0
	        :accessor queue-count)
   (max-size :documentation "The maximum size of the queue."
             :initform *default-finite-queue-max-size*
	           :accessor queue-max-size))
  (:documentation "A mp:queue with finite size."))

(define-condition queue-full (simple-error)
  ((queue :initarg :queue
          :reader queue-full-queue))
  (:report (lambda (condition stream)
  	         (let ((queue (queue-full-queue condition)))
               (format stream 
               	       "~A is at it's maximum size of ~D."
  		                 queue
                       (queue-max-size queue))))))

(defmethod mp:enqueue :around ((queue finite-queue) what)
  (declare (ignorable what))
  (when (>= (queue-count queue) (queue-max-size queue))
    (error 'queue-full :queue queue))
  (incf-atomic (slot-value queue 'count) 1)
  (call-next-method))

(defmethod mp:dequeue :around ((queue finite-queue) &key wait empty-queue-results)
  (declare (ignorable wait empty-queue-results))
  (let ((item (call-next-method)))
    (decf-atomic (slot-value queue 'count) 1)
    item))

;;;
;;; Client
;;;

(defvar *endpoint* "ws://localhost:3145/")

(defun load-api-token ()
  (let* ((path #+macosx #P"~/Library/Application Support/")
         (filename (merge-pathnames #P"stegos/api.token" path)))
    (with-open-file (stream filename)
      (let ((contents (make-string (file-length stream))))
        (read-sequence contents stream)
        contents))
    ))

(defun decrypt (token msg)
  (let* ((decoded (base64-string-to-usb8-array msg))
         (nonce (make-array 16
                            :element-type '(unsigned-byte 8)
                            :displaced-to decoded))
         (data (make-array (- (length decoded) 16)
                           :element-type '(unsigned-byte 8)
                           :displaced-to decoded
                           :displaced-index-offset 16))
         (cipher (crypto:make-cipher :aes
                                     :key token
                                     :mode :ctr
                                     :initialization-vector nonce)))
    (crypto:decrypt-in-place cipher decoded :start 16)
    (with-underlying-simple-vector (data v)
      ;;(format *debug-io* "~&decrypt: ~A~%" (octets-to-string v :start 16))
      (jsown:parse (octets-to-string v :start 16))
      )))

(defun encrypt (token msg)
  (let* ((nonce (crypto:random-data 16))
         (data (make-array (+ (length msg) 16)
                           :element-type '(unsigned-byte 8)))
         (data (replace data nonce))
         (data (replace data (string-to-octets msg) :start1 16))
         (cipher (crypto:make-cipher :aes
                                     :key token
                                     :mode :ctr
                                     :initialization-vector nonce
                                     )))
    (crypto:encrypt-in-place cipher data :start 16)
    (usb8-array-to-base64-string data :wrap-at-column nil)
    ))

(defclass context  ()
  ((token :initarg :token :reader token)
   (websocket :initarg :websocket :reader websocket)
   (queue :initarg :queue :reader queue)
   (message-filters :initform nil :accessor message-filters)
   ))

(defmethod filter ((self context) &rest kind)
  (dolist (k kind)
    (pushnew k (message-filters self))
    ))

(defun create-context (&optional (endpoint *endpoint*))
  (let* ((token (base64-string-to-usb8-array (load-api-token)))
         (q (make-instance 'finite-queue))
         (ws (open-websocket endpoint
                             :on-message (lambda (contract data ext)
                                           (declare (ignore contract ext))
                                           (let ((msg (decrypt token data)))
                                             (mp:enqueue q (from-json msg))
                                             ))
                             :on-close (lambda (contract code data)
                                         (declare (ignore contract))
                                         (format *debug-io* "~&CLOSED with code ~A ~A~%" code data))
                             :on-error (lambda (contract)
                                         (format *debug-io* "~&ERROR: ~A~%" contract))
                             :debug t
                             )))
    (make-instance 'context :token token
                            :websocket ws
                            :queue q)
    ))

(defmethod destroy-context ((self context))
  (close-websocket (websocket self)))

(defmethod send ((self context) js)
  (let* ((msg (jsown:to-json (to-json js)))
         (encrypted (encrypt (token self) msg)))
    (websocket-send (websocket self) encrypted)))

(defun millisecond-timestamp ()
  (let ((stamp (local-time:now)))
    (+ (* (local-time:timestamp-to-unix stamp) 1000)
       (local-time:timestamp-millisecond stamp))))

