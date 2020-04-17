(in-package :stegos/node-api)

;;;
;;; Client
;;;

(defvar *endpoint* "ws://localhost:3145/")

(defun load-api-token ()
  (let* ((path (sys:get-folder-path :my-appsupport))
         (filename (merge-pathnames #P"stegos/api.token" path)))
    (with-open-file (stream filename)
      (let ((contents (make-string (file-length stream))))
        (read-sequence contents stream)
        contents))
    ))

(defun decrypt (token msg)
  (let* ((decoded (base64:base64-string-to-usb8-array msg))
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
    (jsown:parse (flexi-streams:octets-to-string data))
    ))

(defun encrypt (token msg)
  (let* ((nonce (crypto:random-data 16))
         (data (make-array (+ (length msg) 16)
                           :element-type '(unsigned-byte 8)))
         (data (replace data nonce))
         (data (replace data (flexi-streams:string-to-octets msg) :start1 16))
         (cipher (crypto:make-cipher :aes
                                     :key token
                                     :mode :ctr
                                     :initialization-vector nonce
                                     )))
    (crypto:encrypt-in-place cipher data :start 16)
    (base64:usb8-array-to-base64-string data)
    ))

(defclass context ()
  ((token :initarg :token :reader token)
   (websocket :initarg :websocket :accessor websocket)
   (queue :initarg :queue :reader queue)
   (lock :initarg :lock :reader lock)
   (process-msg? :initform nil :accessor process-msg?)
   ))

(define-condition wsd::protocol-error (simple-error) ())

(defun create-context (&key (endpoint *endpoint*) (token (load-api-token)))
  (let* ((token (base64:base64-string-to-usb8-array token))
         (q (mp:make-mailbox :size 16384))
         (ctx (make-instance 'context :token token
                                      :queue q
                                      :lock (mp:make-lock :name "node-api-context-lock")))
         (ws (wsd:make-client endpoint)))
    (setf (websocket ctx) ws)
    (wsd:on :close ws
            (lambda (&key code reason)
              (format *debug-io* "Websocket closing: ~A/~A~%" code reason)
              ))
    (wsd:on :error ws
            (lambda (err)
              (format *debug-io* "Websocket error: ~S~%" err)
              ))
    (wsd:on :message ws
            (lambda (data)
              (mp:with-lock ((lock ctx) nil 0)
                (when (process-msg? ctx)
                  (let ((msg (decrypt token data)))
                    (mp:mailbox-send q (from-json msg))
                    )))))
    (wsd:start-connection ws)
    ctx
    ))

(defmethod destroy-context ((self context))
  (wsd:close-connection (websocket self)))

(defmacro with-incoming-messages ((ctx) &body body)
  `(let ((lock (lock ,ctx)))
     ;; capture messages
     (mp:with-lock (lock nil 0)
       (setf (process-msg? ,ctx) t))
     ;; process
     (unwind-protect (progn ,@body)
       ;; discard again
       (mp:with-lock (lock nil 0)
         (setf (process-msg? ,ctx) nil))
       )))

(defmethod send ((self context) js)
  (let* ((msg (jsown:to-json (to-json js)))
         (encrypted (encrypt (token self) msg)))
    (wsd:send (websocket self) encrypted)
   ))

(defun millisecond-timestamp ()
  (let ((stamp (local-time:now)))
    (+ (* (local-time:timestamp-to-unix stamp) 1000)
       (local-time:timestamp-millisecond stamp))))

