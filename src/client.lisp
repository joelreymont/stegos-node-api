(in-package :stegos/node-api)

(defclass balance ()
  ((total$ :initarg :total$ :initform 0 :accessor total$)
   (available$ :initarg :available$ :initform 0 :accessor available$)
   ))

(defmethod print-object ((self balance) stream)
  (print-unreadable-object (self stream :type t)
    (with-accessors ((total$ total$)
                     (available$ available$)) 
        self
      (format stream "~A of ~A" available$ total$))))

(defclass account ()
  ((unlocked :initform nil :accessor unlocked)
   (address :initarg :address :reader address)
   (net-key :initarg :net-key :reader net-key)
   (private$ :initarg :private$ :accessor private$)
   (public$ :initarg :public$ :accessor public$)
   (stake$ :initarg :stake$ :accessor stake$)
   ))

(defclass client ()
  ((ctx :initarg :ctx :reader ctx)
   (password :initarg :password :reader password)
   (seq :initform 0 :accessor seq)
   (accounts :initform (make-hash-table :test 'equal) :accessor accounts)
   (message-filters :initform nil :accessor message-filters)
   ))

(defmethod filter ((self client) &rest kind)
  (dolist (k kind)
    (pushnew k (message-filters self))
    ))

(defun create-client (password)
  (let ((client (make-instance 'client
                               :ctx (create-context)
                               :password password
                               )))
    (filter client
            'internal-spent
            'internal-received
            'internal-staked
            'internal-unstaked
            'account-balance-change
            'internal-spent-public
            'internal-received-public)
    client
    ))

(defmethod destroy-client ((self client))
  (destroy-context (ctx self)))

(defmethod address-of ((self client) account-id)
  (address (gethash account-id (accounts self))))

(defun match-type-seq (type seq)
  (lambda (o)
    (and (slot-exists-p o 'seq)
         (= seq (slot-value o 'seq))
         (or (eq type (type-of o))
             (eq 'api-error (type-of o))
             ))))

(defun status-done-p (status)
  (member status '("accepted" "rejected" "prepared") :test 'equal))

(defun match-done-tx (id)
  (lambda (o)
    (and (eq 'tx-status (type-of o))
         (equal id (tx-status-tx-id o))
         (status-done-p (tx-status-status o))
         )))

(defvar *max-wait* 30.0)

(defmethod expect ((self client) matcher &key (debug nil) (wait *max-wait*))
  (loop for o = (mp:dequeue (queue (ctx self)) :wait wait)
        unless (member (type-of o) (message-filters self))
        when (or (null o) (funcall matcher o))
        return o do
        (when debug
          (format *debug-io* "--> ~A~%" o))
        ))

(defmethod wait-for-tx ((self client) seq &key debug)
  (let ((o (expect self (match-type-seq 'tx-created seq) :debug debug)))
    (ecase (type-of o)
      (api-error
       (values nil (api-error-error o)))
      (tx-created
       (let ((tx-id (tx-created-tx-id o))
             (status (tx-created-status o)))
         (when debug
           (format *debug-io* "wait-for-tx: tx id ~A, status ~A~%" tx-id status))
         (when (not (status-done-p status))
           (let ((o (expect self (match-done-tx tx-id) :debug debug)))
             (when debug
               (format *debug-io* "wait-for-tx: o: ~A~%" o))
             ))
         (values t tx-id)
         ))
      )))

(defmethod slurp-all ((self client) &key (debug nil))
  (expect self #'null :debug debug))

(defun slice (a start end)
  (make-array (- end start)
              :element-type (array-element-type a)
              :displaced-to a
              :displaced-index-offset start))

(defun chunk-vector (a chunk-size &key start end)
  (loop
   with low = (or start 0)
   and high = (or end (length a))
   for i from low below high by chunk-size
   for j from (+ low chunk-size) by chunk-size
   collect (slice a i (min j high))
   ))

(defmethod send ((self client) o)
  (let ((seq (incf (seq self))))
    (when (slot-boundp o 'seq)
      (setf (slot-value o 'seq) seq))
    (send (ctx self) o)
    ))

(defmethod get-chunks ((self client) account-id)
  (send self (make-get-utxos :id account-id))
  (let* ((utxos (expect self (match-type-seq 'utxos (seq self))))
         (coins (map 'vector (lambda (v) (utxo-amount v))
                     (utxos-private utxos))))
    (sort coins #'>)
    (chunk-vector coins 100)
    ))

(defmethod fetch-accounts ((self client))
  (send self (make-get-accounts))
  (let* ((o (expect self (match-type-seq 'accounts (seq self))))
         (accounts (accounts-accounts o)))
    (loop for k being the hash-keys of accounts
            using (hash-value v)
          for acc = (make-instance 'account
                                   :address (account-keys-pkey v )
                                   :net-key (account-keys-network-pkey v)
                                   :private$ (make-instance 'balance)
                                   :public$ (make-instance 'balance)
                                   :stake$ (make-instance 'balance)) do
                                     (setf (gethash k (accounts self)) acc))
    ))

(defmethod pay ((self client) account-id to amt &key (comment "") (proof t) (debug t))
  (send self (make-create-payment-with-cert :id account-id
                                            :to to
                                            :fee 1000
                                            :comment comment
                                            :amount amt
                                            :has-certificate? proof
                                            ))
  (wait-for-tx self (seq self) :debug debug))

;; (loop for o = (expect self (match-done-tx tx-id) :debug t)
;;       for status = (when o (tx-status-tx-status o))
;;       for tx-id = (when o (tx-status-tx-id o))
;;       until (or (null o) (= 0 (hash-table-count txs))) do
;;         (when debug
;;           (format *debug-io* "sweep-utxos: tx ~A: ~A~%" tx-id status))
;;         (remhash tx-id txs))

(defmethod sweep-utxos ((self client) account-id &key debug)
  (let ((address (address-of self account-id))
        (chunks (get-chunks self account-id)))
    (when debug
      (format *debug-io* "sweep-utxos: Account ~A: ~A chunks found~%" account-id (length chunks)))
    (loop for chunk in chunks
          for i from 0
          for amt = (aref chunk 0) do
            (when debug
              (format *debug-io* "sweep-utxos: Collecting #~A: ~A~%" i amt))
            ;; pay to self
            (pay self account-id address (+ amt 1) :comment "Sweep")
          )))

(defmethod update-balance ((self client) account-id)
  (let ((acc (gethash account-id (accounts self))))
    (when acc
      (send self (make-get-balance :id account-id))
      (let ((balance (expect self (match-type-seq 'account-balance (seq self)))))
        (flet ((update (to from)
                 (setf (total$ to) (balance-info-total from)
                       (available$ to) (balance-info-available from))
                 ))
          (update (private$ acc) (account-balance-private balance))
          (update (public$ acc) (account-balance-public balance))
          (update (stake$ acc) (account-balance-stake balance))
          )))))

(defmethod balance ((self client) account-id)
  (let ((acc (gethash account-id (accounts self))))
    (when acc
      (values (private$ acc) (public$ acc) (stake$ acc))
      )))

(defmethod stake-remote-all ((self client) account-id &key debug)
  (update-balance self account-id)
  (let* ((acc (gethash account-id (accounts self)))
         (available (available$ (private$ acc))))
    (send self (make-stake-remote :id account-id
                                  :amount (- available 2000)
                                  :fee 1000
                                  ))
    (multiple-value-bind (tx-id status)
        (wait-for-tx (seq self) :debug t)
      (when debug
        (format *debug-io* "stake-remote-all: tx ~A ~A~%" tx-id status))
      )))

(defmethod unlock-account ((self client) account-id)
  (send self (make-unlock-account :id account-id
                                  :password (password self)))
  (expect self (match-type-seq 'unlocked (seq self))))


;; "2020-01-01T00:00:00.00000Z"
(defmethod get-payment-history ((self client) account-id timestamp &optional (limit 1000000))
  (send self (make-get-payment-history :id account-id
                                       :from-time timestamp
                                       :limit limit
                                       ))
  (expect self (match-type-seq 'payment-history (seq self))))

