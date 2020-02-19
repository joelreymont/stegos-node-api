(in-package :stegos/node-api)

(defclass balance ()
  ((total$ :initarg :total$ :initform 0 :accessor total$)
   (available$ :initarg :available$ :initform 0 :accessor available$)
   ))

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

(defun create-client (password)
  (let ((client (make-instance 'client
                               :ctx (create-context)
                               :password password
                               )))
    (filter (ctx client)
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
    (and (eq type (type-of o))
         (= seq (slot-value o 'seq))
         )))

(defun status-done-p (status)
  (member status '("accepted" "rejected" "prepared") :test 'equal))

(defun match-done-tx (id)
  (lambda (o)
    (and (eq 'tx-status (type-of o))
         (equal id (tx-status-tx-id o))
         (status-done-p (tx-status-status o))
         )))

(defmethod expect ((self client) matcher &key (debug nil))
  (loop for o = (mp:dequeue (queue (ctx self)) :wait 5.0)
        when (or (null o) (funcall matcher o))
        return o do
        (when debug
          (format *debug-io* "--> ~A~%" o))
        ))

(defmethod wait-for-tx-in-mempool ((self client) seq &key debug)
  (let ((o (expect self (match-type-seq 'tx-created seq) :debug debug)))
    (when o
      (let ((tx-id (tx-created-tx-id o))
            (status (tx-created-status o)))
        (when debug
          (format *debug-io* "wait-for-tx-in-mempool: tx id ~A, status ~A~%" tx-id status))
        (when (not (status-done-p status))
          (let ((o (expect self (match-done-tx tx-id) :debug debug)))
            (when o
              (setq status (tx-status-status o)))))
        (values tx-id status)
        ))))

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

(defmethod sweep-utxos ((self client) account-id &key debug)
  (let ((address (address-of self account-id))
        (chunks (get-chunks self account-id))
        (txs (make-hash-table :test 'equal)))
    (when debug
      (format *debug-io* "sweep-utxos: Account ~A: ~A chunks found~%" account-id (length chunks)))
    (loop for chunk in chunks
          for i from 0
          for amt = (aref chunk 0) do
            (when debug
              (format *debug-io* "sweep-utxos: Collecting #~A: ~A~%" i amt))
            (send self (make-create-payment-with-cert :id account-id
                                                      :to address ; payment to self
                                                      :fee 1000
                                                      :comment "Sweep"
                                                      :amount (+ amt 1)
                                                      :has-certificate? t
                                                      ))
            (multiple-value-bind (tx-id status)
                (wait-for-tx-in-mempool self (seq self) :debug t)
              (when (equal "accepted" status)
                (setf (gethash tx-id txs) t)
                )))
    (when debug
      (format *debug-io* "sweep-utxos: Accepted ~A payments out of ~A, monitoring status...~%"
              (hash-table-count txs) (length chunks)))
    (loop for o = (expect self (match-done-tx tx-id) :debug t)
          for status = (when o (tx-status-tx-id o))
          for tx-id = (when o (tx-status-tx-id o))
          until (or (null o) (= 0 (hash-table-count txs))) do
            (when debug
              (format *debug-io* "sweep-utxos: tx ~A: ~A~%" tx-id status))
            (remhash tx-id txs))
    ))

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

(defmethod stake-remote-all ((self client) account-id &key debug)
  (update-balance self account-id)
  (let* ((acc (gethash account-id (accounts self)))
         (available (available$ (private$ acc))))
    (send self (make-stake-remote :id account-id
                                  :amount (- available 2000)
                                  :fee 1000
                                  ))
    (multiple-value-bind (tx-id status)
        (wait-for-tx-in-mempool (seq self) :debug t)
      (when debug
        (format *debug-io* "stake-remote-all: tx ~A ~A~%" tx-id status))
      )))

(defmethod unlock-account ((self client) account-id)
  (send self (make-unlock-account :id account-id
                                  :password (password self)))
  (expect self (match-type-seq 'unlocked (seq self))))

