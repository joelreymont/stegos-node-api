(in-package :stegos/node-api)

(defun read-output (js-obj)
  (unless (jsown:val-safe js-obj "rvalue")
    (setf (jsown:val js-obj "rvalue") nil))
  (a:switch ((jsown:val js-obj "output_type") :test #'equal)
    ("payment" (read-payment-output js-obj))
    ("stake" (read-stake-output js-obj))))

(defobject api-error ()
  ((seq "id")
   (error "error"))
  (:js-type "error" :parse t))

(defobject get-accounts ()
  ((seq "id"))
  (:js-type "accounts_info"))

(defobject account-keys ()
  ((pkey "account_pkey")
   (network-pkey "network_pkey")
   ))

;;; Fetch account list

(defobject accounts ()
  ((seq "id")
   (accounts "accounts" (read-map #'read-account-keys)))
  (:js-type "accounts_info" :parse t))

;;; Create account

(defobject create-account ()
  ((seq "id")
   (password "password"))
  (:js-type "create_account"))

(defobject account-created ()
  ((seq "id")
   (id "account_id"))
  (:js-type "account_created" :parse t))

;;; Delete account

(defobject delete-account ()
  ((seq "id")
   (id "account_id"))
  (:js-type "delete_account" :parse t))

(defobject account-deleted ()
  ((seq "id")
   (id "account_id"))
  (:js-type "account_deleted" :parse t))

;;; Account recovery

(defobject get-passphrase ()
  ((seq "id")
   (id "account_id"))
  (:js-type "recovery_info"))

(defobject passphrase ()
  ((seq "id")
   (id "account_id")
   (passphrase "recovery")
   (last-public-id "last_public_address_id"))
  (:js-type "recovery" :parse t))

(defobject recover-account ()
  ((seq "id")
   (passphrase "recovery")
   (last-public-id "last_public_address_id")) ; optional
  (:js-type "recover_account" :parse t))

;;; Lock and unlock

(defobject unlock-account ()
  ((seq "id")
   (id "account_id")
   (password "password"))
  (:js-type "unseal"))

(defobject unlocked ()
  ((seq "id")
   (id "account_id"))
  (:js-type "unsealed" :parse t))

(defobject lock-account ()
  ((seq "id")
   (id "account_id"))
  (:js-type "seal"))

(defobject locked ()
  ((seq "id")
   (id "account_id"))
  (:js-type "sealed" :parse t))

;;; Change password 

(defobject change-password ()
  ((seq "id")
   (id "account_id")
   (password "new_password"))
  (:js-type "change_password"))

(defobject password-changed ()
  ((seq "id")
   (id "account_id"))
  (:js-type "password_changed" :parse t))

;;; Account info

(defobject get-account-info ()
  ((seq "id")
   (id "account_id"))
  (:js-type "account_info"))

(defobject account-info ()
  ((seq "id")
   (id "account_id")
   (pkey "account_pkey")
   (network_pkey "network_pkey"))
  (:js-type "account_info" :parse t))

;;; Balance info

(defobject get-balance ()
  ((seq "id")
   (id "account_id"))
  (:js-type "balance_info"))

(defobject balance-info ()
  ((total "current")
   (available "available")
   ))

(defobject account-balance ()
  ((seq "id")
   (id "account_id")
   (current "current")
   (available "available")
   (is-final? "is_final")
   (private "payment" #'read-balance-info)
   (public "public_payment" #'read-balance-info)
   (stake "stake" #'read-balance-info))
  (:js-type "balance_info" :parse t))

(defobject account-balance-change ()
  ((id "account_id")
   (current "current")
   (available "available")
   (is-final? "is_final")
   (private "payment" #'read-balance-info)
   (public "public_payment" #'read-balance-info)
   (stake "stake" #'read-balance-info))
  (:js-type "balance_changed" :parse t))

;;; UTXO information

(defobject get-utxos ()
  ((seq "id")
   (id "account_id"))
  (:js-type "unspent_info"))

(defobject utxo ()
  ((id "output_hash")
   (amount "amount")
   (comment "comment")
   (to "recipient")
   (is-change? "is_change")
   ))

(defobject pub-utxo ()
  ((id "output_hash")
   (amount "amount")
   (to "recipient")
   ))

(defobject stake-utxo ()
  ((id "output_hash")
   (amount "amount")
   (account-pkey "account_pkey")
   (active-until "active_until_epoch")
   (is-active? "is_active")
   ))

(defobject utxos ()
  ((seq "id")
   (id "account_id")
   (private "payments" (read-array #'read-utxo))
   (public "public_payments" (read-array #'read-pub-utxo))
   (stake "stakes" (read-array #'read-stake-utxo)))
  (:js-type "unspent_info" :parse t))

;;; Public addresses

(defobject create-public-address ()
  ((seq "id")
   (id "account_id"))
  (:js-type "create_public_address"))

(defobject public-address ()
  ((seq "id")
   (id "account_id")
   (address "public_address")
   (address-id "public_address_id"))
  (:js-type "public_address_created" :parse t))

(defobject get-public-addresses ()
  ((seq "id")
   (id "account_id")
   (addresses "public_addresses" (read-map #'identity)))
  (:js-type "public_addresses_info"))

;;; Payments

(defobject create-payment-with-cert ()
  ((seq "id")
   (id "account_id")
   (to "recipient")
   (amount "amount")
   (fee "payment_fee")
   (comment "comment")
   (has-certificate? "with_certificate"))
  (:js-type "payment"))

(defobject create-payment ()
  ((seq "id")
   (id "account_id")
   (to "recipient")
   (amount "amount")
   (fee "payment_fee")
   (comment "comment"))
  (:js-type "secure_payment"))

(defobject create-public-payment ()
  ((seq "id")
   (id "account_id")
   (to "reicpient")
   (amount "amount")
   (fee "payment_fee"))
  (:js-type "public_payment"))

(defobject payment-output ()
  ((id "output_hash")
   (to "recipient")
   (amount "amount")
   (comment "comment")
   (rvalue "rvalue")
   (is-change? "is_change")
   ))

(defobject stake-output ()
  ((type "output_type")
   (id "output_hash")
   (to "recipient")
   (amount "amount")
   (comment "comment")
   ))

(defobject tx-created ()
  ((seq "id")
   (id "account_id")
   (tx-id "tx_hash")
   (status "status")
   (fee "fee")
   (outputs "outputs" (read-array #'read-output))
   (inputs "inputs" (read-array #'identity)))
  (:js-type "transaction_created" :parse t))

;;; Notifications

(defobject tx-status ()
  ((id "account_id")
   (tx-id "tx_hash")
   ;; create, accepted, rejected, prepared, committed, conflicted
   (status "status")
   (error "error"))
  (:js-type "transaction_status" :parse t))

(defobject snowball-status ()
  ((id "account_id")
   ;; pool_wait, shared_keying, committment, cloaked value, succeeded
   (status "state"))
  (:js-type "snowball_status" :parse t))

;;; Cloaking

(defobject cloak ()
  ((seq "id")
   (id "account_id")
   (fee "payment_fee"))
  (:js-type "cloak_all"))

;;; Staking

(defobject stake ()
  ((seq "id")
   (id "account_id")
   (amount "amount")
   (fee "payment_fee"))
  (:js-type "stake"))

(defobject stake-remote ()
  ((seq "id")
   (id "account_id")
   (amount "amount")
   (fee "payment_fee"))
  (:js-type "stake_remote"))

(defobject unstake ()
  ((seq "id")
   (id "account_id")
   (amount "amount")
   (fee "payment_fee"))
  (:js-type "unstake"))

(defobject unstake-all ()
  ((seq "id")
   (id "account_id"))
  (:js-type "unstake_all"))

(defobject restake-all ()
  ((seq "id")
   (id "account_id"))
  (:js-type "restake_all"))

;;; Certificate validation

(defobject validate-certificate ()
  ((seq "id")
   (utxo-id "utxo")
   (from "sender")
   (to "recipient")
   (rvalue "rvalue"))
  (:js-type "validate_certificate")) 

(defobject certificate-is-valid ()
  ((seq "id")
   (epoch "utxo")
   (added-to-block "block_hash")
   (block-is-final? "is_final")
   (amount "amount")
   (timestamp "timestamp"))
  (:js-type "certificate_valid" :parse t))

;;; Payment history

(defobject get-payment-history ()
  ((seq "id")
   (id "account_id")
   ;; 2019-08-21T13:36:30.798523887Z
   (from-time "starting_from")
   (limit "limit"))
  (:js-type "history_info"))

(defobject outgoing-tx ()
  ((id "tx_hash")
   (timestamp "timestamp")
   (fee "fee")
   (outputs "outputs" (read-array #'read-output))
   (inputs "inputs" (read-string-array))
   (status "status")
   (epoch "epoch"))
  (:js-type "outgoing" :parse t))

(defobject incoming-tx ()
  ((id "output_hash")
   (timestamp "timestamp")
   (output-type "output_type") ; public_payment
   (amount "amount")
   (comment "comment")
   (to "recipient")
   (is-change? "is_change"))
  (:js-type "incoming" :parse t))

(defobject payment-history ()
  ((seq "id")
   (id "account_id")
   (from-time "starting_from")
   (log "log" (read-obj-array)))
  (:js-type "history_info" :parse t))

;;; Node API

(defobject get-node-status ()
  ((seq "id"))
  (:js-type "status_info"))

(defobject node-status ()
  ((seq "id")
   (is-sync? "is_synchronized")
   (epoch "epoch")
   (timestamp "local_timestamp")
   (offset "offset")
   (view-change "view_change")
   (block-hash "last_block_hash")
   (macroblock-hash "last_macro_block_hash")
   (macroblock-timestamp "last_macro_block_timestamp"))
  (:js-type "status_info" :parse t))

(defobject get-subcription-status ()
  ((seq "id"))
  (:js-type "subscribe_status"))

(defobject subscription-status ()
  ((seq "id")
   (is-sync? "is_synchronized")
   (epoch "epoch")
   (timestamp "local_timestamp")
   (offset "offset")
   (view-change "view_change")
   (block-hash "last_block_hash")
   (macroblock-hash "last_macro_block_hash")
   (macroblock-timestamp "last_macro_block_timestamp"))
  (:js-type "subscribed_status" :parse t))

(defobject status-changed ()
  ((seq "id")
   (is-sync? "is_synchronized")
   (epoch "epoch")
   (timestamp "local_timestamp")
   (offset "offset")
   (view-change "view_change")
   (block-hash "last_block_hash")
   (macroblock-hash "last_macro_block_hash")
   (macroblock-timestamp "last_macro_block_timestamp"))
  (:js-type "status_changed" :parse t))

;;; Macroblock information

(defobject get-macroblock-info ()
  ((seq "id")
   (epoch "epoch"))
  (:js-type "macro_block_info"))

(defobject rand ()
  ((rand "rand")
   (proof "proof")
   ))

(defobject proof ()
  ((vcmt "vcmt")
   (proof "proof")
   ))

(defobject payload ()
  ((ag "ag")
   (ctxt "ctxt")
   ))

(defobject block-payment-output ()
  ((to "recipient")
   (hint "cloaking_hint")
   (proof "proof" #'read-proof)
   (payload "payload" #'read-payload))
  (:js-type "payment_output" :parse t)) 

(defobject block-stake-output ()
  ((to "recipient")
   (validator "validator")
   (amount "amount")
   (serial-number "serno")
   (signature "signature"))
  (:js-type "stake_output" :parse t))

(defobject validator ()
  ((network-pkey "network_pkey")
   (account-pkey "account_pkey")
   ))

(defobject validator-status ()
  ((status "status")))

(defobject awards ()
  ((budget "budget")
   (difficulty "difficulty")
   (validators "validators_activity" (read-map #'read-validator-status))
   ))

(defobject payout ()
  ((to "recipient")
   (amount "amount")
   ))

(defobject macroblock-info ()
  ((seq "id")
   (version "version")
   (prev "previous")
   (epoch "epoch")
   (view-change "view_change")
   (signed-by "pkey")
   (random "random" #'read-rand)
   (difficulty "difficulty")
   (timestamp "timestamp")
   (block-reward "block_reward")
   (activity_map "activity_map")
   (gamma "gamma")
   (input-range "inputs_range_hash")
   (n-inputs "inputs_len")
   (output-range "outputs_range_hash")
   (n-outputs "inputs_len")
   (multisig "multisig")
   (multisig-map "multisigmap")
   (inputs "inputs" (read-array #'identity))
   (outputs "outputs" (read-obj-array))
   (validators "validators" (read-array #'read-validator))
   (facilitator "facilitator")
   (awards "awards" #'read-awards)
   (payout "payout" #'read-payout))
  (:js-type "macro_block_info" :parse t))

;;; Microblock information

(defobject get-microblock-info ()
  ((seq "id")
   (epoch "epoch")
   (offset "offset"))
  (:js-type "micro_block_info"))

(defobject coinbase-tx ()
  ((reward "block_reward")
   (fee "block_fee")
   (gamma "gamma")
   (outputs "txouts" (read-obj-array)))
  (:js-type "coinbase_transaction" :parse t))

(defobject microblock-info ()
  ((seq "id")
   (version "version")
   (prev "previous")
   (epoch "epoch")
   (offset "offset")
   (view-change "view_change")
   (view-change-proof "view_change_proof")
   (signed-by "pkey")
   (random "random" #'read-rand)
   (solution "solution")
   (timestamp "timestamp")
   (tx-range "transactions_range_hash")
   (signature "sig")
   (transactions "transactions" (read-obj-array)))
  (:js-type "micro_block_info" :parse t))

;;; Notifications

(defobject subscribe-blockchain ()
  ((seq "id")
   (epoch "epoch")
   (offset "offset"))
  (:js-type "subscribe_chain"))

(defobject blockchain-sub-info ()
  ((seq "id")
   (epoch "current_epoch")
   (offset "current_offset"))
  (:js-type "subscribed_chain" :parse t))

(defobject macroblock-committed ()
  ((seq "id")
   (version "version")
   (prev "previous")
   (epoch "epoch")
   (view-change "view_change")
   (signed-by "pkey")
   (random "random" #'read-rand)
   (difficulty "difficulty")
   (timestamp "timestamp")
   (block-reward "block_reward")
   (activity_map "activity_map")
   (gamma "gamma")
   (input-range "inputs_range_hash")
   (n-inputs "inputs_len")
   (output-range "outputs_range_hash")
   (n-outputs "inputs_len")
   (multisig "multisig")
   (multisig-map "multisigmap")
   (inputs "inputs" (read-array #'identity))
   (outputs "outputs" (read-obj-array))
   (validators "validators" (read-array #'read-validator))
   (facilitator "facilitator")
   (awards "awards" #'read-awards)
   (payout "payout" #'read-payout))
  (:js-type "macro_block_committed" :parse t))

(defobject microblock-prepared ()
  ((seq "id")
   (version "version")
   (prev "previous")
   (epoch "epoch")
   (offset "offset")
   (view-change "view_change")
   (view-change-proof "view_change_proof")
   (signed-by "pkey")
   (random "random" #'read-rand)
   (solution "solution")
   (timestamp "timestamp")
   (tx-range "transactions_range_hash")
   (signature "sig")
   (transactions "transactions" (read-obj-array)))
  (:js-type "micro_block_prepared" :parse t))

(defobject microblock-reverted ()
  ((seq "id")
   (version "version")
   (prev "previous")
   (epoch "epoch")
   (offset "offset")
   (view-change "view_change")
   (view-change-proof "view_change_proof")
   (signed-by "pkey")
   (random "random" #'read-rand)
   (solution "solution")
   (timestamp "timestamp")
   (tx-range "transactions_range_hash")
   (signature "sig")
   (transactions "transactions" (read-obj-array)))
  (:js-type "micro_block_reverted" :parse t))

;;; Stake

(defobject validator-stake ()
  ((pkey "network_pkey")
   (active "active_stake")
   (expired "expired_stake")
   (stakes "stakes" (read-array #'read-stake-utxo))
   ))

(defobject show-stake ()
  ((seq "id"))
  (:js-type "escrow_info"))

(defobject stake-info ()
  ((seq "id")
   (validators "validators" (read-array #'read-validator-stake)))
  (:js-type "escrow_info" :parse t))

;;; Election

(defobject get-election-info ()
  ((seq "id"))
  (:js-type "election_info"))

(defobject election-info ()
  ((seq "id")
   (epoch "epoch")
   (offset "offset")
   (view-change "view_change")
   (n-slots "slots_count")
   (leader "current_leader"))
  (:js-type "election_info" :parse t))

;;; Restaking

(defobject enable-restaking ()
  ((seq "id"))
  (:js-type "enable_restaking"))

(defobject restaking-enabled ()
  ((seq "id"))
  (:js-type "restaking-enabled" :parse t))

(defobject disable-restaking ()
  ((seq "id"))
  (:js-type "disable_restaking"))

(defobject restaking-disabled ()
  ((seq "id"))
  (:js-type "restaking-disabled" :parse t))

;;; Other

(defobject internal-spent ()
  ((id "account_id")
   (tx-id "output_hash")
   (amount "amount")
   (comment "comment")
   (to "recipient")
   (is-change? "is_change"))
  (:js-type "spent" :parse t))

(defobject internal-received ()
  ((id "account_id")
   (tx-id "output_hash")
   (amount "amount")
   (comment "comment")
   (to "recipient")
   (is-change? "is_change"))
  (:js-type "received" :parse t))

(defobject internal-staked ()
  ((id "account_id")
   (tx-id "output_hash")
   (amount "amount")
   (pkey "account_pkey")
   (active-until "active_until_epoch")
   (is-active? "is_active"))
  (:js-type "staked" :parse t))

(defobject internal-unstaked ()
  ((id "account_id")
   (tx-id "output_hash")
   (amount "amount")
   (pkey "account_pkey")
   (active-until "active_until_epoch")
   (is-active? "is_active"))
  (:js-type "unstaked" :parse t))

(defobject internal-spent-public ()
  ((id "account_id")
   (tx-id "output_hash")
   (to "recipient")
   (amount "amount")
   (timestamp "pending_timestamp"))
  (:js-type "spent_public"))

(defobject internal-received-public ()
  ((id "account_id")
   (tx-id "output_hash")
   (to "recipient")
   (amount "amount")
   (timestamp "pending_timestamp"))
  (:js-type "received_public"))

