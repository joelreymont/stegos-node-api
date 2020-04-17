(defpackage stegos/node-api
  (:use :cl
        :jsown-obj
        :base64
        )
  (:export :client :create-client :destroy-client
           :ctx :password :seq :accounts
           :fetch-accounts :address-of
           :pay :update-balance :balance :total$ :available$
           :expect
           :wait-for-tx-in-mempool
           :send
           :update-balance
           :unlock-account
           :stake-remote-all :unstake
           :get-payment-history
           :sweep-utxos
           :match-type-seq
           :match-done-tx
           )
  (:nicknames :api))

;; alias alexandria to a
(rename-package :alexandria :alexandria '(a))


