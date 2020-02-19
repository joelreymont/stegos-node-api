(defsystem "stegos-node-api"
  :version "0.1.0"
  :author "Joel Reymont"
  :license "LLGPL"
  :depends-on ("ironclad"
               "jsown-obj"
               "local-time"
               "alexandria"
               "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "api"
                  :depends-on ("packages"))
                 (:file "context"
                  :depends-on ("packages"
                               "api"))
                 (:file "client"
                  :depends-on ("packages"
                               "api"
                               "context"))
                 )))
  :description ""
  :in-order-to ((test-op (test-op "stegos-node-api/tests"))))

(defsystem "stegos-node-api/tests"
  :author "Joel Reymont"
  :license "LLGPL"
  :depends-on ("stegos-node-api"
               "rove")
  :components ((:module "t/"
                :components
                ((:file "main"))))
  :description "Test system for stegos-node-api"
  :perform (test-op (op c) (symbol-call :rove :run c))
  )
