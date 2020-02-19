(defpackage stegos-next/tests
  (:use :cl
        :stegos-next
        :rove
        ))
(in-package :stegos-next/tests)

(deftest test-conversion-to-hours-and-minutes 
  (testing "conversion"
    (ok (equal (stg::hours-and-minutes 180) '(0 3)))
    (ok (equal (stg::hours-and-minutes 4500) '(1 15)))))

(deftest test-conversion-to-seconds 
  (testing "seconds"
    (ok (= 60 (stg::seconds '(0 1))))
    (ok (= 4500 (stg::seconds '(1 15))))))

(deftest double-conversion 
  (testing "double"
    (ok (= 3600 (stg::seconds (stg::hours-and-minutes 3600))))
    (ok (= 1234 (stg::seconds (stg::hours-and-minutes 1234))))))
