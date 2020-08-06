(ns lumber.log-test
  (:require
   [clojure.test :refer :all]
   [lumber.log :as log]))

(use-fixtures :each
  (fn [f]
    (log/set-level! :info)
    (f)))

(deftest basic
  (let [logs (atom [])]
    (with-redefs [log/printer (fn [& args]
                                (swap! logs conj args)
                                nil)]
      (is (= 1
             (do
               (log/info "test 1")
               (count @logs))))
      (is (= 2
             (do
               (log/info "test 2")
               (count @logs))))
      (testing "Lower log level elision"
        (is (= 2
               (do
                 (log/debug "test 1")
                 (count @logs)))))

      (testing "Temporarily raise log level"
        (log/with-level :debug
          (is (= 3
                 (do
                   (log/debug "test 1")
                   (count @logs))))))

      (testing "Lower log level elision"
        (is (= 4
               (do
                 (log/set-level! :trace)
                 (log/trace "test 1")
                 (count @logs))))))))
