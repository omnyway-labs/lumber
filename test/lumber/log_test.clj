(ns lumber.log-test
  (:require
   [clojure.test :refer :all]
   [lumber.log :as log]))

(use-fixtures :each
  (fn [f]
    (log/set-level! :info)
    (f)))

(defmacro with-log-atom [name & body]
  `(let [~name (atom [])]
     (with-redefs [log/printer (fn [& args#]
                                 (swap! ~name conj args#)
                                 nil)]
       ~@body)))

(deftest basic
  (with-log-atom logs
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
               (count @logs)))))))

(deftest log-errors
  (with-log-atom logs
    (is (= 1
           (do
             (log/log-errors
              (.get [] 0))
             (count @logs))))

    (is (thrown?
         java.lang.IndexOutOfBoundsException
         (log/log-and-rethrow-errors
          (.get [] 0))))))
