(ns clotri.test.core
  (:use [clotri.core])
  (:use [clojure.test]))

;; testnet1 has two places A and B and one transition F. A has the
;; capacity 1, B has 4. The transition takes one token from A and puts
;; two tokens in B.
(def testnet1  {:arcs [{:src "F", :dst "B", :weight 2} {:dst "F" :src "A" :weight 1}] :places ["A","B"] :transitions ["F"] :state {"A" 1 "B" 0}})
(def testnet1d {:arcs [{:src "F", :dst "B", :weight 2} {:dst "F" :src "A" :weight 2}] :places ["A","B"] :transitions ["F"] :state {"A" 1 "B" 0}})


(deftest active1
  (is (active? testnet1 "F") "Transition F should be active.")
  (is (= ["F"] (enabled testnet1)) "Transition F should be active."))

(deftest inactive1
  (is (not (active? testnet1d "F")) "Transition F should not be active." ))

(deftest fire-fail
  (is (= testnet1d (fire "F" testnet1d )) "Firing an inactive transition has no effect"))

(deftest fire-succeed
  (is (= {"A" 0 "B" 2}  (:state (fire "F" testnet1))) "After firing F there shpuld be no token in A and 2 token in B"))
