(ns clotri.test.core
  (:use [clotri.core])
  (:use [clojure.test]))

;; testnet1 has two places A and B and one transition F. A has the
;; capacity 1, B has 4. The transition takes one token from A and puts
;; two tokens in B.
(def testnet1 {:capacity {"B" 4} :out [{:name "F", :place "B", :weight 2}] :in [{:name "F" :place "A"}] :state {"A" 1}})

;; overall structure like testnet1, but deadlocks for several reasons
;; (weight to high, no tokens, ...)
(def testnet1d1 {:capacity {"B" 4} :out [{:name "F", :place "B", :weight 2}] :in [{:name "F" :place "A"}] :state {}}) ; no token in A
(def testnet1d2 {:capacity {"B" 4} :out [{:name "F", :place "B", :weight 2}] :in [{:weight 2 :name "F" :place "A"}] :state {"A" 1}}) ; not enough tokens
(def testnet1d3 {:capacity {"B" 4} :out [{:name "F", :place "B", :weight 2}] :in [{:weight 2 :name "F" :place "A"}] :state {"A" 1 "B" 4}}) ; no space in B left 


(deftest active1
  (is (active? "F" testnet1) "Transition F should be active.")
  (is (= ["F"] (enabled testnet1)) "Transition F should be active."))

(deftest inactive1
  (is (not (active? "F" testnet1d1)) "Transition F should not be active." )
  (is (not (active? "F" testnet1d2)) "Transition F should not be active." )
  (is (not (active? "F" testnet1d3)) "Transition F should not be active." ))

(deftest fire-fail
  (is (= testnet1d1 (fire "F" testnet1d1)) "Firing an inactive transition has no effect")
  (is (= testnet1d2 (fire "F" testnet1d2)) "Firing an inactive transition has no effect"))

(deftest fire-succeed
  (is (= {"A" 0 "B" 2}  (:state (fire "F" testnet1))) "After firing F there shpuld be no token in A and 2 token in B"))

