(ns clotri.core
  (:require clojure.xml) 
  (:gen-class))

                                        
(defn active? [{state :state arcs :arcs} t]
  (let [a (filter (fn [{dst :dst}] (= dst t)) arcs)]
    (every? (fn [{:keys [src weight]}]
              (<= weight (state src)))
            a)))

(defn enabled [{ts :transitions :as net}] (filter (partial active? net) ts))

(defn- nextstate [{state :state arcs :arcs :as net} t]
  (let [in (filter (fn [{dst :dst}] (= dst t)) arcs)
        out (filter (fn [{src :src}] (= src t)) arcs)
        s (reduce (fn [sx {w :weight p :src}] (assoc sx p (- (sx p) w))) state in)
        ns (reduce (fn [sx {w :weight p :dst}] (assoc sx p (+ (sx p) w))) s out)]
    (assoc net :state ns)))

(defn fire [t net] (if (active? net t) (nextstate net t) net))

(def empty-net {:places [] :transitions [] :arcs [] :state {}})

(defn extract-place [{:keys [places state] :as net}
                     {{id :id} :attrs  c :content}]
  (let [m (first (filter (fn [{tag :tag}] (= :initialMarking tag)) c))
        t (first (:content m))
        v (read-string (first (:content t)))]
    (assoc net :places (conj places id) :state (assoc state id v))))

(defn extract-arc [{arcs :arcs :as net} item]
  (let [src (get-in item [:attrs :source])
        dst (get-in item [:attrs :target])
        m (first (filter (fn [{tag :tag}] (= :inscription tag)) (:content item)))
        c (first (filter (fn [{tag :tag}] (= :text tag)) (:content m)))
        n (read-string (first (:content c)))]
    (assoc net :arcs (conj arcs {:src src :dst dst :weight n}))))

(defn extract [net {tag :tag :as item}]
  (cond (= tag :place) (extract-place net item)
        (= tag :transition) (assoc net :transitions (conj (:transitions net) (get-in item [:attrs :id])))
        (= tag :arc) (extract-arc net item)
        :otherwise net))

(defn read-xml [file] (let [fx (clojure.xml/parse file)] (reduce #(extract % %2) empty-net (-> fx :content first :content))))

(def n (read-xml "nets/testnet.xml")) 



