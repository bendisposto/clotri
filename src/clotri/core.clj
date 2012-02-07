(ns clotri.core
  (:gen-class))

(defn- has-space? [place n {{c place} :capacity}] (if c (<= n c) true))
(defn- tokens [place state] (let [t (state place)] (if t t 0)))
(defn- lookup [transition direction {ts direction}]  (filter #(= transition (:name %)) ts))
(defn- arrows [t net] [(lookup t :in net)  (lookup t :out net)])

(defn- weight [{w :weight}] (if w w 1))
(defn- project [{in :in out :out} target] (let [a (concat in out)] (into #{} (map (fn [{p target}] p) a))))
(defn places [x] (project x :place))
(defn transition-names [x] (project x :name))

(defn active? [t net]
  (let [[in out] (arrows t net)]
    (and (every? (fn [x]
                   (let [w (weight x)
                         p (:place x)]
                     (<= w (tokens p (:state net))))) in)
         (every? (fn [x]
                   (let [w (weight x)
                         p (:place x)]
                     (has-space? p w net))) out))))


(defn enabled [net] (filter #(active? % net) (transition-names net)))

;; assumes that the transition is active
(defn- nextstate [t net] (let [[in out] (arrows t net)
                               s (:state net)
                               si (reduce (fn [sx x] (let [w (weight x) p (:place x)] (assoc sx p (- (tokens p sx) w)))) s in)
                               so (reduce (fn [sx x] (let [w (weight x) p (:place x)] (assoc sx p (+ (tokens p sx) w)))) si out)]
                           so))

(defn fire [t net] (if (active? t net) (assoc net :state (nextstate t net)) net))

(defn add-transition [direction name place weight net] (let [c (net direction) curr (if c c {})] (assoc net direction (conj curr {:name name :place place :weight weight}))))
(defn set-capacity [place capa net] (assoc-in net [:capacity place] capa))

