(ns finance.core
  "Tells me how much money I spend per year vs how much money I earn per year."
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set :refer [difference]]
            [clj-time.coerce :refer [to-long]]
            [clj-time.core :refer [date-time]]
            [clj-time.format :refer [parse formatter]]))

(defn get-transactions []
  (let [lines (with-open [f (io/reader (io/resource "transactions.csv"))]
                (doall (csv/read-csv f)))
        header (first lines)
        lines (rest lines)]
    (reduce #(conj % (zipmap header %2)) [] lines)))

(defn parse-date
  "Takes a string like '1/11/2013', returns a DateTime instance."
  [date-str]
  (parse (formatter "MM/dd/yyyy") date-str))

(defn valid-date
  "Returns true if the specified date happened in 2012, false otherwise."
  [date]
  (apply < (map to-long [(date-time 2012 01 01)
                         (parse-date date)
                         (date-time 2012 12 31)])))

(defn -main [& args]
  (let [transactions (into #{}  (filter #(valid-date (% "Date")) (get-transactions)))
        income (into #{} (filter #(= "Income" (get % "Category")) transactions))
        spending (difference transactions income)]
    (prn (take 100 (map #(select-keys % ["Amount" "Description"]) income)))))
