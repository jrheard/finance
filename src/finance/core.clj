(ns finance.core
  "Tells me how much money I spend per year vs how much money I earn per year."
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn get-transactions []
  (let [lines (with-open [f (io/reader (io/resource "transactions.csv"))]
                (doall (csv/read-csv f)))
        header (first lines)
        lines (rest lines)]
    (reduce #(conj % (zipmap header %2)) [] lines)))

(defn -main [& args]
  (prn (get-transactions)))
