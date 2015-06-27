(ns clojars-stats-plotting.core
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [incanter.charts]
            [incanter.core :as incanter]))

(def stats-url "http://clojars.org/stats/")

(def data-dir "data/")

(def date-formatter (f/formatter "yyyyMMdd"))

(defn daily-files []
  (->> (slurp stats-url)
       (re-seq #"href=\"(downloads-.*)\"")
       (map second)))

(defn daily-file-exists? [filename]
  (.exists (io/file (str data-dir filename))))

(defn download-daily-file [filename]
  (->> (slurp (str stats-url filename))
       (spit (str data-dir filename))))

(defn edn-files []
  (rest (file-seq (io/file data-dir))))

(defn parse-file-date [^java.io.File file]
  (->> (.getName file)
       (re-find #"\d{8}")
       (f/parse date-formatter)))

(defn read-data []
  (reduce (fn [data edn-file]
            (assoc data (parse-file-date edn-file) (edn/read-string (slurp edn-file))))
          {}
          (edn-files)))

(defn daily-counts [artifacts]
  (let [include-artifact? (set artifacts)]
    (for [[date data] (read-data)
          [artifact stats] data
          :when (include-artifact? artifact)]
      {:date        date
       :date-millis (c/to-long date)
       :artifact    artifact
       :count       (reduce + (vals stats))})))

(defn monthly-counts [artifacts]
  (for [[{:keys [artifact date]} stats] (group-by
                                          (fn [{:keys [artifact date]}]
                                            {:artifact artifact
                                             :date     (t/date-time (t/year date) (t/month date))})
                                          (daily-counts artifacts))]
    {:date        date
     :date-millis (c/to-long date)
     :artifact    artifact
     :count       (reduce + (map :count stats))}))

(defn download-data []
  (let [f (io/file data-dir)]
    (when-not (.exists f)
      (.mkdir f)))
  (dorun (->> (daily-files)
              (remove daily-file-exists?)
              (map download-daily-file))))

(defn plot [title artifacts]
  (let [data (group-by :artifact (monthly-counts artifacts))
        chart (incanter.charts/time-series-plot :date-millis
                                                :count
                                                :data (incanter/to-dataset (get data (first artifacts)))
                                                :legend true
                                                :x-label "Month"
                                                :y-label "Downloads/month"
                                                :series-label (second (first artifacts))
                                                :title title)]

    (doseq [artifact (rest artifacts)]
      (incanter.charts/add-lines chart :date-millis
                                 :count
                                 :data (incanter/to-dataset (get data artifact))
                                 :series-label (second artifact)))
    (dotimes [i (count artifacts)]
      (incanter.charts/set-stroke chart :dataset i :width 3))
    chart))

;; Create, view & save chart

(download-data)

(def chart (plot "Download counts of Clojure SQL libraries"
                 [["korma" "korma"]
                 ["honeysql" "honeysql"]
                 ["yesql" "yesql"]
                 ["sqlingvo" "sqlingvo"]
                 #_["funcool" "suricatta"]
                 #_["oj" "oj"]]))

#_(incanter/view chart)

(incanter/save chart
               "clojure-sql.png"
               :width 640
               :height 480)
