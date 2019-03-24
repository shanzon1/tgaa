(ns tgaa.util.mri-converter
  (:require [tgaa.util.gui :as gui])
  (:import [javax.swing JFileChooser]
           [javax.swing.filechooser FileNameExtensionFilter FileSystemView]
           [java.io File]))

(use '[clojure.java.shell :only [sh]])

;config
(def qp "C:\\Users\\erudi\\OneDrive\\Activity Organizer\\Projects\\Active\\TAA Research\\Project Resources\\images\\")
(def conv-name "dcm2jpg.exe")
(def conv-att  [["-q" "95"] ["-z" "3"] ["-s" "yes"]])


(defn extract-mri [dir]
     (filter #(= (last (clojure.string/split (str %) #"[.]"))  "IMA") (vec (file-seq dir))))

(defn get-input-mir [] (extract-mri  (gui/sys-view-ref "Select Input Directory" :folder qp )))

(defn get-out-dir [] (gui/sys-view-ref "Select Output Directory or Cancel" :folder ))

(defn process-mri [file output]
 (let [conv-att (if (nil? output)
                  conv-att
                  (concat conv-att  ["-o" (str  output )]))
       res (apply sh (flatten [ conv-name  conv-att  file]))]
   (println "Result: "
            (if (= (:err res) "")
              "Success"
              (str "Fail" (:err res)))
            file)))

(defn process-all-mri [files output]
  (map #(process-mri (str %) output)  files))


(defn run-processor []
  (process-all-mri 
     (get-input-mir) (get-out-dir)))
    
