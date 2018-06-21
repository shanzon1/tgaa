(ns tgaa.util.shared)

(def session (atom {:trialNum 0
                    :imageLocation "C:\\Users\\erudi\\OneDrive\\Activity Organizer\\Projects\\Active\\TAA Research\\Project Resources\\images\\unprocessed\\3\\1\\3_1_1.jpg"
                    :canPaths []}))

(defn get-config "Gets Configuration set in text file" [& qualified-path] 
  (if (= (first qualified-path) nil) 
    (read-string (slurp "tgaaConfig.edn"))
    (print "not implemented")))

(defn inc-trail[]
  "Increments session map :trialNum"
  (reset! session 
          (assoc @session :trialNum 
                 (inc 
                   (:trialNum @session)))))


(def config (get-config))