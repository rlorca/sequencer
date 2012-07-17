(ns sequencer.core)

(def current-sequence (ref 0))

(def pending-sequence (ref {}))

(defn- generate-new [client]
  (let [s (alter current-sequence inc)]
    (alter pending-sequence conj [client s])
    s))     
  
(defn next-id []
  (inc @current-sequence))

(defn create [client]
  (dosync
   (if-let [pending (get @pending-sequence client)]
     pending
     (generate-new client))))

(defn confirm [client sequential]
  (dosync
   (alter pending-sequence dissoc client)))

                                            

  



  