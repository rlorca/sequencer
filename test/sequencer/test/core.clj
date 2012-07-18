(ns sequencer.test.core
  (:use [sequencer.core])
  (:use [clojure.test]))

(deftest sequential-generation
  (is
   (= (next-id)
      (create "a"))
   "code is generated")

  (is
   (= (next-id)
      (create "b"))
   "second code is generated"))


(deftest pending-sequential
  (is
   (= (create "c") (create "c"))
   "without confirmation, previous sequential is returned"))
      

(deftest confirmation
  (is
   (let [d1 (create "d")
         _  (confirm "d" d1)
         d2 (create "d")]

     (not= d1 d2))
   
   "new sequential is returned after confirmation of the previous"))


(deftest interleaved
  (is
   (let [e1 (create "e")
         f1 (create "f")
         e2 (create "e")
         f2 (create "f")
         _  (confirm "e" e1)
         e3 (create "e")
         f3 (create "f")]

     (and
      (= e1 e2)
      (= f1 f2 f3)
      (not= e1 e3)
      (not= e1 f1))

     "IDs are independent")))


(defn sequence-cycle [id acc]
  (let [p (create id)]
    (confirm id p)
    (send acc conj p)
    id))

(deftest concurrent-clients

  (let [acc (agent #{})
        agents (map  #(agent [%]) ["a" "b" "c" "d"])]
        
    (doseq [ag (take 1000 (cycle agents))]
      (send ag sequence-cycle acc))

    (apply await agents)
    
    (is
     (= 1000 (count @acc))
     "all clients got unique ID")))
        
        
   


  