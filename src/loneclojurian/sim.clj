(ns loneclojurian.sim
  (:require [loneclojurian.ovm :as ovm])
  (:import (java.io File RandomAccessFile)
           (java.nio ByteBuffer ByteOrder)))

;; Constants

(def *big-g* 6.67428E-11)
(def *earth-mass* 6.0E24)
(def *earth-u* (/ (* *earth-mass* *big-g*) 1.0E9))

;; Use refs to keep track of time and 
(def *timestamp* (ref 0))
(def *thruster-trace* (ref []))

(def *sim-timestep* (atom {}))
(def *sim-status* (atom {}))

(defn new-timestamp
  []
  (dosync (alter *timestamp* inc)))

(defn gen-trace-file
  [filename trace-vector #^Integer scenario-id]
  (let [trace-file-bytes (+ 40 (* (- (count trace-vector) 2) 32))]
    (let [file-buffer (make-array Byte/TYPE trace-file-bytes)]
        (with-open [outfile (RandomAccessFile. filename "rw")]
          (let [byte-buffer (ByteBuffer/wrap file-buffer)]
            (.order byte-buffer ByteOrder/LITTLE_ENDIAN)
            (.putInt byte-buffer 0xCAFEBABE)
            (.putInt byte-buffer 511)
            (.putInt byte-buffer scenario-id)
            (doseq [frame trace-vector :when (>= (count frame) 2)]
              (.putInt byte-buffer (frame 0))
              (.putInt byte-buffer (dec (count (filter #(not (nil? %)) frame))))
              (doseq [thruster (filter #(not (nil? %)) (subvec frame 1))]
                (.putInt byte-buffer (thruster 0))
                (.putDouble byte-buffer (thruster 1))))
            (.putInt byte-buffer ((trace-vector (dec (count trace-vector))) 0))
            (.putInt byte-buffer 0))
          (.write outfile file-buffer)))))

(defn change-speed
  "Apply delta-V command for one cycle."
  [machine #^Double delta-v-x #^Double delta-v-y]
  (let [[_ _ old-delta-v-x old-delta-v-y] (subvec (:input-ports machine) 0 4)]
    (if (or (not (= old-delta-v-x delta-v-x ))
            (not (= old-delta-v-y delta-v-y)))
      (dosync (ref-set *thruster-trace* (conj @*thruster-trace* (conj [@*timestamp*]
                                                                      (if (not (= old-delta-v-x delta-v-x ))
                                                                        [2 delta-v-x]
                                                                        nil)
                                                                      (if (not (= old-delta-v-y delta-v-y))
                                                                        [3 delta-v-y]
                                                                        nil)))))))
  (new-timestamp)
  (ovm/run-cycle (assoc machine :input-ports
                        (apply conj [0.0 0.0 delta-v-x delta-v-y] (subvec (:input-ports machine) 4)))))

(defn quiet-run
  "Run one cycle of the sim with actuator ports set to 0.0"
  [machine]
  (let [[_ _ old-delta-v-x old-delta-v-y] (subvec (:input-ports machine) 0 4)]
    (if (or (not (= old-delta-v-x 0.0 ))
            (not (= old-delta-v-y 0.0)))
      (dosync (ref-set *thruster-trace* (conj @*thruster-trace* (conj [@*timestamp*]
                                                                      (if (not (= old-delta-v-x 0.0 ))
                                                                        [2 0.0]
                                                                        nil)
                                                                      (if (not (= old-delta-v-y 0.0))
                                                                        [3 0.0]
                                                                        nil)))))))
  (new-timestamp)
  (ovm/run-cycle (assoc machine :input-ports
                        (apply conj [0.0 0.0 0.0 0.0] (subvec (:input-ports machine) 4)))))

(defn score
  "Read the current score"
  [machine]
  ((:output-ports machine) 0))

(defn remaining-fuel
  "Read the amount of fuel left"
  [machine]
  ((:output-ports machine) 0))

(defn position
  "Read the current position and calculate distance from Earth, return everything as a vector"
  [machine]
  (let [[x-pos y-pos] (subvec (:output-ports machine) 2 4)
        r (Math/sqrt (+ (* x-pos x-pos) (* y-pos y-pos)))]
    [x-pos y-pos r]))

;; Functions related to the Hohmann procedure
(defn delta-v-first
  [r-start r-target]
  (let [r-start-km (/ r-start 1000)
        r-target-km (/ r-target 1000)]
    (* 1000
       (* (Math/sqrt (/ *earth-u* r-start-km))
          (- (Math/sqrt (/ 2 (+ 1 (/ r-start-km r-target-km))))
             1)))))

(defn delta-v-second
  [r-start r-target]
  (let [r-start-km (/ r-start 1000)
        r-target-km (/ r-target 1000)]
    (* 1000 (* (Math/sqrt (/ *earth-u* r-target-km))
               (- 1
                  (Math/sqrt (/ 2 (+ 1 (/ r-target-km r-start-km)))))))))

(defn delta-t
  [r-start r-target]
  (let [r-start-km (/ r-start 1000)
        r-target-km (/ r-target 1000)]
    (* Math/PI
       (Math/sqrt (/ (Math/pow (+ r-start-km r-target-km) 3)
                     (* 8 *earth-u*))))))

(defn decompose-delta-v
  "Decompose the absolute value of the speed change into vector components,
   the new speed vector must point in the same direction as the old vector"
  [orig-speed-x orig-speed-y delta-v-absolute]
  (let [angle (Math/atan (/ (Math/abs orig-speed-y) (Math/abs orig-speed-x)))]
    [(* delta-v-absolute (Math/cos angle) (Math/signum orig-speed-x))
     (* delta-v-absolute (Math/sin angle) (Math/signum orig-speed-y))]))

(defn game-loop
  "This is how it should be done. The game loop accepts an initialized machine,
   a scenario ID and pointers to functions take-game-state, select-and-execute-action
   and conditions-satisfied. The algorithm goes:
   1. do an initial quiet-run (to initialize the sim)
   2. run take-game-state to fetch all relevant parameters
   3. check if conditions-satisfied returns true, if it does finish
   4. run select-and-execute-action to run the game logic and execute
      the next action selected by the game logic.
   5. run take-game-state to fetch new state of the world
   6. goto 3 ;)

   Possible actions are:

   quiet-run :   run one sim iteration with thrusters turned off
   change-speed: run one sim iteration with thrusters set to achieve desired delta-v
  "
  [machine #^Double scenario-id take-game-state select-and-execute-action conditions-satisfied]
  (dosync (ref-set *timestamp* 0)
          (ref-set *thruster-trace* [[0 [16000 scenario-id]]]))
  (loop [current-machine (quiet-run (update-in machine [:input-ports] assoc 16000 scenario-id))
         game-state (take-game-state current-machine)]
    (if (conditions-satisfied game-state)
      [@*thruster-trace* game-state]
      (recur (select-and-execute-action current-machine game-state)
             (take-game-state current-machine)))))

;; This is ugly, has a lot of duplicated code and should be rewritten
;; to use the game-loop function defined above... With more time... :)
(defn hohmann-maneuver
  [core-filename #^Double task-id]
  (let [machine (ovm/init-ovm core-filename)]
    (dosync (ref-set *timestamp* 0)
            (ref-set *thruster-trace* [[0 [16000 task-id]]]))
    (reset! *sim-timestep* (update-in machine [:input-ports] assoc 16000 task-id))
    (swap! *sim-timestep* quiet-run)
    (reset! *sim-status*
             {:position (position @*sim-timestep*)
              :target-radius ((:output-ports @*sim-timestep*) 4)
              })
    (swap! *sim-timestep* quiet-run)
    (swap! *sim-status* merge {:speed-x (- ((:output-ports @*sim-timestep*) 2)
                                           ((:position @*sim-status*) 0))
                               :speed-y (- ((:output-ports @*sim-timestep*) 3)
                                           ((:position @*sim-status*) 1))
                               :delta-v-1 (delta-v-first ((:position @*sim-status*) 2)
                                                         (:target-radius @*sim-status*))
                               :delta-v-2 (delta-v-second ((:position @*sim-status*) 2)
                                                          (:target-radius @*sim-status*))
                               :delta-t (delta-t ((:position @*sim-status*) 2)
                                                 (:target-radius @*sim-status*))})
    (println "Target radius is:" (:target-radius @*sim-status*))
    (println "Changing speed..." (decompose-delta-v (- (:speed-x @*sim-status*))
                                                    (- (:speed-y @*sim-status*))
                                                    (:delta-v-1 @*sim-status*)))
    
    (swap! *sim-timestep* #(apply change-speed % (decompose-delta-v (- (:speed-x @*sim-status*))
                                                                    (- (:speed-y @*sim-status*))
                                                                    (:delta-v-1 @*sim-status*))))
    (swap! *sim-status* merge {:second-burn-t (+ @*timestamp* (:delta-t @*sim-status*))})
    (println "Speed changed... waiting until " (:second-burn-t @*sim-status*) "seconds")
    ;; Now that we have fired the thrusters, we wait until we
    ;; are at the second orbit
    (reset! *sim-timestep* (loop [sim-state @*sim-timestep*
                                  target-r (:target-radius @*sim-status*)
                                  wait-steps 0
                                  old-x nil
                                  old-y nil]
                             (when (not (nil? old-x))
                               (swap! *sim-status* merge {:speed-x (- ((:output-ports sim-state) 2) old-x)
                                                          :speed-y (- ((:output-ports sim-state) 3) old-y)}))
                             (if (>= @*timestamp* (:second-burn-t @*sim-status*))
                               sim-state
                               (recur (quiet-run sim-state)
                                      target-r
                                      (inc wait-steps)
                                      ((:output-ports sim-state) 2)
                                      ((:output-ports sim-state) 3)))))
    ;; We are at the second orbit, fire again to enter this
    ;; orbit
    (println "Arrived at orbit! Syncing...")        
    (swap! *sim-timestep* #(apply change-speed % (decompose-delta-v (- (:speed-x @*sim-status*))
                                                                    (- (:speed-y @*sim-status*))
                                                                    (:delta-v-2 @*sim-status*))))
    (println "Burning fuel...")
    (reset! *sim-timestep* (loop [sim-state @*sim-timestep*
                                  wait-steps 0
                                  old-x nil
                                  old-y nil]
                             (when (not (nil? old-x))
                               (swap! *sim-status* merge {:speed-x (- ((:output-ports sim-state) 2) old-x)
                                                          :speed-y (- ((:output-ports sim-state) 3) old-y)}))
                             (if (< ((:output-ports sim-state) 1) 200)
                               sim-state
                               (recur (apply change-speed
                                             (apply change-speed sim-state (decompose-delta-v (- (:speed-x @*sim-status*))
                                                                                              (- (:speed-y @*sim-status*))
                                                                                              100))
                                             (decompose-delta-v (- (:speed-x @*sim-status*))
                                                                (- (:speed-y @*sim-status*))
                                                                -100))
                                      (inc wait-steps)
                                      ((:output-ports sim-state) 2)
                                      ((:output-ports sim-state) 3)))))

    (println "Done, waiting for success status (no more than 1500 sec)...")
    ;; Hopefully this is enough, now we wait some 900 cycles
    ;; until victory is declared! :)
    (loop [sim-state @*sim-timestep*
           runs 0]
      (if (> ((:output-ports sim-state) 0) 0.0)
        (do (dosync (alter *thruster-trace* conj [@*timestamp*]))
          (println "Success! Score:" ((:output-ports sim-state) 0)))
        (if (> runs 1500)
          (println "Failure :(")
          (recur (quiet-run sim-state)
                 (inc runs)))))))

