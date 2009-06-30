(ns loneclojurian.ovm
  (:import (java.io File RandomAccessFile)
           (java.nio ByteBuffer ByteOrder)))

(def *trace* false)

;; Instruction-generating functions
(defn make-add
  [#^Integer r1 #^Integer r2 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":add" r1 r2 ((:data machine-state) r1) ((:data machine-state) r2) (:status-register machine-state)))
    (update-in machine-state [:data] assoc
               pc
               (+ #^Double ((:data machine-state) r1)
                  #^Double ((:data machine-state) r2)))))
(defn make-sub
  [#^Integer r1 #^Integer r2 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":sub" r1 r2 ((:data machine-state) r1) ((:data machine-state) r2) (:status-register machine-state)))
    (update-in machine-state [:data] assoc
               pc
               (- #^Double ((:data machine-state) r1)
                  #^Double ((:data machine-state) r2)))))
(defn make-mult
  [#^Integer r1 #^Integer r2 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":mult" r1 r2 ((:data machine-state) r1) ((:data machine-state) r2) (:status-register machine-state)))
    (update-in machine-state [:data] assoc
               pc
               (* #^Double ((:data machine-state) r1)
                  #^Double ((:data machine-state) r2)))))
(defn make-div
  [#^Integer r1 #^Integer r2 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":div" r1 r2 ((:data machine-state) r1) ((:data machine-state) r2) (:status-register machine-state)))
    (if (= ((:data machine-state) r2) 0)
      (update-in machine-state [:data] assoc pc 0)
      (update-in machine-state [:data] assoc
                 pc
                 (/ #^Double ((:data machine-state) r1)
                    #^Double ((:data machine-state) r2))))))
(defn make-output
  [#^Integer r1 #^Integer r2 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":output" r1 r2 ((:data machine-state) r1) ((:data machine-state) r2) (:status-register machine-state)))
    (update-in machine-state [:output-ports] assoc
               r1
               ((:data machine-state) r2))))
(defn make-phi
  [#^Integer r1 #^Integer r2 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":phi" r1 r2 ((:data machine-state) r1) ((:data machine-state) r2) (:status-register machine-state)))
    (if (:status-register machine-state)
      (update-in machine-state [:data] assoc
                 pc
                 ((:data machine-state) r1))
      (update-in machine-state [:data] assoc
                 pc
                 ((:data machine-state) r2)))))
(defn make-noop
  []
  (fn [machine-state]
    (if *trace* (println ":noop"))
    machine-state))
(defn make-cmpltz
  [#^Integer r1 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":cmpltz" r1 ((:data machine-state) r1) (:status-register machine-state)))
    (assoc machine-state :status-register
           (< #^Double ((:data machine-state) r1) 0.))))
(defn make-cmplez
  [#^Integer r1 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":cmplez" r1 ((:data machine-state) r1) (:status-register machine-state)))
    (assoc machine-state :status-register
           (<= #^Double ((:data machine-state) r1) 0.0))))
(defn make-cmpeqz
  [#^Integer r1 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":cmpeqz" r1 ((:data machine-state) r1) (:status-register machine-state)))
    (assoc machine-state :status-register
           (= #^Double ((:data machine-state) r1) 0.0))))
(defn make-cmpgez
  [#^Integer r1 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":cmpgez" r1 ((:data machine-state) r1) (:status-register machine-state)))
    (assoc machine-state :status-register
           (>= #^Double ((:data machine-state) r1) 0.0))))
(defn make-cmpgtz
  [#^Integer r1 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":cmpgtz" r1 ((:data machine-state) r1) (:status-register machine-state)))
    (assoc machine-state :status-register
           (> #^Double ((:data machine-state) r1) 0.0))))
(defn make-sqrt
  [#^Integer r1 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":sqrt" r1 ((:data machine-state) r1) (:status-register machine-state)))
    (update-in machine-state [:data] assoc
               pc
               (Math/sqrt #^Double ((:data machine-state) r1)))))
(defn make-copy
  [#^Integer r1 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":copy" r1 ((:data machine-state) r1) (:status-register machine-state)))
    (update-in machine-state [:data] assoc
               pc
               #^Double ((:data machine-state) r1))))
(defn make-input [#^Integer r1 #^Integer pc]
  (fn [machine-state]
    (if *trace* (println pc ":input" r1 ((:input-ports machine-state) r1) (:status-register machine-state)))
    (update-in machine-state [:data] assoc
               pc
               #^Double ((:input-ports machine-state) r1))))


(defn execute-current-instruction
  "Run the given instruction fn with current machine state as argument"
  [machine-state instr]
  (instr machine-state))

(defn run-cycle
  "Run one iteration of the whole program"
  [machine-state]
  (reduce execute-current-instruction machine-state (:program machine-state)))

(def *frame-size* 12)

;; Simple binary-handling functions... convert 32-bit integer into a
;; string of 0's ans 1's and operate on it using string manipulation functions
(defn to-binary-string
  "Convert an integer to a binary string and add zero padding"
  [#^Integer int-value]
  (let [bits (Integer/toBinaryString int-value)]
      (str (.substring "00000000000000000000000000000000"
                       0
                       (- 32 (.length bits)))
           bits)))

(defn decode-instruction
  "Decode one instruction given by a 32-bit integer and return a that performs
   this action."
  [#^Integer instruction-code #^Integer frame-number]
  (let [#^String bits (to-binary-string instruction-code)]
    (if (= "0000" (.substring bits 0 4))
      ;; decode single-arg instructions
      (let [opcode (.substring bits 4 8)
            imm (.substring bits 8 11)
            r1 (.substring bits 18 32)]
          (cond
            (= opcode "0000") (make-noop)
            (and (= opcode "0001") (= imm "000")) (make-cmpltz (Integer/parseInt r1 2) frame-number)
            (and (= opcode "0001") (= imm "001")) (make-cmplez (Integer/parseInt r1 2) frame-number)
            (and (= opcode "0001") (= imm "010")) (make-cmpeqz (Integer/parseInt r1 2) frame-number)
            (and (= opcode "0001") (= imm "011")) (make-cmpgez (Integer/parseInt r1 2) frame-number)
            (and (= opcode "0001") (= imm "100")) (make-cmpgtz (Integer/parseInt r1 2) frame-number)
            (= opcode "0010") (make-sqrt (Integer/parseInt r1 2) frame-number)
            (= opcode "0011") (make-copy (Integer/parseInt r1 2) frame-number)
            (= opcode "0100") (make-input (Integer/parseInt r1 2) frame-number)
            true (str bits " - single arg instr not found!" )))
      ;; decode two-arg instructions
      (let [opcode (.substring bits 0 4)
            r1 (.substring bits 4 18)
            r2 (.substring bits 18 32)]
        (cond
          (= opcode "0001") (make-add (Integer/parseInt r1 2) (Integer/parseInt r2 2) frame-number)
          (= opcode "0010") (make-sub (Integer/parseInt r1 2) (Integer/parseInt r2 2) frame-number)
          (= opcode "0011") (make-mult (Integer/parseInt r1 2) (Integer/parseInt r2 2) frame-number)
          (= opcode "0100") (make-div (Integer/parseInt r1 2) (Integer/parseInt r2 2) frame-number)
          (= opcode "0101") (make-output (Integer/parseInt r1 2) (Integer/parseInt r2 2) frame-number)
          (= opcode "0110") (make-phi (Integer/parseInt r1 2) (Integer/parseInt r2 2) frame-number)
          true (str bits " - two arg instr not found!"))))))

(defn fill-data
  "Fill the machine data storage on address frame-number with a given data value"
  [machine-state frame-number data]
  (update-in machine-state [:data] assoc frame-number data))

(defn fill-instruction
  "Fill the machine instruction storage on address frame-number with a function
   pointer"
  [machine-state frame-number instruction-code]
  (update-in machine-state [:program] assoc frame-number (decode-instruction instruction-code frame-number)))

(defn fill-instruction-and-data
  "Simultaneously fill one address of instruction and data storage"
  [machine-state frame-number frame-data]
  (if (= (class (frame-data 0)) java.lang.Integer)
    (fill-instruction (fill-data machine-state frame-number (frame-data 1))
                      frame-number
                      (frame-data 0))
    (fill-instruction (fill-data machine-state frame-number (frame-data 0))
                      frame-number
                      (frame-data 1))))

(defn load-core-file
  "Load the core file and return a machine state with instruction and data
   storages ready for execution"
  [core-filename initial-machine-state]
  (let [core-size (.length (File. core-filename))]
    (if (not (zero? core-size))
      (let [file-buffer (make-array Byte/TYPE core-size)]
        (with-open [infile (RandomAccessFile. core-filename "r")]
         (.read infile file-buffer)
         (let [byte-buffer (ByteBuffer/wrap file-buffer)]
           (.order byte-buffer ByteOrder/LITTLE_ENDIAN)
           (loop [frame-number 0
                  num-frames (/ core-size *frame-size*)
                  machine-state initial-machine-state]
             (if (zero? num-frames)
               machine-state
               (recur (inc frame-number)
                      (dec num-frames)
                      (if (zero? (rem frame-number 2))
                        (fill-instruction-and-data machine-state
                                                   frame-number
                                                   [(.getDouble byte-buffer) (.getInt byte-buffer)])
                        (fill-instruction-and-data machine-state
                                                   frame-number
                                                   [(.getInt byte-buffer) (.getDouble byte-buffer)]))))))))
      (println "File not found!"))))

(defn init-ovm
  "Initialize the machine and load the core file."
  [core-filename]
  (let [initial-machine-state {:status-register false
                               :program (apply vector (take 16384 (repeat (make-noop))))
                               :data (apply vector (take 16384 (repeat 0.0)))
                               :input-ports (apply vector (take 16384 (repeat 0.0)))
                               :output-ports (apply vector (take 16384 (repeat 0.0)))
                               }]
    (load-core-file core-filename initial-machine-state)))
