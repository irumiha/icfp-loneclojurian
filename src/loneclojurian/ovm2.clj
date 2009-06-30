(ns loneclojurian.ovm2
  (:import (java.io File RandomAccessFile)
           (java.nio ByteBuffer ByteOrder)))

(def *trace* false)

(def status-reg (make-array Boolean/TYPE 1))
(def data (make-array Double/TYPE 16384))
(def input-ports (make-array Double/TYPE 16384))
(def output-ports (make-array Double/TYPE 16384))
(def program (atom []))

(defn run-cycle
  "Run one iteration of the whole program"
  [prog]
  (dotimes [x 16384]
    ((prog x))))

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
  [#^Integer instruction-code frame-num]
  (let [#^String bits (to-binary-string instruction-code)]
    (if (= "0000" (.substring bits 0 4))
      ;; decode single-arg instructions
      (let [opcode (.substring bits 4 8)
            imm (.substring bits 8 11)
            r1 (.substring bits 18 32)]
          (cond
            (= opcode "0000") (fn [] nil)
            (and (= opcode "0001") (= imm "000"))
              (fn [] (aset-boolean status-reg 0 (< #^Double (aget data (Integer/parseInt r1 2)) 0.)))
            (and (= opcode "0001") (= imm "001"))
              (fn [] (aset-boolean status-reg 0 (<= #^Double (aget data (Integer/parseInt r1 2)) 0.)))
            (and (= opcode "0001") (= imm "010"))
              (fn [] (aset-boolean status-reg 0 (= #^Double (aget data (Integer/parseInt r1 2)) 0.)))
            (and (= opcode "0001") (= imm "011"))
              (fn [] (aset-boolean status-reg 0 (>= #^Double (aget data (Integer/parseInt r1 2)) 0.)))
            (and (= opcode "0001") (= imm "100"))
              (fn [] (aset-boolean status-reg 0 (> #^Double (aget data (Integer/parseInt r1 2)) 0.)))
            (= opcode "0010")
              (fn [] (aset-double data frame-num (Math/sqrt #^Double (aget data (Integer/parseInt r1 2)))))  
            (= opcode "0011")
              (fn [] (aset-double data frame-num (aget data (Integer/parseInt r1 2))))  
            (= opcode "0100")
              (fn [] (aset-double data frame-num (aget input-ports (Integer/parseInt r1 2))))
            true (str bits " - single arg instr not found!" )))
      ;; decode two-arg instructions
      (let [opcode (.substring bits 0 4)
            r1 (.substring bits 4 18)
            r2 (.substring bits 18 32)]
        (cond
          (= opcode "0001")
            (fn [] (aset-double data frame-num (+ (aget data (Integer/parseInt r1 2))
                                                  (aget data (Integer/parseInt r2 2)))))
          (= opcode "0010")
            (fn [] (aset-double data frame-num (- (aget data (Integer/parseInt r1 2))
                                                  (aget data (Integer/parseInt r2 2)))))
          (= opcode "0011")
            (fn [] (aset-double data frame-num (* (aget data (Integer/parseInt r1 2))
                                                  (aget data (Integer/parseInt r2 2)))))
          (= opcode "0100")
            (fn [] (if (= (aget data (Integer/parseInt r2 2)) 0.0)
                     (aset-double data frame-num 0.0)
                     (aset-double data frame-num (/ (aget data (Integer/parseInt r1 2))
                                                    (aget data (Integer/parseInt r2 2))))))
          (= opcode "0101")
            (fn [] (aset-double output-ports (Integer/parseInt r1 2) (aget data (Integer/parseInt r2 2))))
          (= opcode "0110")
            (fn [] (if (aget status-reg 0)
                     (aset-double data frame-num (aget data (Integer/parseInt r1 2)))
                     (aset-double data frame-num (aget data (Integer/parseInt r2 2)))))
          true (str bits " - two arg instr not found!"))))))

(defn fill-data
  "Fill the machine data storage on address frame-number with a given data value"
  [frame-number #^Double new-data]
  (aset-double data frame-number new-data))

(defn fill-instruction
  "Fill the machine instruction storage on address frame-number with a function
   pointer"
  [frame-number instruction-code]
  (swap! program assoc frame-number (decode-instruction instruction-code frame-number)))

(defn fill-instruction-and-data
  "Simultaneously fill one address of instruction and data storage"
  [frame-number frame-data]
  (if (= (class (frame-data 0)) java.lang.Integer)
    (do
     (fill-instruction frame-number (frame-data 0))
     (fill-data frame-number (frame-data 1)))
    (do
      (fill-instruction frame-number (frame-data 1))
      (fill-data frame-number (frame-data 0)))))

(defn load-core-file
  "Load the core file and return a machine state with instruction and data
   storages ready for execution"
  [core-filename]
  (let [core-size (.length (File. core-filename))]
    (if (not (zero? core-size))
      (let [file-buffer (make-array Byte/TYPE core-size)]
        (with-open [infile (RandomAccessFile. core-filename "r")]
         (.read infile file-buffer)
         (let [byte-buffer (ByteBuffer/wrap file-buffer)]
           (.order byte-buffer ByteOrder/LITTLE_ENDIAN)
           (loop [frame-number 0
                  num-frames (/ core-size *frame-size*)]
             (if (zero? num-frames)
               true
               (do
                 (if (zero? (rem frame-number 2))
                   (fill-instruction-and-data frame-number
                                              [(.getDouble byte-buffer) (.getInt byte-buffer)])
                   (fill-instruction-and-data frame-number
                                              [(.getInt byte-buffer) (.getDouble byte-buffer)]))
                 (recur (inc frame-number)
                        (dec num-frames))))))))
      (println "File not found!"))))

(defn init-ovm
  "Initialize the machine and load the core file."
  [core-filename]
  (aset-boolean status-reg 0 false)
  (reset! program (apply vector (take 16384 (repeat (fn [] nil)))))
  (dotimes [x 16384] (aset-double data x 0.0))
  (dotimes [x 16384] (aset-double input-ports x 0.0))
  (dotimes [x 16384] (aset-double output-ports x 0.0))
  (load-core-file core-filename))

