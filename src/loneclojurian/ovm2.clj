(ns loneclojurian.ovm2
  (:import (java.io File RandomAccessFile)
           (java.nio ByteBuffer ByteOrder)
           (loneclojurian Machine)))

(def *trace* false)

(def program (atom ()))

(def *machine* (Machine.))

(def *frame-size* 12)

;; Simple binary-handling functions... convert 32-bit integer into a
;; string of 0's and 1's and operate on it using string manipulation functions
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
            (= opcode "0000") ()
            (and (= opcode "0001") (= imm "000"))
                  `(Machine/cmpltz ~(Integer/parseInt r1 2))
            (and (= opcode "0001") (= imm "001"))
                  `(Machine/cmplez ~(Integer/parseInt r1 2))
            (and (= opcode "0001") (= imm "010"))
                  `(Machine/cmpeqz ~(Integer/parseInt r1 2))
            (and (= opcode "0001") (= imm "011"))
                  `(Machine/cmpgez ~(Integer/parseInt r1 2))
            (and (= opcode "0001") (= imm "100"))
                  `(Machine/cmpgtz ~(Integer/parseInt r1 2))
            (= opcode "0010")
                  `(Machine/sqrt ~frame-num ~(Integer/parseInt r1 2))
            (= opcode "0011")
                  `(Machine/copy ~frame-num ~(Integer/parseInt r1 2))
            (= opcode "0100")
                  `(Machine/input ~frame-num ~(Integer/parseInt r1 2))
            true (str bits " - single arg instr not found!" )))
      ;; decode two-arg instructions
      (let [opcode (.substring bits 0 4)
            r1 (.substring bits 4 18)
            r2 (.substring bits 18 32)]
        (cond
          (= opcode "0001")
                  `(Machine/add ~frame-num ~(Integer/parseInt r1 2) ~(Integer/parseInt r2 2))
          (= opcode "0010")
                  `(Machine/sub ~frame-num ~(Integer/parseInt r1 2) ~(Integer/parseInt r2 2))
          (= opcode "0011")
                  `(Machine/mult ~frame-num ~(Integer/parseInt r1 2) ~(Integer/parseInt r2 2))
          (= opcode "0100")
                  `(Machine/div ~frame-num ~(Integer/parseInt r1 2) ~(Integer/parseInt r2 2))
          (= opcode "0101")
                  `(Machine/output ~(Integer/parseInt r1 2) ~(Integer/parseInt r2 2))
          (= opcode "0110")
                  `(Machine/phi ~frame-num ~(Integer/parseInt r1 2) ~(Integer/parseInt r2 2))
          true (str bits " - two arg instr not found!"))))))

(defn fill-data
  "Fill the machine data storage on address frame-number with a given data value"
  [#^Integer frame-number #^Double new-data]
  (Machine/set_data frame-number new-data))

(defn fill-instruction
  "Fill the machine instruction storage on address frame-number with a function
   pointer"
  [frame-number instruction-code]
  (reset! program (cons (decode-instruction instruction-code frame-number) @program)))

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
  (def *machine* (Machine.))
  (load-core-file core-filename)
  (let [instructions (reverse @program)
        prg-one `(fn [] (do ~@instructions))]
    (eval prg-one)))
