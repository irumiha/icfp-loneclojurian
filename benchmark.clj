(require 'loneclojurian.ovm2)

(in-ns 'loneclojurian.ovm2)
(def machine-fn (init-ovm "/Users/irumiha/Work/clojure/icfp-contest/data/bin1.obf"))
(defn one-run [] (machine-fn) [(Machine/get_output_port 0)
                               (Machine/get_output_port 1)
                               (Machine/get_output_port 2)
                               (Machine/get_output_port 3)
                               (Machine/get_output_port 4)])

(Machine/set_input_port 16000 1001.0)
(time (dotimes [_ 200000] (one-run)))
