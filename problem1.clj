(require 'loneclojurian.sim)
(in-ns 'loneclojurian.sim)

(println "Running task 1001")
(hohmann-maneuver "data/bin1.obf" 1001.0)
(gen-trace-file "solutions/solution1001.osf" @*thruster-trace* 1001)

(println "Running task 1002")
(hohmann-maneuver "data/bin1.obf" 1002.0)
(gen-trace-file "solutions/solution1002.osf" @*thruster-trace* 1002)

(println "Running task 1003")
(hohmann-maneuver "data/bin1.obf" 1003.0)
(gen-trace-file "solutions/solution1003.osf" @*thruster-trace* 1003)

(println "Running task 1004")
(hohmann-maneuver "data/bin1.obf" 1004.0)
(gen-trace-file "solutions/solution1004.osf" @*thruster-trace* 1004)

(println "Done, solution files are in the solutions/ directory")

