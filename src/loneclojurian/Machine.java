package loneclojurian;

public final class Machine {
    static private double[] machine_data  = new double[16384];
    static private double[] input_ports = new double[16384];
    static private double[] output_ports = new double[16384];
    static private Boolean status_reg = false;

    public void Machine() {
        status_reg = false;

        for (int i = 0; i < 16384; i++) {
            machine_data[i] = 0.0;
            input_ports[i] = 0.0;
            output_ports[i] = 0.0;
        }
    }

    public static final double get_data(int index){
        return machine_data[index];
    }

    public static final double get_input_port(int index){
        return input_ports[index];
    }

    public static final double get_output_port(int index){
        return output_ports[index];
    }
    
    public static final void set_data(int index, double value){
        machine_data[index] = value;
    }

    public static final void set_input_port(int index, double value){
        input_ports[index] = value;
    }

    public static final void set_output_port(int index, double value){
        output_ports[index] = value;
    }

    public static final void set_status_reg(boolean value){
        status_reg = value;
    }
    public static final boolean get_status_reg(){
        return status_reg;
    }
    public static final void add (int pc, int r1, int r2) {
        machine_data[pc] = machine_data[r1] + machine_data[r2];
    }
    public static final void sub (int pc, int r1, int r2) {
        machine_data[pc] = machine_data[r1] - machine_data[r2];
    }
    public static final void mult (int pc, int r1, int r2) {
        machine_data[pc] = machine_data[r1] * machine_data[r2];
    }
    public static final void div (int pc, int r1, int r2) {
        if (machine_data[r2] == 0.0){
            machine_data[pc] = 0.0;
        }
        else {
            machine_data[pc] = machine_data[r1] / machine_data[r2];
        }
    }
    public static final void cmpltz (int r1){
        if (machine_data[r1] < 0.0){
            status_reg = true;
        }else {
            status_reg = false;
        }
    }
    public static final void cmplez (int r1){
        if (machine_data[r1] <= 0.0){
            status_reg = true;
        }else {
            status_reg = false;
        }
    }
    public static final void cmpeqz (int r1){
        if (machine_data[r1] == 0.0){
            status_reg = true;
        }else {
            status_reg = false;
        }
    }
    public static final void cmpgez (int r1){
        if (machine_data[r1] >= 0.0){
            status_reg = true;
        }else {
            status_reg = false;
        }
    }
    public static final void cmpgtz (int r1){
        if (machine_data[r1] > 0.0){
            status_reg = true;
        }else {
            status_reg = false;
        }
    }
    public static final void phi(int pc, int r1, int r2){
        if (status_reg == true){
            machine_data[pc] = machine_data[r1];
        }else {
            machine_data[pc] = machine_data[r2];
        }
    }
    public static final void output(int r1, int r2){
        output_ports[r1] = machine_data[r2];
    }
    public static final void input(int pc, int r1){
        machine_data[pc] = input_ports[r1];
    }
    public static final void copy(int pc, int r1){
        machine_data[pc] = machine_data[r1];
    }
    public static final void sqrt(int pc, int r1){
        machine_data[pc] = Math.sqrt(machine_data[r1]);
    }
}