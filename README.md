The purpose of this repo is show some VHDL code that I wrote for ATLAS back in 2020.

 -  I wrote VHDL code for an I2C/SMbus/PMbus interface suitable for temperature, voltage, and current measurements/regulation with components such as an LT3884, a PMbus compatible chip. **Original repo**: 

https://gitlab.cern.ch/atlas-tdaq-p2-firmware/tdaq-htt-firmware/prm-fw-group/prm-fw-max10/-/blob/ae7d891925b07701076152065f98f71fe64c595d/src/prm_fw_max10/master/pmbus_master.vhd , 

https://gitlab.cern.ch/atlas-tdaq-p2-firmware/tdaq-htt-firmware/prm-fw-group/prm-fw-max10/-/blob/ae7d891925b07701076152065f98f71fe64c595d/src/prm_fw_max10/master/smbus_master.vhd , 

https://gitlab.cern.ch/atlas-tdaq-p2-firmware/tdaq-htt-firmware/prm-fw-group/prm-fw-max10/-/blob/5f863437c5534643edd7d75c848e543f6e876601/clock_manager_setup/source_files/i2c_master.vhd
 -  I designed an SMbus interface for communication with an ESP32 microcontroller. Simulator: Modelsim. **Original repo** 

https://gitlab.cern.ch/atlas-tdaq-p2-firmware/tdaq-htt-firmware/prm-fw-group/prm-fw-max10/-/blob/ae7d891925b07701076152065f98f71fe64c595d/src/prm_fw_max10/master/smbus_master.vhd

SMBus FSM Diagram:
 
 '''mermaid
flowchart 
    B(((ready)))-->X{ena}
    X-->|'1'|C[start]
    X-->|'0'|B
    C-->D[slv_addr]
    D-->X0{bit_cnt}
    X0-->|\='0'|D
    X0-->|='0'|E[slv_ack1]
    E-->X1{ack_error}
    
    X1-->|'0'|X2{rw}
    X2-->|'1'|X3{cmd_sent}

    X1-->|'1'|W

    X2-->|'0'|F[command]
    X3-->|'0'|F
    X3-->|'1'|K[rd]
    F-->X4{bit_cnt}
    X4-->|/='0'|F
    X4-->|='0'|O[slv_ack2]
    O-->X5{ack_error}
    X5-->|'1'|W
    X5-->|'0'|X12{rw}
    X12-->|'1'|C
    X12-->|'0'|X11{ena}
    X11-->|'1'|X13{addr and rw bit \n did not change}
    X11-->|'0'|C
    X13-->|false|C
    X13-->|true|G[wr]

    G-->X14{bit_cnt}
    X14-->|'0'|P[slv_ack3]
    X14-->|'1'|G
    P-->X15{ack_error}
    X15-->|'1'|W
    X15-->H[wr2]


    H-->X6{bit_cnt}
    X6-->|='0'|Q[slv_ack4]
    X6-->|\='0'|H
    Q-->X7{ack_error}
    X7-->|'1'|W
    X7-->|'0'|I[wr3]
    I-->X8{bit_cnt}
    X8-->|\='0'|I
    X8-->|='0'|R[slv_ack5]
    R-->X9{ack_error}
    X9-->|'1'|W
    X9-->|'0'|J[wr4]
    J-->X10{bit_cnt}
    X10-->|\='0'|J
    X10-->|='0'|S[slv_ack6]
    S-->W

    K-->X16{bit_cnt}
    X16-->|='0'|T[master_ack]
    X16-->|\='0'|K

    T-->X17{ena}
    X17-->|'1'|X18{addr and rw bit \n did not change}
    X17-->|'0'|W
    X18-->|true|L[rd2]
    X18-->|false|C

    L-->X19{bit_cnt}
    X19-->|\='0'|L
    X19-->|='0'|U[master_ack2]

    
    U-->M[rd3]

    M-->X20{bit_cnt}
    X20-->|\='0'|M
    X20-->|='0'|V[master_ack3]
    V-->N[rd4]

    N-->X21{bit_cnt}
    X21-->|\='0'|N
    X21-->|='0'|Z[master_ack4]

    Z-->W
    W[stop]

  
'''
