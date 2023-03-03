
LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

ENTITY smbus_master IS
  GENERIC(
    input_clk : INTEGER := 50_000_000; --input clock speed from user logic in Hz
    bus_clk   : INTEGER := 400_000);   --speed the i2c bus (scl) will run at in Hz
  PORT(
    clk       : IN     STD_LOGIC;                    --system clock
    reset_n   : IN     STD_LOGIC;                    --active low reset
    ena       : IN     STD_LOGIC;                    --latch in slv_addr
    addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
    rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
	cmnd	  : IN	   STD_LOGIC_VECTOR(7 downto 0); --command code (smbus mode only)
    data_wr   : IN     STD_LOGIC_VECTOR(31 DOWNTO 0); --data to write to slave
    busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
    data_rd   : OUT    STD_LOGIC_VECTOR(31 DOWNTO 0); --data read from slave
    ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC);                   --serial clock output of i2c bus
END smbus_master;

ARCHITECTURE logic OF smbus_master IS
  CONSTANT divider  :  INTEGER := (input_clk/bus_clk)/4; --number of clocks in 1/4 cycle of scl
  TYPE machine IS(ready, start, slv_addr, slv_ack1, command, wr,wr2,wr3,wr4, rd,rd2,rd3,rd4, slv_ack2,slv_ack3,slv_ack4,slv_ack5,slv_ack6, mstr_ack,mstr_ack2,mstr_ack3,mstr_ack4, stop); --needed states
  SIGNAL state         : machine;                        --state machine
  SIGNAL data_clk      : STD_LOGIC;                      --data clock for sda
  SIGNAL data_clk_prev : STD_LOGIC;                      --data clock during previous system clock
  SIGNAL scl_clk       : STD_LOGIC;                      --constantly running internal scl
  SIGNAL scl_ena       : STD_LOGIC := '0';               --enables internal scl to output
  SIGNAL sda_int       : STD_LOGIC := '1';               --internal sda
  SIGNAL sda_ena_n     : STD_LOGIC;                      --enables internal sda to output
  SIGNAL addr_rw       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --latched in address and read/write
  SIGNAL data_tx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --latched in data to write to slave
  SIGNAL data_rx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --data received from slave
  SIGNAL bit_cnt       : INTEGER RANGE 0 TO 7 := 7;      --tracks bit number in transaction
  SIGNAL stretch       : STD_LOGIC := '0';               --identifies if slave is stretching scl
  SIGNAL arb_lost	   : STD_LOGIC :='0';
  signal sda_prev	   : std_logic;
  signal busy_b		   : std_logic;
  signal busy_mas	   : std_logic;
  signal cmnd_sent	   : std_logic;						 --bit used to signify that command byte is sent
BEGIN

  --generate the timing for the bus clock (scl_clk) and the data clock (data_clk)
  PROCESS(clk, reset_n)
    VARIABLE count  :  INTEGER RANGE 0 TO divider*4;  --timing for clock generation
  BEGIN
    IF(reset_n = '0') THEN                --reset asserted
      stretch <= '0';
      count := 0;
    ELSIF(clk'EVENT AND clk = '1') THEN
      data_clk_prev <= data_clk;          --store previous value of data clock
	    sda_prev <= sda;
      IF(count = divider*4-1) THEN        --end of timing cycle
        count := 0;                       --reset timer
      ELSIF(stretch = '0') THEN           --clock stretching from slave not detected
        count := count + 1;               --continue clock generation timing
      END IF;
      CASE count IS
        WHEN 0 TO divider-1 =>            --first 1/4 cycle of clocking
          scl_clk <= '0';
          data_clk <= '0';
        WHEN divider TO divider*2-1 =>    --second 1/4 cycle of clocking
          scl_clk <= '0';
          data_clk <= '1';
        WHEN divider*2 TO divider*3-1 =>  --third 1/4 cycle of clocking
          scl_clk <= '1';                 --release scl
          IF(scl = '0') THEN              --detect if slave is stretching clock
            stretch <= '1';
          ELSE
            stretch <= '0';
          END IF;
          data_clk <= '1';
        WHEN OTHERS =>                    --last 1/4 cycle of clocking
          scl_clk <= '1';
          data_clk <= '0';
      END CASE;
    END IF;
  END PROCESS;

  --state machine and writing to sda during scl low (data_clk rising edge)
  PROCESS(clk, reset_n)
  BEGIN
    IF(reset_n = '0') THEN                 --reset asserted
      state <= ready;                      --return to initial state
      busy_mas <= '1';                         --indicate not available
      scl_ena <= '0';                      --sets scl high impedance
      sda_int <= '1';                      --sets sda high impedance
      ack_error <= '0';                    --clear acknowledge error flag
      bit_cnt <= 7;                        --restarts data bit counter
      data_rd <= (others=>'0');               --clear data read port
	  cmnd_sent<='0';
    ELSIF(clk'EVENT AND clk = '1') THEN
      IF(data_clk = '1' AND data_clk_prev = '0') THEN  --data clock rising edge
        CASE state IS
          WHEN ready =>                      --idle state
			      arb_lost<='0';
			      ack_error<='0';
				  cmnd_sent<='0';
            IF(ena = '1') THEN               --transaction requested
              busy_mas <= '1';                   --flag busy
			  IF (rw='1') THEN
				addr_rw <= addr & '0';			    
			  ELSE
				addr_rw <= addr & rw;          --collect requested slave address and slv_addr
			  END IF;
              data_tx <= data_wr(7 downto 0);            --collect requested data to write
              state <= start;                --go to start bit
            ELSE                             --remain idle
			  busy_mas <= '0';                   --unflag busy
              state <= ready;                --remain idle
            END IF;
          WHEN start =>                      --start bit of transaction
            busy_mas <= '1';                 --resume busy if continuous mode
            sda_int <= addr_rw(bit_cnt);     --set first address bit to bus
            state <= slv_addr;                --go to slv_addr
          WHEN slv_addr =>                    --address and slv_addr byte of transaction
            IF(bit_cnt = 0) THEN             --slv_addr transmit finished
              sda_int <= '1';                --release sda for slave acknowledge
              bit_cnt <= 7;                  --reset bit counter for "byte" states
              state <= slv_ack1;             --go to slave acknowledge (slv_addr)
			  IF (rw='1') THEN
				addr_rw(0)<='1';
			  END IF;
            ELSE                             --next clock cycle of slv_addr state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= addr_rw(bit_cnt-1); --write address/slv_addr bit to bus
              state <= slv_addr;              --continue with slv_addr
            END IF;
          WHEN slv_ack1 =>                   --slave acknowledge bit (slv_addr)
		  IF ack_error='1' then
			  state<=stop;
		  ELSIF (addr_rw(0)='1') THEN
			if (cmnd_sent='0') then
				state<=command;
				sda_int <= cmnd(bit_cnt);
				cmnd_sent<='1';
			else
				state<=rd;
				sda_int<='1';
			end if;
		  ELSE
				state<=command;
				sda_int <= cmnd(bit_cnt);
		  END IF;
		  WHEN command =>
			IF(bit_cnt = 0) THEN            
              sda_int <= '1';
              bit_cnt <= 7;
              state <= slv_ack2;             
            ELSE                             
              bit_cnt <= bit_cnt - 1;        
              sda_int <= cmnd(bit_cnt-1); 
              state <= command;              
			END IF;
          WHEN wr =>                         --write byte of transaction
            busy_mas <= '1';                     --resume busy if continuous mode
            IF(bit_cnt = 0) THEN             --write byte transmit finished
              sda_int <= '1';                --release sda for slave acknowledge
              bit_cnt <= 7;                  --reset bit counter for "byte" state
			  state<=slv_ack3;
            ELSE                             --next clock cycle of write state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= data_tx(bit_cnt-1); --write next bit to bus
              state <= wr;                   --continue writing
            END IF;
          WHEN rd =>                         --read byte of transaction
            busy_mas <= '1';                     --resume busy if continuous mode
            IF(bit_cnt = 0) THEN             --read byte receive finished
              IF(ena = '1' AND addr_rw = addr & rw) THEN  --continuing with another read at same address
                sda_int <= '0';              --acknowledge the byte has been received
              ELSE                           --stopping or continuing with a write
                sda_int <= '1';              --send a no-acknowledge (before stop or repeated start)
              END IF;
              bit_cnt <= 7;                  --reset bit counter for "byte" states
              data_rd(7 downto 0) <= data_rx;            --output received data
              state <= mstr_ack;             --go to master acknowledge
            ELSE                             --next clock cycle of read state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              state <= rd;                   --continue reading
            END IF;
          WHEN slv_ack2 =>                   --slave acknowledge bit (write)
			IF ack_error='1' then
			  state<=stop;
			ELSIF (addr_rw(0)='1') THEN
				state<=start;
			ELSE
			  IF(ena = '1') THEN               --continue transaction
				busy_mas <= '0';                   --continue is accepted
				addr_rw <= addr & rw;          --collect requested slave address and slv_addr
				data_tx <= data_wr(7 downto 0);            --collect requested data to write
				IF(addr_rw = addr & rw) THEN   --continue transaction with another write
					sda_int <= data_wr(bit_cnt); --write first bit of data
					state <= wr;                 --go to write byte
				ELSE                           --continue transaction with a read or new slave
					state <= start;              --go to repeated start
				END IF;
              ELSE                             --complete transaction
				state <= stop;                 --go to stop bit
              END IF;
			END IF;
		  WHEN slv_ack3 =>                   --slave acknowledge bit (write)
			IF ack_error='1' then
			  state<=stop;
			ELSE
				busy_mas <= '0';                   --continue is accepted
				addr_rw <= addr & rw;          --collect requested slave address and slv_addr
				data_tx <= data_wr(15 downto 8);            --collect requested data to write
				sda_int <= data_wr(15); --write first bit of data
				state <= wr2;                 --go to write byte
			END IF;
		  WHEN wr2 =>                         --write byte of transaction
            busy_mas <= '1';                     --resume busy if continuous mode
            IF(bit_cnt = 0) THEN             --write byte transmit finished
              sda_int <= '1';                --release sda for slave acknowledge
              bit_cnt <= 7;                  --reset bit counter for "byte" states
			  state<=slv_ack4;
            ELSE                             --next clock cycle of write state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= data_tx(bit_cnt-1); --write next bit to bus
              state <= wr2;                   --continue writing
            END IF;
		  WHEN slv_ack4 =>                   --slave acknowledge bit (write)
			IF ack_error='1' then
			  state<=stop;
			ELSE
				busy_mas <= '0';                   --continue is accepted
				addr_rw <= addr & rw;          --collect requested slave address and slv_addr
				data_tx <= data_wr(23 downto 16);            --collect requested data to write
				sda_int <= data_wr(23); --write first bit of data
				state <= wr3;                 --go to write byte
			END IF;
		  WHEN wr3 =>                         --write byte of transaction
            busy_mas <= '1';                     --resume busy if continuous mode
            IF(bit_cnt = 0) THEN             --write byte transmit finished
              sda_int <= '1';                --release sda for slave acknowledge
              bit_cnt <= 7;                  --reset bit counter for "byte" states
			  state<=slv_ack5;
            ELSE                             --next clock cycle of write state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= data_tx(bit_cnt-1); --write next bit to bus
              state <= wr3;                   --continue writing
            END IF;
		  WHEN slv_ack5 =>                   --slave acknowledge bit (write)
			IF ack_error='1' then
			  state<=stop;
			ELSE
				busy_mas <= '0';                   --continue is accepted
				addr_rw <= addr & rw;          --collect requested slave address and slv_addr
				data_tx <= data_wr(31 downto 24);            --collect requested data to write
				sda_int <= data_wr(31); --write first bit of data
				state <= wr4;                 --go to write byte
			END IF;
		  WHEN wr4 =>                         --write byte of transaction
            busy_mas <= '1';                     --resume busy if continuous mode
            IF(bit_cnt = 0) THEN             --write byte transmit finished
              sda_int <= '1';                --release sda for slave acknowledge
              bit_cnt <= 7;                  --reset bit counter for "byte" states
			  state<=slv_ack6;
            ELSE                             --next clock cycle of write state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= data_tx(bit_cnt-1); --write next bit to bus
              state <= wr4;                   --continue writing
            END IF;
		  WHEN slv_ack6 =>                   --slave acknowledge bit (write)
			  state<=stop;
          WHEN mstr_ack =>                   --master acknowledge bit after a read
            IF(ena = '1') THEN               --continue transaction
              busy_mas <= '0';                   --continue is accepted and data received is available on bus
              addr_rw <= addr & rw;          --collect requested slave address and slv_addr
              data_tx <= data_wr(7 downto 0);            --collect requested data to write
              IF(addr_rw = addr & rw) THEN   --continue transaction with another read
                sda_int <= '1';              --release sda from incoming data
				state <= rd2;
              ELSE                           --continue transaction with a write or new slave
                state <= start;              --repeated start
              END IF;    
            ELSE                             --complete transaction
              state <= stop;                 --go to stop bit
            END IF;
		  WHEN rd2 =>                         --read byte of transaction
            busy_mas <= '1';                     --resume busy if continuous mode
            IF(bit_cnt = 0) THEN             --read byte receive finished
              sda_int <= '0';              --acknowledge the byte has been received
              bit_cnt <= 7;                  --reset bit counter for "byte" states
              data_rd(15 downto 8) <= data_rx;            --output received data
              state <= mstr_ack2;             --go to master acknowledge
            ELSE                             --next clock cycle of read state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              state <= rd2;                   --continue reading
            END IF;
		  WHEN mstr_ack2 =>                   --master acknowledge bit after a read
              busy_mas <= '0';                   --continue is accepted and data received is available on 
              sda_int <= '1';              --release sda from incoming data
			  state <= rd3; 
		   WHEN rd3 =>                         --read byte of transaction
            busy_mas <= '1';                     --resume busy if continuous mode
            IF(bit_cnt = 0) THEN             --read byte receive finished
              sda_int <= '0';              --acknowledge the byte has been received
              bit_cnt <= 7;                  --reset bit counter for "byte" states
              data_rd(23 downto 16) <= data_rx;            --output received data
              state <= mstr_ack3;             --go to master acknowledge
            ELSE                             --next clock cycle of read state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              state <= rd3;                   --continue reading
            END IF;
		  WHEN mstr_ack3 =>                   --master acknowledge bit after a read
              busy_mas <= '0';                   --continue is accepted and data received is available on 
              sda_int <= '1';              --release sda from incoming data
			  state <= rd4; 
		   WHEN rd4 =>                         --read byte of transaction
            busy_mas <= '1';                     --resume busy if continuous mode
            IF(bit_cnt = 0) THEN             --read byte receive finished
              sda_int <= '0';              --acknowledge the byte has been received
              bit_cnt <= 7;                  --reset bit counter for "byte" states
              data_rd(31 downto 24) <= data_rx;            --output received data
              state <= mstr_ack4;             --go to master acknowledge
            ELSE                             --next clock cycle of read state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              state <= rd4;                   --continue reading
            END IF;
		  WHEN mstr_ack4 =>                   --master acknowledge bit after a read
              busy_mas <= '0';                   --continue is accepted and data received is available on 
              sda_int <= '1';              --release sda from incoming data
			  state <= stop; 
          WHEN stop =>                       --stop bit of transaction
            busy_mas <= '0';                     --unflag busy
            state <= ready;                  --go to idle state
		  
        END CASE;    
      ELSIF(data_clk = '0' AND data_clk_prev = '1') THEN  --data clock falling edge
        CASE state IS
          WHEN start =>                  
            IF(scl_ena = '0') THEN                  --starting new transaction
              scl_ena <= '1';                       --enable scl output
              ack_error <= '0';                     --reset acknowledge error output
            END IF;
          WHEN slv_ack1 =>                          --receiving slave acknowledge (slv_addr)
            IF(sda /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
          WHEN rd =>                                --receiving slave data
		  IF (sda='Z') THEN
			data_rx(bit_cnt) <= '1';
		  ELSE
            data_rx(bit_cnt) <= sda;                --receive current slave data bit
		  END IF;
		  WHEN rd2 =>                                --receiving slave data
		  IF (sda='Z') THEN
			data_rx(bit_cnt) <= '1';
		  ELSE
            data_rx(bit_cnt) <= sda;                --receive current slave data bit
		  END IF;
		  WHEN rd3 =>                                --receiving slave data
		  IF (sda='Z') THEN
			data_rx(bit_cnt) <= '1';
		  ELSE
            data_rx(bit_cnt) <= sda;                --receive current slave data bit
		  END IF;
		  WHEN rd4 =>                                --receiving slave data
		  IF (sda='Z') THEN
			data_rx(bit_cnt) <= '1';
		  ELSE
            data_rx(bit_cnt) <= sda;                --receive current slave data bit
		  END IF;
          WHEN slv_ack2 =>                          --receiving slave acknowledge (write)
            IF(sda /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
		  WHEN slv_ack3 =>                          --receiving slave acknowledge (write)
            IF(sda /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
		  WHEN slv_ack4 =>                          --receiving slave acknowledge (write)
            IF(sda /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
		  WHEN slv_ack5 =>                          --receiving slave acknowledge (write)
            IF(sda /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
		  WHEN slv_ack6 =>                          --receiving slave acknowledge (write)
            IF(sda /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
          WHEN stop =>
            scl_ena <= '0';                         --disable scl
          WHEN OTHERS =>
            NULL;
        END CASE;
      END IF;
	  if (state=slv_addr or state=wr) then
		if(scl_clk='1' and sda='0') then
			if (sda_int='1') then
				state<=ready;
				bit_cnt<=7;
				arb_lost<='1';
				scl_ena<='0';
				sda_int<='1';
				--busy<='0';
			end if;
		end if;
	  end if;
	  
	 
			
    END IF;
  END PROCESS; 
  
  PROCESS(CLK)
  BEGIN
	IF (clk'event and clk='1') THEN
		 if scl/='0' then
		if (sda='0' and sda_prev/='0') then
			busy_b<='1';
		elsif (sda_prev='0' and sda/='0') then
			busy_b<='0';
		end if;
	  end if;
	END IF;
  END PROCESS;

  --set sda output
  WITH state SELECT
    sda_ena_n <= data_clk_prev WHEN start,     --generate start condition
                 NOT data_clk_prev WHEN stop,  --generate stop condition
                 sda_int WHEN OTHERS;          --set to internal sda signal    
      
  --set scl and sda outputs
  scl <= '0' WHEN (scl_ena = '1' AND scl_clk = '0') ELSE 'Z';
  sda <= '0' WHEN sda_ena_n = '0' ELSE 'Z';
  
  busy<=busy_b or busy_mas;
  
END logic;