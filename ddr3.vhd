LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY DDR3 IS
    GENERIC(ROW_WIDTH : INTEGER := 13;
            COL_WIDTH : INTEGER := 10;
            BANK_WIDTH : INTEGER := 3;
            CAS : INTEGER := 7;
            CWL : INTEGER := 6;
            RCD : INTEGER := 8;
            REC : INTEGER := 8;
            MRD : INTEGER := 8;
            RP : INTEGER := 8;
            RC :INTEGER := 26;
            MD : INTEGER := 12
            );
    PORT(ck, pclk, fclk, reset, rd, wr, DDR3_refresh : IN STD_LOGIC;
         addr : IN STD_LOGIC_VECTOR (BANK_WIDTH + ROW_WIDTH + COL_WIDTH - 1 DOWNTO 0);
         din : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
         DDR3_odt, DDR3_cke, DDR3_we, DDR3_cas, DDR3_ras, DDR3_cs, DDR3_ck, DDR3_reset, write_done, read_done : OUT STD_LOGIC;
         data_ready : OUT STD_LOGIC := '0';
         busy : OUT STD_LOGIC := '1';
         rclkpos : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
         DDR3_dm : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
         DDR3_dqs : INOUT STD_LOGIC_VECTOR (1 DOWNTO 0);
         rclksel : OUT STD_LOGIC_VECTOR (2 DOWNTO 0);
         wstep : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
         DDR3_dq : INOUT STD_LOGIC_VECTOR (15 DOWNTO 0);
         dout : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
         debug : OUT STD_LOGIC_VECTOR (63 DOWNTO 0);
         dout128 : OUT STD_LOGIC_VECTOR (127 DOWNTO 0);
         DDR3_a : OUT STD_LOGIC_VECTOR (ROW_WIDTH - 1 DOWNTO 0);
         DDR3_ba : OUT STD_LOGIC_VECTOR (BANK_WIDTH - 1 DOWNTO 0));
END ENTITY;

ARCHITECTURE rtl OF DDR3 IS

CONSTANT WLMRD : INTEGER := 44;
CONSTANT SERDES : INTEGER := 16;
CONSTANT USEC : INTEGER := 134;

SIGNAL dlllock : STD_LOGIC;
SIGNAL rstlock : STD_LOGIC;
SIGNAL resetDelay : STD_LOGIC;

TYPE dqout IS ARRAY (15 DOWNTO 0) OF STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL dq_out : dqout;

TYPE dqin IS ARRAY (15 DOWNTO 0) OF STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL dq_in : dqin;

SIGNAL dq_oen : STD_LOGIC_VECTOR (0 TO 3);
SIGNAL dqs_out : STD_LOGIC_VECTOR (0 TO 7);
SIGNAL dqs_oen : STD_LOGIC_VECTOR (0 TO 3);
SIGNAL dm_out : STD_LOGIC_VECTOR (0 TO 7);

TYPE BYTE IS ARRAY (7 DOWNTO 0) OF BIT;
TYPE NIB IS ARRAY (3 DOWNTO 0) OF BIT;
TYPE FIVEB IS ARRAY (4 DOWNTO 0) OF BIT;

SIGNAL cnt_read : STD_LOGIC_VECTOR (7 DOWNTO 0) := (OTHERS => '0');
SIGNAL cnt_write : STD_LOGIC_VECTOR (7 DOWNTO 0) := (OTHERS => '0');

SIGNAL state : STD_LOGIC_VECTOR (3 DOWNTO 0);

SIGNAL nRAS, nCAS, nWE : STD_LOGIC_VECTOR (3 DOWNTO 0);

TYPE ddr3a IS ARRAY (ROW_WIDTH - 1 DOWNTO 0) OF STD_LOGIC_VECTOR (3 DOWNTO 0);
SIGNAL A : ddr3a;

TYPE ddr3ba IS ARRAY (2 DOWNTO 0) OF STD_LOGIC_VECTOR (3 DOWNTO 0);
SIGNAL BA : ddr3ba;

CONSTANT RST_WAIT : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"0";
CONSTANT CKE_WAIT : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"1";
CONSTANT CONFIG : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"2";
CONSTANT ZQCL : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"3";
CONSTANT IDLE : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"4";
CONSTANT RED : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"5";
CONSTANT WRTE : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"6";
CONSTANT REFRESH : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"7";
CONSTANT WRITE_LEVEL : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"8";
CONSTANT READ_CALIB : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"9";

SIGNAL cycle : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL tick : STD_LOGIC;
SIGNAL tick_count : STD_LOGIC_VECTOR (16 DOWNTO 0) := d"50000";

CONSTANT CMD_SMR : STD_LOGIC_VECTOR (2 DOWNTO 0) := (OTHERS => '0');
CONSTANT CMD_AR : STD_LOGIC_VECTOR (2 DOWNTO 0) := "1";
CONSTANT CMD_PC : STD_LOGIC_VECTOR (2 DOWNTO 0) := b"010";
CONSTANT CMD_BA : STD_LOGIC_VECTOR (2 DOWNTO 0) := b"011";
CONSTANT CMD_W : STD_LOGIC_VECTOR (2 DOWNTO 0) := b"100";
CONSTANT CMD_R : STD_LOGIC_VECTOR (2 DOWNTO 0) := b"101";
CONSTANT CMD_ZQCL : STD_LOGIC_VECTOR (2 DOWNTO 0) := b"110";
CONSTANT CMD_NOP : STD_LOGIC_VECTOR (2 DOWNTO 0) := (OTHERS => '1');

CONSTANT MBL : STD_LOGIC_VECTOR (1 DOWNTO 0) := "1";
CONSTANT MCAS : STD_LOGIC_VECTOR (3 DOWNTO 0) := b"0110";
CONSTANT MCWL : STD_LOGIC_VECTOR (2 DOWNTO 0) := "1";
CONSTANT MWR : STD_LOGIC_VECTOR (2 DOWNTO 0) := b"100";
CONSTANT MDLLR : STD_LOGIC_VECTOR (0 TO 0) := "1";
CONSTANT MRTTNOM : STD_LOGIC_VECTOR (2 DOWNTO 0) := (OTHERS => '0');
CONSTANT MRTTWR : STD_LOGIC_VECTOR (1 DOWNTO 0) := "1";
CONSTANT MRTTWROFF : STD_LOGIC_VECTOR (1 DOWNTO 0) := (OTHERS => '0');
CONSTANT MDRIVE : STD_LOGIC_VECTOR (1 DOWNTO 0) := (OTHERS => '0');
CONSTANT MAL : STD_LOGIC_VECTOR (1 DOWNTO 0) := (OTHERS => '0');

SIGNAL MR0 : STD_LOGIC_VECTOR (15 DOWNTO 0);
SIGNAL MR1 : STD_LOGIC_VECTOR (15 DOWNTO 0);
SIGNAL MR2 : STD_LOGIC_VECTOR (15 DOWNTO 0);
SIGNAL MR2WR : STD_LOGIC_VECTOR (15 DOWNTO 0);
SIGNAL MR3 : STD_LOGIC_VECTOR (15 DOWNTO 0);

SIGNAL wleveldone : STD_LOGIC := '0';
SIGNAL wlevelcnt : STD_LOGIC_VECTOR := (OTHERS => '0');

SIGNAL dqs_read : STD_LOGIC_VECTOR := (OTHERS => '0');
SIGNAL dqs_hold : STD_LOGIC := '0';
SIGNAL rburst : STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL rburstseen : STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL rcalibdone : STD_LOGIC := '0';
SIGNAL rcalibcnt : STD_LOGIC_VECTOR (3 DOWNTO 0) := (OTHERS => '0');

CONSTANT WLEVELCOUNT : INTEGER := 1;
CONSTANT RCALIBCOUNT : INTEGER := 8;

SIGNAL dllstep : STD_LOGIC_VECTOR (7 DOWNTO 0);

TYPE dqs_addr IS ARRAY (2 DOWNTO 0) OF STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL dqs_waddr, dqs_raddr : dqs_addr;

SIGNAL clk_dqsr : STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL clk_dqsw : STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL clk_dqsw270 : STD_LOGIC_VECTOR (1 DOWNTO 0);

SIGNAL dq_buf, dq_buf_oen : STD_LOGIC_VECTOR (15 DOWNTO 0);
SIGNAL dqs_buf, dqs_buf_oen, dqs_buf_delayed : STD_LOGIC_VECTOR (1 DOWNTO 0);

COMPONENT DQS 
    GENERIC( 
        FIFO_MODE_SEL:bit:='0'; 
        RD_PNTR : bit_vector:="000"; 
        DQS_MODE:string:="X1"; 
        HWL:string:="false"; 
        GSREN : string:="false" 
    ); 
    PORT( 
        DQSIN,PCLK,FCLK,RESET:IN std_logic; 
        READ:IN std_logic_vector(3 downto 0); 
        RCLKSEL:IN std_logic_vector(2 downto 0); 
        DLLSTEP,WSTEP:IN std_logic_vector(7 downto 0); 
        RLOADN,RMOVE,RDIR,HOLD:IN std_logic; 
        WLOADN,WMOVE,WDIR:IN std_logic; 
        DQSR90,DQSW0,DQSW270:OUT std_logic; 
        RPOINT, WPOINT:OUT std_logic_vector(2 downto 0); 
        RVALID,RBURST,RFLAG,WFLAG:OUT std_logic 
    ); 
END COMPONENT;

COMPONENT DLL 
    GENERIC( 
        DLL_FORCE:integer:=0; 
        DIV_SEL:bit:='1'; 
        CODESCAL:STRING:="000"; 
        SCAL_EN:STRING:="true" 
    ); 
    PORT( 
        CLKIN:IN std_logic; 
        STOP:IN std_logic; 
        RESET:IN std_logic; 
        UPDNCNTL:IN std_logic; 
        LOCK:OUT std_logic; 
        STEP:OUT std_logic_vector(7 downto 0) 
    ); 
END COMPONENT;

COMPONENT OSER8_MEM 
    GENERIC (GSREN:string:="false"; 
        LSREN:string:="true"; 
        HWL:string:="false"; 
        TXCLK_POL:bit:='0'; 
        TCLK_SOURCE:string:="DQSW" 
    ); 
    PORT( 
        Q0:OUT std_logic; 
        Q1:OUT std_logic;  
        D0:IN std_logic; 
        D1:IN std_logic; 
        D2:IN std_logic; 
        D3:IN std_logic; 
        D4:IN std_logic; 
        D5:IN std_logic; 
        D6:IN std_logic; 
        D7:IN std_logic; 
        TX0:IN std_logic; 
        TX1:IN std_logic; 
        TX2:IN std_logic; 
        TX3:IN std_logic; 
        TCLK:IN std_logic; 
        FCLK:IN std_logic; 
        PCLK:IN std_logic; 
        RESET:IN std_logic 
    ); 
END COMPONENT;

COMPONENT IDES8_MEM 
    GENERIC (GSREN:string:="false"; 
        LSREN:string:="true" 
    );
    PORT( 
        Q0:OUT std_logic; 
        Q1:OUT std_logic;  
        Q2:OUT std_logic; 
        Q3:OUT std_logic; 
        Q4:OUT std_logic; 
        Q5:OUT std_logic; 
        Q6:OUT std_logic; 
        Q7:OUT std_logic; 
        D:IN std_logic; 
        ICLK:IN std_logic; 
        FCLK:IN std_logic; 
        PCLK:IN std_logic; 
        WADDR:IN std_logic_vector(2 downto 0); 
        RADDR:IN std_logic_vector(2 downto 0); 
        CALIB:IN std_logic; 
        RESET:IN std_logic 
    ); 
END COMPONENT;

COMPONENT OSER8 
    GENERIC (GSREN:string:="false"; 
        LSREN:string:="true";
        HWL:string:="false"; 
        TXCLK_POL:bit:='0' 
         ); 
   PORT( 
        Q0:OUT std_logic; 
        Q1:OUT std_logic; 
        D0:IN std_logic; 
        D1:IN std_logic; 
        D2:IN std_logic; 
        D3:IN std_logic; 
        D4:IN std_logic; 
        D5:IN std_logic; 
        D6:IN std_logic; 
        D7:IN std_logic; 
        TX0:IN std_logic; 
        TX1:IN std_logic;  
        TX2:IN std_logic; 
        TX3:IN std_logic; 
        FCLK:IN std_logic; 
        PCLK:IN std_logic; 
        RESET:IN std_logic 
    ); 
END COMPONENT; 

BEGIN
    PROCESS(ALL)
        BEGIN
            rstlock <= reset AND dlllock;
            DDR3_reset <= rstlock AND resetDelay;
            DDR3_odt <= '1';
            debug <= cnt_write & cnt_read & state;
            DDR3_ck <= ck;
            DDR3_cs <= '0';
            dout128 <= dq_in(0) & dq_in(1) & dq_in(2) & dq_in(3) & dq_in(4) & dq_in(5) & dq_in(6) & dq_in(7);                 
            dout <= dq_in(4);
            
            MR0 <= "000" & "0" & MWR & MDLLR & "0" & MCAS(3 DOWNTO 1) & "0" & MCAS(0) & MBL;
            MR1 <= "001" & "000" & MRTTNOM(2) & "00" & MRTTNOM(1) & MDRIVE(1) & MAL & MRTTNOM(0) & MDRIVE(0) & "0";
            MR2 <= "010" & "00" & MRTTWROFF & "000" & MCWL & "000";
            MR2WR <= "010" & "00" & MRTTWR & "000" & MCWL & "000";
            MR3 <= "011" & "0000000000000";

            write_done <= wleveldone;
            read_done <= rcalibdone;

            IF RISING_EDGE(pclk) THEN
                IF (rstlock) THEN
                    cycle <= d"31" WHEN (cycle = d"31") ELSE (cycle + '1');
                    tick <= '1' WHEN (tick_count = d"1");
                    tick_count <= "0" WHEN (tick_count = d"0") ELSE (tick_count - '1');

                    (nRAS(0), nCAS(0), nWE(0)) <= CMD_NOP;
                    (nRAS(1), nCAS(1), nWE(1)) <= CMD_NOP;
                    (nRAS(2), nCAS(2), nWE(2)) <= CMD_NOP;
                    (nRAS(3), nCAS(3), nWE(3)) <= CMD_NOP;

                    FOR i IN 0 TO 3 LOOP
                        A(i) <= "0";
                        BA(i) <= "0";
                    END LOOP;

                    dm_out <= (OTHERS => '1');
                    dqs_oen <= (OTHERS => '1');
                    dq_oen <= (OTHERS => '1');
                    dqs_hold <= '0';

                    CASE (state & cycle) IS
                        WHEN (RST_WAIT & XXXXX) => IF tick THEN
                                resetDelay <= '1';
                                tick_count <= d"500" * STD_LOGIC_VECTOR(TO_UNSIGNED(USEC, 17)) + 20;
                                state <= CKE_WAIT;
                            END IF;
                        WHEN (CKE_WAIT & XXXXX) => IF (tick_count = d"15") THEN
                                DDR3_cke <= '1';
                            END IF;
                            IF tick THEN
                                state <= CONFIG;
                                cycle <= "0";
                            END IF;
                        WHEN (CONFIG & 00000) => (nRAS(0), nCAS(0), nWE(0)) <= CMD_SMR;
                            BA(0) <= "0" & MR2(15 DOWNTO 13);
                            A(0)(12 DOWNTO 0) <= MR2(12 DOWNTO 0);
                        WHEN (CONFIG & MRD / 4) => (nRAS(0), nCAS(0), nWE(0)) <= CMD_SMR;
                            BA(0) <= "0" & MR3(15 DOWNTO 13);
                            A(0)(12 DOWNTO 0) <= MR3(12 DOWNTO 0);
                        WHEN (CONFIG & NRD / 2) => (nRAS(0), nCAS(0), nWE(0)) <= CMD_SMR;
                            BA(0) <= "0" & MR1(15 DOWNTO 13);
                            A(0)(12 DOWNTO 0) <= MR1(12 DOWNTO 0);
                        WHEN (CONFIG & MRD * 3 / 4) => (nRAS(0), nCAS(0), nWE(0)) <= CMD_SMR;
                            BA(0) <= "0" & MR0(15 DOWNTO 13);
                            A(0)(12 DOWNTO 0) <= MR0(12 DOWNTO 0);
                        WHEN (CONFIG & MRD * 3 / 4 + MD / 4 + 1) => (nRAS(0), nCAS(0), nWE(0)) <= CMD_ZQCL;
                            A(0)(10) <= '1';
                            tick_count <= d"514";
                            state <= ZQCL;
                        WHEN (ZQCL & XXXXX) => IF tick THEN
                            state <= WRITE_LEVEL;
                            cycle <= "0";
                        END IF;
                        WHEN (IDLE & XXXXX) => IF (rd OR wr) THEN
                                    (nRAS(0), nCAS(0), nWE(0)) <= CMD_BA;
                                    BA(0) <= addr(ROW_WIDTH + COL_WIDTH + BANK_WIDTH - 1 DOWNTO ROW_WIDTH + COL_WIDTH);
                                    A(0) <= addr(ROW_WIDTH + COL_WIDTH - 1 DOWNTO COL_WIDTH);
                                    state <= RED WHEN rd ELSE WRTE;
                                    IF rd THEN cnt_read <= x"FF" WHEN cnt_read = x"FF" ELSE cnt_read + '1'; END IF;
                                    IF wr THEN cnt_write <= x"FF" WHEN cnt_write = x"FF" ELSE cnt_write + '1'; END IF;
                                    cycle <= "1";
                                    busy <= '1';
                                    IF rd THEN dqs_hold <= '1'; END IF;
                                ELSIF DDR3_refresh THEN
                                    (nRAS(0), nCAS(0), nWE(0)) <= CMD_AR;
                                    state <= REFRESH;
                                    cycle <= "1";
                                    busy <= '1';
                            END IF;
                        WHEN (RED & (RCD / 4)) => (nRAS(RCD MOD 4), nCAS(RCD MOD 4), nWE(RCD MOD 4)) <= CMD_R;
                            BA(RCD MOD 4) <= addr(ROW_WIDTH + COL_WIDTH + BANK_WIDTH - 1 DOWNTO ROW_WIDTH + COL_WIDTH);
                            A(RCD MOD 4)(12) <= '1';
                            A(RCD MOD 4)(10) <= '1';
                            A(RCD MOD 4)(9 DOWNTO 0) <= addr(COL_WIDTH - 1 DOWNTO 0);
                            dqs_hold <= '1';
                        WHEN (RED & (RCD + CAS + SERDES) / 4 + 1) => data_ready <= '1';
                        WHEN (RED & (RCD + CAS + SERDES) / 4 + 2) => data_ready <= '0';
                            busy <= '0';
                            state <= IDLE;
                        WHEN (WRTE & RCD / 4) => (nRAS(RCD MOD 4), nCAS(RCD MOD 4), nWE(RCD MOD 4)) <= CMD_W;
                            BA(RCD MOD 4) <= addr(ROW_WIDTH + COL_WIDTH + BANK_WIDTH - 1 DOWNTO ROW_WIDTH + COL_WIDTH);
                            A(RCD MOD 4)(12) <= '0';
                            A(RCD MOD 4)(10) <= '1';
                            A(RCD MOD 4)(9 DOWNTO 0) <= addr(COL_WIDTH - 1 DOWNTO 0);
                        WHEN (WRTE & (RCD + CWL) / 4) => dqs_out <= b"11111010";
                            dqs_oen <= b"1100";
                            dq_out(4) <= "0";
                            FOR i IN 5 TO 7 LOOP
                                dq_out(i) <= din;
                            END LOOP;
                            dq_oen <= b"1100";
                            dm_out(5) <= NOT(addr(1 DOWNTO 0) = b"00");
                            dm_out(6) <= NOT(addr(1 DOWNTO 0) = b"01");
                            dm_out(7) <= NOT(addr(1 DOWNTO 0) = b"10");
                        WHEN (WRTE & (RCD + CWL) / 4 + 1) => dqs_out <= b"10000000";
                            dqs_oen <= b"0111";
                            dq_out(0) <= din;
                            dq_out(1) <= "0";
                            dq_oen <= b"0111";
                            dm_out(0) <= NOT(addr(1 DOWNTO 0) = 3);
                        WHEN (WRTE & 6) => busy <= '0';
                            state <= IDLE;
                        WHEN (REFRESH & RC / 4) => busy <= '0';
                            state <= IDLE;
                        WHEN (WRITE_LEVEL & "00000") => (nRAS(0), nCAS(0), nWE(0)) <= CMD_SMR;
                            BA(0) <= "0" & MR1(15 DOWNTO 13);
                            A(0)(12 DOWNTO 0) <= MR1(12 DOWNTO 0) OR b"10000100";
                            wlevelcnt <= "0";
                            wstep <= x"12";
                        WHEN (WRITE_LEVEL & WLMRD / 4 - 1) => dqs_out(0 TO 7) <= (OTHERS => '0');
                            dqs_oen(0 TO 3) <= (OTHERS => '0');
                        WHEN (WRITE_LEVEL & WLMRD / 4 + 1) => dqs_out(0 TO 7) <= (OTHERS => '0');
                            dqs_oen(0 TO 3) <= (OTHERS => '0');
                        WHEN (WRITE_LEVEL & WLMRD / 4 + 2) => dqs_out(0 TO 7) <= (OTHERS => '0');
                            dqs_oen(0 TO 3) <= (OTHERS => '0');
                        WHEN (WRITE_LEVEL & WLMRD / 4 + 3) => dqs_out(0 TO 7) <= (OTHERS => '0');
                            dqs_oen(0 TO 3) <= (OTHERS => '0');
                        WHEN (WRITE_LEVEL & WLMRD / 4 + 4) => dqs_out(0 TO 7) <= (OTHERS => '0');
                            dqs_oen(0 TO 3) <= (OTHERS => '0');
                        WHEN (WRITE_LEVEL & WLMRD / 4 + 5) => dqs_out(0 TO 7) <= (OTHERS => '0');
                            dqs_oen(0 TO 3) <= (OTHERS => '0');
                        WHEN (WRITE_LEVEL & WLMRD / 4) => dqs_out (0 TO 7) <= b"10101010";
                            dqs_oen(0 TO 3) <= (OTHERS => '0');
                        WHEN (WRITE_LEVEL & WLMRD / 4 + 6) => dqs_out (0 TO 7) <= (OTHERS => '0');  
                            dqs_oen(0 TO 3) <= (OTHERS => '0');
                            IF NOT DDR3_dq(0) OR NOT DDR3_dq(8) THEN
                                wstep <= wstep + 1;
                                wlevelcnt <= "0";
                                cycle <= TO_UNSIGNED(WLMRD, 5) / 4 - 1;
                            ELSE
                                wlevelcnt <= wlevelcnt + 1;
                                IF wlevelcnt = WLEVELCOUNT - 1 THEN
                                    wleveldone <= '1';
                                    (nRAS(0), nCAS(0), nWE(0)) <= CMD_SMR;
                                    BA(0) <= "0" & MR1(15 DOWNTO 13);
                                    A(0)(12 DOWNTO 0) <= MR1(12 DOWNTO 0);
                                ELSE
                                    cycle <= WLMRD / 4 - 1;
                                END IF;
                            END IF;
                        WHEN (WRITE_LEVEL & (WLMRD + MRD) / 4 + 6) => (nRAS(0), nCAS(0), nWE(0)) <= CMD_SMR;
                            BA(0) & A(0)(12 DOWNTO 0) <= MR2WR;
                            END IF;
                        WHEN (WRITE_LEVEL & (WLMRD + MRD) / 4 + 6) => cycle <= '0';
                            state <= READ_CALIB;
                        WHEN (READ_CALIB & "00000") => (nRAS(0), nCAS(0), nWE(0)) <= CMD_BA;
                            BA(0) <= '0';
                            A(0) <= '0';
                            rcalibcnt <= '0';
                        WHEN (READ_CALIB & RCD / 4) => (nRAS(2), nCAS(2), nWE(2)) <= CMD_R;
                            A(2)(12) <= '1';
                            A(2)(10) <= '0';
                            A(2)(9 DOWNTO 0) <= (OTHERS => '0');
                        WHEN (READ_CALIB & RCD / 4 + 10) => IF (rburstseen /= b"11") THEN
                                rclksel <= rclksel + 1;
                                rclkpos <= rclkpos + 1 WHEN rclksel = 7 ELSE rclkpos;
                                rcalibcnt <= '0';
                                cycle <= RCD / 4;
                            ELSE
                                rcalibcnt <= rcalibcnt + 1;
                                IF rcalibcnt = RCALIBCOUNT - 1 THEN
                                    rcalibdone <= '1';
                                    nRAS(0) & nCAS(0) & nWE(0) <= CMD_PC;
                                    BA(0) <= '0';
                                    A(0) <= '0';
                                ELSE
                                    cycle <= RCD / 4;
                                END IF;
                            END IF;
                        WHEN (READ_CALIB & RCD / 4 + 10 + RP / 4) => busy <= '0';
                            state <= IDLE;
                    END CASE;
                    ELSIF (NOT rstlock) THEN
                        busy <= '1';
                        data_ready <= '0';
                        DDR3_cke <= '0';
                        (nRAS(0), nCAS(0), nWE(0)) <= CMD_NOP;
                        (nRAS(1), nCAS(1), nWE(1)) <= CMD_NOP;
                        (nRAS(2), nCAS(2), nWE(2)) <= CMD_NOP;
                        (nRAS(3), nCAS(3), nWE(3)) <= CMD_NOP;
                        tick_count <= 60000;
                        tick <= '0';
                        cycle <= '0';
                        wlevelcnt <= '0';
                        wleveldone <= '0';
                        wstep <= '0';
                        rcalibcnt <= '0';
                        rcalibdone <= '0';
                        rclkpos <= '0';
                        rclksel <= '0';
                        resetDelay <= '0';
                        state <= RST_WAIT;
                END IF;
            END IF;
    END PROCESS;

    PROCESS(ALL)
        BEGIN
            IF (RISING_EDGE(pclk)) THEN
                IF rburst(0) THEN
                    rburstseen(0) <= '1';
                END IF;
                IF rburst(1) THEN
                    rburstseen(1) <= '1';
                END IF;
                IF (state & cycle) = (READ_CALIB & RCD / 4) THEN
                    rburstseen <= '0';
                END IF;
                IF NOT rstlock THEN
                    rburstseen <= '0';
                END IF;
            END IF;
    END PROCESS;

    PROCESS(ALL)
        BEGIN
            IF (RISING_EDGE(pclk)) THEN
                dqs_read <= '0';
                IF (state = RED OR state = READ_CALIB AND cycle = rclkpos + RCD / 4 + 1) THEN
                    dqs_read <= b"1111";
                END IF;
            END IF;
    END PROCESS;

    delay:DLL 
        GENERIC MAP( 
            CODESCAL=>"101", 
            SCAL_EN=>"true" 
        ) 
        PORT MAP( 
            CLKIN=>fclk, 
            STOP=>0, 
            RESET=>NOT reset, 
            UPDNCNTL=>0, 
            LOCK=>dlllock, 
            STEP=>dllstep 
        );

    gen_dqs_cont : FOR i in 0 TO 1 GENERATE
       u_dqs:DQS 
            GENERIC MAP( 
                DQS_MODE=>"X4", 
                HWL=>"false"
            ) 
            PORT MAP( 
                DQSIN=>DDR3_dqs(i), 
                PCLK=>pclk, 
                FCLK=>fclk, 
                RESET=>rstlock, 
                READ=>dqs_read, 
                RCLKSEL=>rclksel, 
                DLLSTEP=>dllstep, 
                WSTEP=>wstep, 
                RLOADN=>0, 
                RMOVE=>0,  
                HOLD=>dqs_hold, 
                WLOADN=>0,  
                WMOVE=>0, , 
                DQSR90=>clk_dqsr(i),  
                DQSW0=>clk_dqsw(i),  
                DQSW270=>clk_dqsw270(i), 
                RPOINT=>dqs_raddr(i), 
                WPOINT=>dqs_waddr(i), 
                RBURST=>rburst(i)
            ); 
    END GENERATE;

    gen_dq : FOR i in 0 TO 15 GENERATE
        oser_dq:OSER8_MEM 
            GENERIC MAP (
                TCLK_SOURCE=>"DQSW270"
                ) 
            PORT MAP ( 
                Q0=>dq_buf(i), 
                Q1=>dq_buf_oen(i), 
                D0=>dq_out(0)(i), 
                D1=>dq_out(1)(i), 
                D2=>dq_out(2)(i), 
                D3=>dq_out(3)(i), 
                D4=>dq_out(4)(i), 
                D5=>dq_out(5)(i), 
                D6=>dq_out(6)(i), 
                D7=>dq_out(7)(i), 
                TX0=>dq_oen(0), 
                TX1=>dq_oen(1), 
                TX2=>dq_oen(2), 
                TX3=>dq_oen(3), 
                TCLK=>clk_dqsw270(i / 8), 
                FCLK=>fclk,  
                PCLK=>pclk, 
                RESET=>(NOT rstlock OR NOT dlllock)
            );

        DDR3_dq(i) <= 'Z' WHEN dq_buf_oen(i) ELSE dq_buf(i);

        iser_dq:IDES8_MEM 
            PORT MAP ( 
                Q0=>dq_in(0)(i), 
                Q1=>dq_in(1)(i), 
                Q2=>dq_in(2)(i), 
                Q3=>dq_in(3)(i), 
                Q4=>dq_in(4)(i), 
                Q5=>dq_in(5)(i), 
                Q6=>dq_in(6)(i), 
                Q7=>dq_in(7)(i), 
                D=>DDR3_dq(i), 
                ICLK=>clk_dqsr(i / 8), 
                FCLK=>fclk, 
                PCLK=>pclk, 
                WADDR=>dqs_waddr(i / 8), 
                RADDR=>dqs_raddr(i / 8), 
                CALIB=>0, 
                RESET=>NOT rstlock 
            );
    END GENERATE;

    gen_dqs : FOR i IN 0 TO 1 GENERATE
        oser_dqs:OSER8_MEM 
            GENERIC MAP (
                TCLK_SOURCE=>"DQSW270"
                ) 
            PORT MAP ( 
                Q0=>dqs_buf(i), 
                Q1=>dqs_buf_oen(i), 
                D0=>dqs_out(0)(i), 
                D1=>dqs_out(1)(i), 
                D2=>dqs_out(2)(i), 
                D3=>dqs_out(3)(i), 
                D4=>dqs_out(4)(i), 
                D5=>dqs_out(5)(i), 
                D6=>dqs_out(6)(i), 
                D7=>dqs_out(7)(i), 
                TX0=>dqs_oen(0), 
                TX1=>dqs_oen(1), 
                TX2=>dqs_oen(2), 
                TX3=>dqs_oen(3), 
                TCLK=>clk_dqsw(i), 
                FCLK=>fclk,  
                PCLK=>pclk, 
                RESET=>NOT rstlock
            );

        DDR3_dqs(i) <= 'Z' WHEN dqs_buf_oen(i) ELSE dqs_buf (i);

        oser_dq:OSER8_MEM 
            GENERIC MAP (
                TCLK_SOURCE=>"DQSW270"
                ) 
            PORT MAP ( 
                Q0=>DDR3_dm(i),  
                D0=>dm_out(0)(i), 
                D1=>dm_out(1)(i), 
                D2=>dm_out(2)(i), 
                D3=>dm_out(3)(i), 
                D4=>dm_out(4)(i), 
                D5=>dm_out(5)(i), 
                D6=>dm_out(6)(i), 
                D7=>dm_out(7)(i), 
                TCLK=>clk_dqsw270(i), 
                FCLK=>fclk,  
                PCLK=>pclk, 
                RESET=>NOT rstlock
            );
    END GENERATE;

    oser_nras:OSER8 
        PORT MAP ( 
              Q0=>DDR3_ras,  
              D0=>nRAS(0), 
              D1=>nRAS(1), 
              D2=>nRAS(2),  
              D3=>nRAS(3), 
              D4=>nRAS(4), 
              D5=>nRAS(5), 
              D6=>nRAS(6), 
              D7=>nRAS(7), 
              FCLK=>fclk, 
              PCLK=>pclk, 
              RESET=>NOT rstlock 
        );

    oser_ncas:OSER8 
        PORT MAP ( 
              Q0=>DDR3_cas,  
              D0=>nCAS(0), 
              D1=>nCAS(1), 
              D2=>nCAS(2),  
              D3=>nCAS(3), 
              D4=>nCAS(4), 
              D5=>nCAS(5), 
              D6=>nCAS(6), 
              D7=>nCAS(7), 
              FCLK=>fclk, 
              PCLK=>pclk, 
              RESET=>NOT rstlock 
        );

    oser_nwe:OSER8 
        PORT MAP ( 
              Q0=>DDR3_we,  
              D0=>nWE(0), 
              D1=>nWE(1), 
              D2=>nWE(2),  
              D3=>nWE(3), 
              D4=>nWE(4), 
              D5=>nWE(5), 
              D6=>nWE(6), 
              D7=>nWE(7), 
              FCLK=>fclk, 
              PCLK=>pclk, 
              RESET=>NOT rstlock 
        );

    gena : FOR i IN 0 TO ROW_WIDTH - 1 GENERATE
          oser_nwe:OSER8 
            PORT MAP ( 
                  Q0=>DDR3_a(i),  
                  D0=>A(0)(i), 
                  D1=>A(1)(i), 
                  D2=>A(2)(i),  
                  D3=>A(3)(i), 
                  D4=>A(4)(i), 
                  D5=>A(5)(i), 
                  D6=>A(6)(i), 
                  D7=>A(7)(i), 
                  FCLK=>fclk, 
                  PCLK=>pclk, 
                  RESET=>NOT rstlock 
            );
    END GENERATE;

    genba : FOR i IN 0 TO 2 GENERATE
          oser_nwe:OSER8 
            PORT MAP ( 
                  Q0=>DDR3_ba(i),  
                  D0=>BA(0)(i), 
                  D1=>BA(1)(i), 
                  D2=>BA(2)(i),  
                  D3=>BA(3)(i), 
                  D4=>BA(4)(i), 
                  D5=>BA(5)(i), 
                  D6=>BA(6)(i), 
                  D7=>BA(7)(i), 
                  FCLK=>fclk, 
                  PCLK=>pclk, 
                  RESET=>NOT rstlock 
            );
    END GENERATE;
END ARCHITECTURE;
