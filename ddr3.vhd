LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD_UNSIGNED.ALL;

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
         DDR3_bank : OUT STD_LOGIC_VECTOR (2 DOWNTO 0);
         wstep : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
         DDR3_addr : OUT STD_LOGIC_VECTOR (13 DOWNTO 0);
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
                    cycle <= d"31" WHEN (cycle = d"31") ELSE (cycle + 1);
                    tick <= '1' WHEN (tick_count = 1);
                    tick_count <= "0" WHEN (tick_count = 0) ELSE (tick_count - 1);

                    FOR i IN 0 TO 3 LOOP
                        (nRAS(i), nCAS(i), nWE(i)) <= CMD_NOP;
                    END LOOP;

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
                                tick_count <= 500 * TO_STD_LOGIC_VECTOR(USEC, 17) + 20;
                                state <= CKE_WAIT;
                            END IF;
                        WHEN (CKE_WAIT & XXXXX) => IF (tick_count = 15) THEN
                                DDR3_cke <= '1';
                            END IF;
                            IF tick THEN
                                state <= CONFIG;
                                cycle <= "0";
                            END IF;
                        WHEN (CONFIG & 00000) => nRAS(0) & nCAS(0) & nWE(0) <= CMD_SMR;
                            BA(0) & A(0)(12 DOWNTO 0) <= MR2;
                        WHEN (CONFIG & MRD / 4) => nRAS(0) & nCAS(0) & nWE(0) <= CMD_SMR;
                            BA(0) & A(0)(12 DOWNTO 0) <= MR3;
                        WHEN (CONFIG & NRD / 2) => nRAS(0) & nCAS(0) & nWE(0) <= CMD_SMR;
                            BA(0) & A(0)(12 DOWNTO 0) <= MR1;
                        WHEN (CONFIG & MRD * 3 / 4) => nRAS(0) & nCAS(0) & nWE(0) <= CMD_SMR;
                            BA(0) & A(0)(12 DOWNTO 0) <= MR0;
                        WHEN (CONFIG & MRD * 3 / 4 + MD / 4 + 1) => nRAS(0) & nCAS(0) & nWE(0) <= CMD_ZQCL;
                            A(0)(10) <= 1;
                            tick_count <= 514;
                            state <= ZQCL;
                        WHEN (ZQCL & XXXXX) => IF tick THEN
                            state <= WRITE_LEVEL;
                            cycle <= 0;
                        WHEN (IDLE & XXXXX) => IF (rd OR wr) THEN
                                    (nRAS(0), nCAS(0), nWE(0)) <= CMD_BA;
                                    BA(0) <= addr(ROW_WIDTH + COL_WIDTH + BANK_WIDTH - 1 DOWNTO ROW_WIDTH + COL_WIDTH);
                                    A(0) <= addr(ROW_WIDTH + COL_WIDTH - 1 DOWNTO COL_WIDTH);
                                    state <= RED WHEN rd ELSE WRTE;
                                    IF rd THEN cnt_read <= x"FF" WHEN cnt_read = x"FF" ELSE cnt_read + 1 END IF;
                                    IF wr THEN cnt_write <= x"FF" WHEN cnt_write = x"FF" ELSE cnt_write + 1 END IF;
                                    cycle <= '1';
                                    busy <= '1';
                                    IF rd THEN dqs_hold <= '1' END IF;
                                ELSIF DDR3_refresh THEN
                                    (nRAS(0), nCAS(0), nWE(0)) <= CMD_AR;
                                    state <= REFRESH;
                                    cycle <= '1';
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
                            dq_out(4) <= '0';
                            FOR i 5 TO 7 LOOP
                                dq_out(i) <= din;
                            END LOOP;
                            dq_oen <= b"1100";
                            dm_out(5) <= NOT(addr(1 DOWNTO 0) = 0);
                            dm_out(6) <= NOT(addr(1 DOWNTO 0) = 1);
                            dm_out(7) <= NOT(addr(1 DOWNTO 0) = 2);
                        WHEN (WRTE & (RCD + CWL) / 4 + 1) => dqs_out <= b"10000000";
                            dqs_oen <= b"0111";
                            dq_out(0) <= din;
                            dq_out(1) <= '0';
                            dq_oen <= b"0111";
                            dm_out(0) <= NOT(addr(1 DOWNTO 0) = 3);
                        WHEN (WRTE & 6) => busy <= '0';
                            state <= IDLE;
                        WHEN (REFRESH & RC / 4) => busy <= '0';
                            state <= IDLE;
                        WHEN (WRITE_LEVEL & "00000") => nRAS(0) & nCAS(0) & nWE(0) <= CMD_SMR;
                            BA(0) & A(0)(12 DOWNTO 0) <= MR1 OR b"10000000";
                            wlevelcnt <= '0';
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
                            IF NOT DDR3_dq(0) OR DDR3_dq(8) THEN
                                wstep <= wstep + 1;
                                wlevelcnt <= '0';
                                cycle <= WLMRD / 4 - 1;
                            ELSE
                                wlevelcnt <= wlevelcnt + 1;
                                IF wlevelcnt = WLEVELCOUNT - 1 THEN
                                    wleveldone <= '1';
                                    nRAS(0) & nCAS(0) & nWE(0) <= CMD_SMR;
                                    BA(0) & A(0)(12 DOWNTO 0) <= MR1;
                                ELSE
                                    cycle <= WLMRD / 4 - 1;
                                END IF;
                            END IF;
                        WHEN (WRITE_LEVEL & (WLMRD + MRD) / 4 + 6) => nRAS(0) & nCAS(0) & nWE(0) <= CMD_SMR;
                            BA(0) & A(0)(12 DOWNTO 0) <= MR2WR;
                            END IF;
                        WHEN (WRITE_LEVEL & (WLMRD + MRD) / 4 + 6) => cycle <= '0';
                            state <= READ_CALIB;
                        WHEN (READ_CALIB & "00000") => nRAS(0) & nCAS(0) & nWE(0) <= CMD_BA;
                            BA(0) <= '0';
                            A(0) <= '0';
                            rcalibcnt <= '0';
                        WHEN (READ_CALIB & RCD / 4) => nRAS(2) & nCAS(2) & nWE(2) <= CMD_R;
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
                        FOR i IN 0 TO 3 LOOP
                            (nRAS(i), nCAS(i), nWE(i)) <= CMD_NOP;
                        END LOOP;
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
END ARCHITECTURE;
