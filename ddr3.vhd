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

SIGNAL state : UNSIGNED (3 DOWNTO 0);

SIGNAL nRAS, nCAS, nWE : STD_LOGIC_VECTOR (3 DOWNTO 0);

TYPE ddr3a IS ARRAY (ROW_WIDTH - 1 DOWNTO 0) OF STD_LOGIC_VECTOR (3 DOWNTO 0);
SIGNAL A : ddr3a;

TYPE ddr3ba IS ARRAY (2 DOWNTO 0) OF STD_LOGIC_VECTOR (3 DOWNTO 0);
SIGNAL BA : ddr3ba;

CONSTANT RST_WAIT : UNSIGNED (3 DOWNTO 0) := d"0";
CONSTANT CKE_WAIT : UNSIGNED (3 DOWNTO 0) := d"1";
CONSTANT CONFIG : UNSIGNED (3 DOWNTO 0) := d"2";
CONSTANT ZQCL : UNSIGNED (3 DOWNTO 0) := d"3";
CONSTANT IDLE : UNSIGNED (3 DOWNTO 0) := d"4";
CONSTANT RED : UNSIGNED (3 DOWNTO 0) := d"5";
CONSTANT WRTE : UNSIGNED (3 DOWNTO 0) := d"6";
CONSTANT REFRESH : UNSIGNED (3 DOWNTO 0) := d"7";
CONSTANT WRITE_LEVEL : UNSIGNED (3 DOWNTO 0) := d"8";
CONSTANT READ_CALIB : UNSIGNED (3 DOWNTO 0) := d"9";

SIGNAL cycle : STD_LOGIC_VECTOR (4 DOWNTO 0);
SIGNAL tick : STD_LOGIC;
SIGNAL tick_count : UNSIGNED (16 DOWNTO 0) := d"50000";

CONSTANT CMD_SMR : UNSIGNED (2 DOWNTO 0) := (OTHERS => '0');
CONSTANT CMD_ar : UNSIGNED (2 DOWNTO 0) := "1";
CONSTANT CMD_PC : UNSIGNED (2 DOWNTO 0) := b"010";
CONSTANT CMD_BA : UNSIGNED (2 DOWNTO 0) := b"011";
CONSTANT CMD_W : UNSIGNED (2 DOWNTO 0) := b"100";
CONSTANT CMD_R : UNSIGNED (2 DOWNTO 0) := b"101";
CONSTANT CMD_ZQCL : UNSIGNED (2 DOWNTO 0) := b"110";
CONSTANT CMD_NOP : UNSIGNED (2 DOWNTO 0) := (OTHERS => '1');

CONSTANT MBL : UNSIGNED (1 DOWNTO 0) := "1";
CONSTANT MCAS : UNSIGNED (3 DOWNTO 0) := b"0110";
CONSTANT MCWL : UNSIGNED (2 DOWNTO 0) := "1";
CONSTANT MWR : UNSIGNED (2 DOWNTO 0) := b"100";
CONSTANT MDLLR : UNSIGNED (0 TO 0) := "1";
CONSTANT MRTTNOM : UNSIGNED (2 DOWNTO 0) := (OTHERS => '0');
CONSTANT MRTTWR : UNSIGNED (1 DOWNTO 0) := "1";
CONSTANT MRTTWROFF : UNSIGNED (1 DOWNTO 0) := (OTHERS => '0');
CONSTANT MDRIVE : UNSIGNED (1 DOWNTO 0) := (OTHERS => '0');
CONSTANT MAL : UNSIGNED (1 DOWNTO 0) := (OTHERS => '0');

SIGNAL MR0 : UNSIGNED (15 DOWNTO 0);
SIGNAL MR1 : UNSIGNED (15 DOWNTO 0);
SIGNAL MR2 : UNSIGNED (15 DOWNTO 0);
SIGNAL MR2WR : UNSIGNED (15 DOWNTO 0);
SIGNAL MR3 : UNSIGNED (15 DOWNTO 0);

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
                        nRAS(i) & nCAS(i) & nWE(i) <= CMD_NOP;
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
                        WHEN (RST_WAIT & "XXXXX") => IF tick THEN
                                resetDelay <= '1';
                                tick_count <= 500 * TO_UNSIGNED(USEC, 17) + 20;
                                state <= CKE_WAIT;
                            END IF;
                        WHEN (CKE_WAIT & "XXXXX") => IF (tick_count = 15) THEN
                                DDR3_cke <= '1';
                            END IF;
                            IF tick THEN
                                state <= CONFIG;
                                cycle <= "0";
                            END IF;
                        WHEN (CONFIG & "00000") => nRAS(0) & nCAS(0) & nWE(0) <= CMD_SMR;
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
                    END CASE;
                END IF;
            END IF;
    END PROCESS;
END ARCHITECTURE;