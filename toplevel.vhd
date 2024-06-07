LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY toplevel IS
    PORT(ck, reset : IN STD_LOGIC;
         DDR3_odt, DDR3_cke, DDR3_we, DDR3_cas, DDR3_ras, DDR3_cs : OUT STD_LOGIC;
         DDR3_dm : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
         DDR3_dqs : INOUT STD_LOGIC_VECTOR (1 DOWNTO 0);
         DDR3_bank : OUT STD_LOGIC_VECTOR (2 DOWNTO 0);
         DDR3_addr : OUT STD_LOGIC_VECTOR (13 DOWNTO 0);
         DDR3_dq : INOUT STD_LOGIC_VECTOR (15 DOWNTO 0));
END ENTITY;

ARCHITECTURE behavior OF toplevel IS

SIGNAL start : STD_LOGIC := 1;

SIGNAL rd, wr, refresh, clk_x4, lock, clk : STD_LOGIC;
SIGNAL din : STD_LOGIC_VECTOR (15 DOWNTO 0);
SIGNAL dOUT : STD_LOGIC_VECTOR (15 DOWNTO 0);
SIGNAL addr : STD_LOGIC_VECTOR (25 DOWNTO 0);
SIGNAL dOUT128 : STD_LOGIC_VECTOR (127 DOWNTO 0);

SIGNAL rclkpos : STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL rclksel : STD_LOGIC_VECTOR (2 DOWNTO 0);
SIGNAL wstep : STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL debug : STD_LOGIC_VECTOR (63 DOWNTO 0);

CONSTANT freq : INTEGER := 99800000;
CONSTANT TOTAL_SIZE : INTEGER := 8 * 1024 * 1024;

CONSTANT INIT : INTEGER := 0;
CONSTANT PRINT : INTEGER := 1;
CONSTANT WRITE1 : INTEGER := 2;
CONSTANT WRITE2 : INTEGER := 3;
CONSTANT WRITE3 : INTEGER := 4;
CONSTANT READSTART : INTEGER := 5;
CONSTANT READ : INTEGER := 6;
CONSTANT READDONE : INTEGER := 7;
CONSTANT WRITEBLOCK : INTEGER := 8;
CONSTANT VERIFY : INTEGER := 9;
CONSTANT WIPE : INTEGER := 10;
CONSTANT FINISH : INTEGER := 11;

SIGNAL state, endState, workCount, latWr1, latWr2, latR : STD_LOGIC_VECTOR (7 DOWNTO 0);

SIGNAL error, refreshNeeded, refreshExec : STD_LOGIC;

SIGNAL refreshTime : STD_LOGIC_VECTOR (11 DOWNTO 0);
CONSTANT REFRESHCOUNT : REAL := freq / 1000 / 1000 * 7813 / 1000;

SIGNAL refreshCycle : STD_LOGIC;
SIGNAL refreshCnt : STD_LOGIC_VECTOR (23 DOWNTO 0);
SIGNAL refreshAddr : STD_LOGIC_VECTOR (24 DOWNTO 0);

TYPE debugdq IS ARRAY (64 DOWNTO 0) OF STD_LOGIC_VECTOR (15 DOWNTO 0);
SIGNAL debugdqbuf : debugdq;

SIGNAL debugCycle : STD_LOGIC_VECTOR (3 DOWNTO 0);
SIGNAL tickCount :STD_LOGIC_VECTOR (19 DOWNTO 0);
SIGNAL tick : STD_LOGIC;
SIGNAL rsltPrint : STD_LOGIC;
SIGNAL expect, actual : STD_LOGIC_VECTOR (15 DOWNTO 0);
SIGNAL actual128 : STD_LOGIC_VECTOR (127 DOWNTO 0);
SIGNAL addrRead : STD_LOGIC_VECTOR (25 DOWNTO 0);
SIGNAL wlevelFeed : STD_LOGIC;
SIGNAL wlevelDone : STD_LOGIC := 0;
SIGNAL rlevelDone := 0;
SIGNAL readCnt : STD_LOGIC_VECTOR (7 DOWNTO 0);

TYPE BYTE IS ARRAY (7 DOWNTO 0) OF BIT;
TYPE ADDR IS ARRAY (25 DOWNTO 0) OF BIT;

SIGNAL START_ADDR : STD_LOGIC_VECTOR (25 DOWNTO 0) := (OTHERS => '0');

COMPONENT Gowin_rPLL
    PORT (
        clkout: OUT STD_LOGIC;
        lock: OUT STD_LOGIC;
        clkoutp: OUT STD_LOGIC;
        clkoutd: OUT STD_LOGIC;
        clkin: IN STD_LOGIC
    );
END COMPONENT;

COMPONENT DDR3 IS
    GENERIC(ROW_WIDTH : INTEGER;
            COL_WIDTH : INTEGER;
            BANK_WIDTH : INTEGER
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
END COMPONENT;

BEGIN
    pll: Gowin_rPLL
        port map (
            clkout => clk_x4,
            lock => lock,
            clkoutp => clk_ck,
            clkoutd => clk,
            clkin => ck
        );

    uddr3 : DDR3
        GENERIC MAP (ROW_WIDTH => 13,
                COL_WIDTH => 10,
                BANK_WIDTH => 3
                )
        PORT MAP (ck => clk_ck, pclk => clk, fclk => clk_x4, reset => (reset & lock),
                  rd => rd, wr => wr, DDR3_refresh => refresh, addr => addr,
                  din => din, DDR3_odt => DDR3_odt, DDR3_cke => DDR3_cke, DDR3_we => DDR3_we,
                  DDR3_cas => DDR3_cas, DDR3_ras => DDR3_ras, DDR3_cs => DDR3_cs, DDR3_ck => DDR3_ck,
                  DDR3_reset => DDR3_reset, write_done => write_done, read_done => read_done, data_ready => data_ready,
                  busy => busy, rclkpos => rclkpos, DDR3_dm => DDR3_dm, DDR3_dqs => DDR3_dqs,
                  rclksel => rclksel, wstep => wstep, DDR3_dq => DDR3_dq, dOUT => dOUT,
                  debug => debug, dOUT128 => dOUT128, DDR3_a => DDR3_a, DDR3_ba => DDR3_ba);

    PROCESS(ALL)
        BEGIN
        IF RISING_EDGE(clk) THEN
            IF state THEN
                refreshTime <= (REFRESHCOUNT * 2 - 2) WHEN refreshTime = (REFRESHCOUNT * 2 - 2) ELSE refreshTime + 1;
                IF refreshTime = REFRESHCOUNT THEN
                    refreshNeeded <= '1';
                END IF;
                IF refreshExec THEN
                    refreshTime <= refreshTime - REFRESHCOUNT;
                    refreshNeeded <= '0';
                END IF;
                IF NOT reset THEN
                    refreshTime <= '0';
                    refreshNeeded <= '0';
                END IF;
            END IF;
        END IF;
    END PROCESS;

    PROCESS(ALL)
        BEGIN
        IF RISING_EDGE(clk) THEN
            wr <= '0';
            rd <= '0';
            refresh <= '0';
            refreshExec <= '0';
            workCount <= workCount + 1;
            tickCount <= "0" WHEN tickCount = 0 ELSE tickCount - 1;
            tick <= tickCount = 1;

            CASE state IS
                WHEN INIT => IF (lock AND reset AND NOT busy AND start) THEN
                        tickCount <= d"100000";
                        state <= PRINT;
                    END IF;
                WHEN PRINT => IF tick THEN
                        tickCount <= d"100000";
                        workCount <= '0';
                        addr = START_ADDR;
                        state <= WRITE1;    
                    END IF;
                WHEN WRITE1 => IF tick THEN
                        wr <= '1';
                        addr <= x"0000";
                        din <= x"1122";
                        workCount <= '0';
                        tickCount <= d"100000";
                        state <= WRITE2;
                        END IF;
                WHEN WRITE2 => IF tick THEN
                        wr <= '1';
                        addr <= x"0001";
                        din <= x"3344";
                        workCount <= '0';
                        tickCount <= d"100000";
                        state <= WRITE3;
                        END IF;
                WHEN WRITE3 => IF tick THEN
                        IF busy THEN
                            error <= '1';
                        END IF;
                        latWr1 <= workCount(7 DOWNTO 0);
                        wr <= '1';
                        addr <= x"0002";
                        din <= x"5566";
                        workCount <= '0';
                        debugCycle <= '0';
                        tickCount <= d"100000";
                        state <= READSTART;
                        END IF;
                WHEN READSTART => IF tick THEN
                        addr(15 DOWNTO 0) <= x"0000";
                        tickCount <= d"100000";
                        state <= READ;
                        END IF;
                WHEN READ => rsltPrint <= '0';
                        IF tick THEN
                            IF addr(15 DOWNTO 0) = x"0003" THEN
                                tickCount <= d"200000";
                                state <= READDONE;
                            ELSE
                                rd <= '1';
                                tickCount <= x"200000";
                            END IF;
                        ELSIF data_ready THEN
                            actual <= dout;
                            actual128 <= dout128;
                            addrRead <= addr;
                            rsltPrint <= '1';
                            addr(15 DOWNTO 0) <= addr(15 DOWNTO 0) + 1;
                        END IF;
                WHEN READDONE => addr <= START_ADDR;
                        workCount <= '0';
                        state <= WIPE;
                WHEN WIPE => IF (addr = (START_ADDR + TOTAL_SIZE)) THEN
                            workCount <= '0';
                            addr <= START_ADDR;
                            state <= WRITEBLOCK;
                        ELSE
                            IF workCount = 0 THEN
                                IF NOT refreshNeeded THEN
                                    wr <= '1';
                                    din <= '0';
                                    refreshCycle <= '0';
                                ELSE
                                    refresh <= '1';
                                    refreshCnt <= refreshCnt + 1;
                                    refreshAddr <= addr;
                                    refreshExec <= '1';
                                    refreshCycle <= '1';
                                END IF;
                            ELSIF (NOT wr AND NOT refresh AND NOT busy) THEN
                                workCount <= '0';
                                IF NOT refreshCycle THEN
                                    addr <= addr + 1;
                                END IF;
                            END IF;
                        END IF;
                WHEN WRITEBLOCK => IF (addr = (START_ADDR + TOTAL_SIZE)) THEN
                            addr <= START_ADDR;
                            workCount <= '0';
                            state <= VERIFY;
                        ELSE
                            IF workCount = 0 THEN
                                IF NOT refreshNeeded THEN
                                    wr <= '1';
                                    din <= addr(15 DOWNTO 0) XOR ("000000" & addr(25 DOWNTO 16)) XOR x"59";
                                    refreshCycle <= '0';
                                ELSE
                                    refresh <= '1';
                                    refreshCnt <= refreshCnt + 1;
                                    refreshAddr <= addr;
                                    refreshExec <= '1';
                                    refreshCycle <= '1';
                                END IF;
                            ELSIF (NOT wr AND NOT refresh AND NOT busy) THEN
                                workCount <= '0';
                                IF NOT refreshCycle THEN
                                    addr <= addr + 1;
                                END IF;
                            END IF;
                        END IF;
                WHEN VERIFY => IF (addr = (START_ADDR + TOTAL_SIZE)) THEN
                            endState <= state;
                            state <= VERIFY;
                        ELSE
                            IF workCount = 0 THEN
                                IF NOT refreshNeeded THEN
                                    rd <= '1';
                                    refreshCycle <= '0';
                                ELSE
                                    refresh <= '1';
                                    refreshCnt <= refreshCnt + 1;
                                    refreshAddr <= addr;
                                    refreshExec <= '1';
                                    refreshCycle <= '1';
                                END IF;
                            ELSIF (data_ready) THEN
                                expect <= addr(15 DOWNTO 0) XOR ("000000" & addr(25 DOWNTO 16)) XOR x"59";
                                actual <= dout;
                                actual128 <= dout128;
                                IF (dout(7 DOWNTO 0) /= (addr XOR ("000000" & addr(25 DOWNTO 16)) XOR d"59") THEN
                                    error <= '1';
                                    endState <= state;
                                    state <= FINISH;
                                END IF;
                            ELSIF (NOT wr AND NOT refresh AND NOT busy) THEN
                                workCount <= '0';
                                IF NOT refreshCycle THEN
                                    addr <= addr + 1;
                                END IF;
                            END IF;
                        END IF;
            END CASE;
            IF NOT reset THEN
                error <= '0';
                tick <= '0';
                tickCount <= d"100000";
                latWr1 <= '0';
                latWr2 <= '0';
                latR <= '0';
                refreshCnt <= '0';
                state <= INIT;
            END IF;
        END IF;
    END PROCESS;
END ARCHITECTURE;
