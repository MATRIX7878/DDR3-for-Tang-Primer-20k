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
CONSTANT total_size : INTEGER := 8 * 1024 * 1024;

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

SIGNAL start_addr : STD_LOGIC_VECTOR (25 DOWNTO 0) := (OTHERS => '0');

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
         dOUT : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
         debug : OUT STD_LOGIC_VECTOR (63 DOWNTO 0);
         dOUT128 : OUT STD_LOGIC_VECTOR (127 DOWNTO 0);
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



END ARCHITECTURE;
