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
SIGNAL dout : STD_LOGIC_VECTOR (15 DOWNTO 0);
SIGNAL addr : STD_LOGIC_VECTOR (25 DOWNTO 0);
SIGNAL dout128 : STD_LOGIC_VECTOR (127 DOWNTO 0);

CONSTANT freq : INTEGER := 99800000;
CONSTANT total_size : INTEGER := 8 * 1024 * 1024;

SIGNAL start_addr : STD_LOGIC_VECTOR (25 DOWNTO 0) := (OTHERS => '0');

COMPONENT Gowin_rPLL
    PORT(
        clkout: OUT STD_LOGIC;
        lock: OUT STD_LOGIC;
        clkoutd: OUT STD_LOGIC;
        clkin: IN STD_LOGIC
    );
END COMPONENT;

SIGNAL rclkpos : STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL rclksel : STD_LOGIC_VECTOR (2 DOWNTO 0);
SIGNAL wstep : STD_LOGIC_VECTOR (7 DOWNTO 0);
SIGNAL debug : STD_LOGIC_VECTOR (63 DOWNTO 0);

BEGIN
    PLL: Gowin_rPLL
        PORT MAP(
            clkout => clk_x4,
            lock => lock,
            clkoutd => clk,
            clkin => ck
        );

END ARCHITECTURE;