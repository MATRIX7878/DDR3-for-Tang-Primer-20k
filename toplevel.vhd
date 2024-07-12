LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY toplevel IS
    PORT(clk, DDR3_ck, DDR3_reset : IN STD_LOGIC;
         DDR3_odt, DDR3_cke, DDR3_we, DDR3_cas, DDR3_ras, DDR3_cs : OUT STD_LOGIC;
         DDR3_dm : OUT STD_LOGIC_VECTOR (1 DOWNTO 0);
         DDR3_dqs : INOUT STD_LOGIC_VECTOR (1 DOWNTO 0);
         DDR3_bank : OUT STD_LOGIC_VECTOR (2 DOWNTO 0);
         DDR3_addr : OUT STD_LOGIC_VECTOR (13 DOWNTO 0);
         DDR3_dq : INOUT STD_LOGIC_VECTOR (15 DOWNTO 0)
    );
END toplevel;

ARCHITECTURE behavior OF toplevel IS

SIGNAL clock : STD_LOGIC;

COMPONENT Gowin_rPLL
    PORT (
        clkout: OUT STD_LOGIC;
        clkin: IN STD_LOGIC
    );
END COMPONENT;

BEGIN

PLL: Gowin_rPLL
    PORT MAP (
        clkout => CLOCK,
        clkin => clk
    );

END ARCHITECTURE;
