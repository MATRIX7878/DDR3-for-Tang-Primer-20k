LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD_UNSIGNED.ALL;

ENTITY ddr3 IS
    PORT(clk, ddr3reset, rd, wr, restart: IN STD_LOGIC;
         ck, ckn, cke, cs, rst, ras, cas, we, odt: OUT STD_LOGIC;
         DQS : INOUT STD_LOGIC_VECTOR (1 DOWNTO 0);
         BA : OUT STD_LOGIC_VECTOR (2 DOWNTO 0);
         A : OUT STD_LOGIC_VECTOR (15 DOWNTO 0);
         DQ : INOUT STD_LOGIC_VECTOR (15 DOWNTO 0);
         addr : IN STD_LOGIC_VECTOR (25 DOWNTO 0)
    );
END ddr3;

ARCHITECTURE Behavior OF ddr3 IS
TYPE state IS (reset, init, MRS, MR0, MR1, MR2, MR3, ZQCal, DLLZQ, writeLevel, writeStable, DQH, DQL, writeExit, readCalib, readLevel, readStable, idle, refresh, red, write, writeData);
SIGNAL currentState, nextState : state;

CONSTANT MRC : STD_LOGIC_VECTOR (3 DOWNTO 0) := (OTHERS => '0');
CONSTANT AUTO : STD_LOGIC_VECTOR (3 DOWNTO 0) := "0001";
CONSTANT PC : STD_LOGIC_VECTOR (3 DOWNTO 0) := "0010";
CONSTANT ACT : STD_LOGIC_VECTOR (3 DOWNTO 0) := "0011";
CONSTANT WRTE : STD_LOGIC_VECTOR (3 DOWNTO 0) := "0100";
CONSTANT READ : STD_LOGIC_VECTOR (3 DOWNTO 0) := "0101";
CONSTANT ZQCL : STD_LOGIC_VECTOR (3 DOWNTO 0) := "0110";
CONSTANT NOP : STD_LOGIC_VECTOR (3 DOWNTO 0) := "0111";
CONSTANT xs : STD_LOGIC_VECTOR (18 DOWNTO 0) := (OTHERS => '-');

CONSTANT counterCL : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"10";
CONSTANT counterMOD : STD_LOGIC_VECTOR (3 DOWNTO 0) := d"14";
CONSTANT counterDQ : STD_LOGIC_VECTOR (4 DOWNTO 0) := d"24";
CONSTANT counterREF : STD_LOGIC_VECTOR (6 DOWNTO 0) := d"87";
CONSTANT counterXPR : STD_LOGIC_VECTOR (6 DOWNTO 0) := d"95";
CONSTANT counterZQ : STD_LOGIC_VECTOR (8 DOWNTO 0) := d"511";
CONSTANT counterRST : STD_LOGIC_VECTOR (17 DOWNTO 0) := d"159999";
CONSTANT counterCKE : STD_LOGIC_VECTOR (18 DOWNTO 0) := d"399999";

SIGNAL counter : STD_LOGIC_VECTOR (19 DOWNTO 0) := (OTHERS => '0');

SIGNAL dlllock : STD_LOGIC;
SIGNAL rstlock : STD_LOGIC;

SIGNAL fclk : STD_LOGIC;
SIGNAL dllstep : STD_LOGIC_VECTOR (7 DOWNTO 0);

SIGNAL wstep : STD_LOGIC_VECTOR (7 DOWNTO 0) := (OTHERS => '0');

SIGNAL dqshold : STD_LOGIC := '0';

SIGNAL dqsout : STD_LOGIC_VECTOR (0 TO 7);
SIGNAL dqsen : STD_LOGIC_VECTOR (0 TO 3);

SIGNAL rburstSeen, rclkpos, rburst : STD_LOGIC_VECTOR (1 DOWNTO 0);
SIGNAL rclksel : STD_LOGIC_VECTOR (2 DOWNTO 0) := (OTHERS => '0');
SIGNAL rcalibCnt, dqsRead : STD_LOGIC_VECTOR (3 DOWNTO 0) := (OTHERS => '0');

COMPONENT DLL 
    GENERIC( 
        DLL_FORCE:integer:=0; 
        DIV_SEL:bit:='1'; 
        CODESCAL:STRING:="000"; 
        SCAL_EN:STRING:="TRUE" 
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

BEGIN
    PROCESS(ALL)
        BEGIN
        IF RISING_EDGE(clk) THEN
            rstlock <= ddr3reset AND dlllock;
            IF rstlock THEN
                CASE currentState IS
                    WHEN reset => IF counter = counterRST THEN
                        counter <= (OTHERS => '0');
                        rst <= '0';
                        nextState <= init;
                    ELSE
                        counter <= counter + '1';
                        rst <= '1';
                        cke <= '0';
                    END IF;
                    WHEN init => IF counter = counterCKE THEN
                        counter <= (OTHERS => '0');
                        cke <= '1';
                        nextState <= MRS;
                    ELSE
                        counter <= counter + '1';
                        IF counter = 199998 THEN
                            cs <= NOP(3);
                            ras <= NOP(2);
                            cas <= NOP(1);
                            we <= NOP(0);
                            BA <= xs(18 DOWNTO 16);
                            A <= xs(15 DOWNTO 0);
                            odt <= '1';
                        END IF;
                    END IF;
                    WHEN MRS => IF counter = counterXPR THEN
                        counter <= (OTHERS => '0');
                        nextState <= MR2;
                    ELSE
                        counter <= counter + '1';
                    END IF;
                    WHEN MR0 => BA <= (OTHERS => '0');
                        cs <= MRC(3);
                        ras <= MRC(2);
                        cas <= MRC(1);
                        we <= MRC(0);
                        A(15 DOWNTO 12) <= (OTHERS => '0');
                        A(11 DOWNTO 8) <= (OTHERS => '1');
                        A(7) <= '0';
                        A(6 DOWNTO 4) <= (OTHERS => '1');
                        A(3) <= '0';
                        A(2 DOWNTO 1) <= (OTHERS => '0');
                        A(0) <= '1';
                        nextState <= ZQCal;
                    WHEN MR1 => BA(2 DOWNTO 1) <= (OTHERS => '0');
                        BA(0) <= '1';
                        cs <= MRC(3);
                        ras <= MRC(2);
                        cas <= MRC(1);
                        we <= MRC(0);
                        A(15 DOWNTO 0) <= (OTHERS => '0');
                        nextState <= MR0;
                    WHEN MR2 => BA(0) <= '0';
                        BA(1) <= '1';
                        BA(2) <= '0';
                        cs <= MRC(3);
                        ras <= MRC(2);
                        cas <= MRC(1);
                        we <= MRC(0);
                        A(15 DOWNTO 6) <= (OTHERS => '0');
                        A(5 DOWNTO 4) <= (OTHERS => '1');
                        A(3 DOWNTO 0) <= (OTHERS => '0');
                        nextState <= MR3;
                    WHEN MR3 => BA(2) <= '0';
                        BA(1 DOWNTO 0) <= (OTHERS => '1');
                        cs <= MRC(3);
                        ras <= MRC(2);
                        cas <= MRC(1);
                        we <= MRC(0);
                        A(15 DOWNTO 0) <= (OTHERS => '0');
                        nextState <= MR1;
                    WHEN ZQCal => cs <= ZQCL(3);
                        ras <= ZQCL(2);
                        cas <= ZQCL(1);
                        we <= ZQCL(0);
                        A(15 DOWNTO 11) <= (OTHERS => 'X');
                        A(10) <= '1';
                        A(9 DOWNTO 0) <= (OTHERS => 'X');
                        nextState <= DLLZQ;
                    WHEN DLLZQ => IF counter = counterZQ THEN
                        counter <= (OTHERS => '0');
                        nextState <= writeLevel;
                    ELSE
                        counter <= counter + '1';
                    END IF;
                    WHEN writeLevel => BA(2 DOWNTO 1) <= (OTHERS => '0');
                        BA(0) <= '1';
                        cs <= MRC(3);
                        ras <= MRC(2);
                        cas <= MRC(1);
                        we <= MRC(0);
                        A(15 DOWNTO 8) <= (OTHERS => '0');
                        A(7) <= '1';
                        A(6 DOWNTO 3) <= (OTHERS => '0');
                        A(2) <= '1';
                        A(1 DOWNTO 0) <= (OTHERS => '0');
                        odt <= '0';
                        nextState <= writeStable;
                    WHEN writeStable => IF counter = counterMod THEN
                        odt <= '1';
                        counter <= counter + '1';
                    ELSIF counter = counterDQ THEN
                        counter <= (OTHERS => '0');
                        dqsout <= (OTHERS => '0');
                        dqsen <= (OTHERS => '0');
                        nextState <= DQH;
                    ELSE
                        cs <= NOP(3);
                        ras <= NOP(2);
                        cas <= NOP(1);
                        we <= NOP(0);
                        BA <= xs(18 DOWNTO 16);
                        A <= xs(15 DOWNTO 0);
                        counter <= counter + '1';
                    END IF;
                    WHEN DQH => dqsout <= "10101010";
                        dqsen <= (OTHERS => '0');
                        nextState <= DQL;
                    WHEN DQL => dqsout <= (OTHERS => '0');
                        dqsen <= (OTHERS => '0');
                        IF NOT DQ(0) OR NOT DQ(8) THEN
                            wstep <= wstep + '1';
                            nextState <= writeStable;
                        ELSE
                            BA(2 DOWNTO 1) <= (OTHERS => '0');
                            BA(0) <= '1';
                            cs <= MRC(3);
                            ras <= MRC(2);
                            cas <= MRC(1);
                            we <= MRC(0);
                            A(15 DOWNTO 0) <= (OTHERS => '0');
                            nextState <= writeExit;
                        END IF;
                    WHEN writeExit => odt <= '0';
                        BA(0) <= '0';
                        BA(1) <= '1';
                        BA(2) <= '0';
                        cs <= MRC(3);
                        ras <= MRC(2);
                        cas <= MRC(1);
                        we <= MRC(0);
                        A(15 DOWNTO 10) <= (OTHERS => '0');
                        A(9) <= '1';
                        A(8 DOWNTO 6) <= (OTHERS => '0');
                        A(5 DOWNTO 4) <= (OTHERS => '1');
                        A(3 DOWNTO 0) <= (OTHERS => '0');
                        nextState <= readCalib;
                    WHEN readCalib => cs <= ACT(3);
                        ras <= ACT(2);
                        cas <= ACT(1);
                        we <= ACT(0);
                        BA(0) <= '0';
                        A(0) <= '0';
                        nextState <= readLevel;
                    WHEN readLevel => cs <= READ(3);
                        ras <= READ(2);
                        cas <= READ(1);
                        we <= READ(0);
                        A(15 DOWNTO 13) <= xs(15 DOWNTO 13);
                        A(12) <= '1';
                        A(11) <= '0';
                        A(10) <= '0';
                        A(9 DOWNTO 0) <= (OTHERS => '0');
                        nextState <= readStable;
                    WHEN readStable => IF rburstSeen /= "11" THEN
                            rclksel <= rclksel + '1';
                            rclkpos <= rclkpos + '1' WHEN rclksel = d"7" ELSE rclkpos;
                            rcalibCnt <= (OTHERS => '0');
                            nextState <= readLevel;
                        ELSE
                            rcalibCnt <= rcalibCnt + '1';
                            IF rcalibCnt = 7 THEN
                                cs <= PC(3);
                                ras <= PC(2);
                                cas <= PC(1);
                                we <= PC(0);
                                BA(2 DOWNTO 0) <= xs(18 DOWNTO 16);
                                A(15 DOWNTO 11) <= xs(15 DOWNTO 11);
                                A(10) <= '1'; 
                                A(9 DOWNTO 0) <= xs(9 DOWNTO 0);
                                nextState <= idle;
                            ELSE
                                nextState <= readLevel;
                            END IF;
                        END IF;
                    WHEN idle => IF rd OR wr THEN
                        cs <= ACT(3);
                        ras <= ACT(2);
                        cas <= ACT(1);
                        we <= ACT(0);
                        BA(2 DOWNTO 0) <= addr(25 DOWNTO 23);
                        A(15 DOWNTO 13) <= xs(15 DOWNTO 13);
                        A(12 DOWNTO 0) <= addr(22 DOWNTO 10);
                        nextState <= red WHEN rd = '1' ELSE write;
                        IF rd THEN
                            dqshold <= '1';
                        END IF;
                    ELSIF restart THEN
                        cs <= AUTO(3);
                        ras <= AUTO(2);
                        cas <= AUTO(1);
                        we <= AUTO(0);
                        BA(2 DOWNTO 0) <= xs(18 DOWNTO 16);
                        A(15 DOWNTO 0) <= xs(15 DOWNTO 0);
                        nextState <= refresh;
                    END IF;
                    WHEN refresh => IF counter = counterREF THEN
                        counter <= (OTHERS => '0');
                        nextState <= idle;
                    ELSE
                        counter <= counter + '1';
                    END IF;
                    WHEN write => cs <= WRTE(3);
                        ras <= WRTE(2);
                        cas <= WRTE(1);
                        we <= WRTE(0);
                        BA(2 DOWNTO 0) <= addr(25 DOWNTO 23);
                        A(15 DOWNTO 13) <= xs(15 DOWNTO 13);
                        A(12) <= '0';
                        A(11) <= xs(11);
                        A(10) <= '1';
                        A(9 DOWNTO 0) <= addr(9 DOWNTO 0);
                        nextState <= writeData;
                    WHEN writeData => IF counter = counterCL THEN
                        counter <= (OTHERS => '0');
                    ELSE
                        counter <= counter + '1';
                    END IF;
                END CASE;
            END IF;
        END IF;  
    END PROCESS;

    PROCESS (clk)
    BEGIN
        IF RISING_EDGE(clk) THEN
            currentState <= nextState;
        END IF;

        IF rburst(0) THEN
            rburstSeen(0) <= '1';
        END IF;

        IF rburst(1) THEN
            rburstSeen(1) <= '1';
        END IF;

        IF currentState = readLevel THEN
            rburstSeen <= (OTHERS => '0');
        END IF;

        IF NOT rstlock THEN
            rburstSeen <= (OTHERS => '0');
        END IF;

        dqsRead <= (OTHERS => '0');
        IF currentState = readStable THEN
            dqsRead <= (OTHERS => '1');
        END IF;
    END PROCESS;

    delay:DLL 
        GENERIC MAP( 
            CODESCAL=>"101", 
            SCAL_EN=>"TRUE" 
        ) 
        PORT MAP( 
            CLKIN=>fclk, 
            STOP=>'0', 
            RESET=>NOT ddr3reset, 
            UPDNCNTL=>'0', 
            LOCK=>dlllock, 
            STEP=>dllstep 
    );
END ARCHITECTURE;
