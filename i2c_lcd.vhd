-- I2C LCD controller
-- This entity controls an LCD display over an I2C bus.
--
-- Port map:
--   clk      : in std_logic;          -- Clock signal
--   rst      : in std_logic;          -- Reset signal
--   sda      : inout std_logic;       -- I2C data line
--   scl      : inout std_logic;       -- I2C clock line
--   en       : in std_logic;          -- Enable signal
--   rs       : in std_logic;          -- Register select signal
--   rw       : in std_logic;          -- Read/write signal
--   db       : in std_logic_vector;   -- Data bus
--   busy     : out std_logic;         -- Busy flag
--
-- State machine:
--   idle  : waiting for start condition
--   start : waiting for slave address
--   addr  : waiting for register select
--   data  : waiting for data
--   stop  : waiting for stop condition

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity i2c_lcd is
    port(
        clk      : in std_logic;
        rst      : in std_logic;
        sda      : inout std_logic;
        scl      : inout std_logic;
        en       : in std_logic;
        rs       : in std_logic;
        rw       : in std_logic;
        db       : in std_logic_vector(7 downto 0);
        busy     : out std_logic
    );
end i2c_lcd;

architecture behavioral of i2c_lcd is
    type state_t is (idle, start, addr, data, stop);
    signal state    : state_t;
    signal bit_cnt  : natural;
    signal sda_d    : std_logic;
    signal scl_d    : std_logic;
    signal sda_i    : std_logic;
    signal scl_i    : std_logic;
    signal data_reg : std_logic_vector(7 downto 0);
    signal addr_reg : std_logic_vector(7 downto 0);
    signal rw_reg   : std_logic;
    signal rs_reg   : std_logic;
    signal en_reg   : std_logic;
    signal busy_d   : std_logic;

    -- I2C start condition
    function start_cond return std_logic is
        variable sda_p : std_logic;
    begin
        sda_p := '1';
        if rising_edge(scl) then
            if sda_i = '0' then
                sda_p := '0';
            end if;
        end if;
        return sda_p;
    end function;

    -- I2C stop condition
    function stop_cond return std_logic is
        variable sda_p : std_logic;
    begin
        sda_p := '0';
        if rising_edge(scl) then
            if sda_i = '1' then
                sda_p := '1';
            end if;
        end if;
        return sda_p;
    end function;

    -- I2C data transfer
    function data_xfer(din : std_logic_vector; dout : std_logic_vector)
        return std_logic_vector is
        variable d_reg : std_logic_vector(7 downto 0);
        variable d_in  : std_logic_vector(7 downto 0);
    begin
        d_reg := dout;
        d_in  := din;
        if rising_edge(scl) then
            if sda_i = '1' then
                d_reg(bit_cnt) := '1';
            else
                d_reg(bit_cnt) := '0';
            end if;
            d_in(bit_cnt) := sda_d;
        end if;
        return d_reg;
    end function;

begin
    -- I/O assignments
    sda <= sda_d;
    scl <= scl_d;
    busy <= busy_d;

    -- Data registers
    data_reg <= db;
    addr_reg <= "01111100";  -- I2C slave address for LCD
    rw_reg   <= rw;
    rs_reg   <= rs;
    en_reg   <= en;

    -- State machine
    process(clk, rst)
    begin
        if rst = '1' then
            state <= idle;
            sda_d <= '1';
            scl_d <= '1';
            busy_d <= '1';
        elsif rising_edge(clk) then
            case state is
                when idle =>
                    if sda_i = '0' and scl_i = '1' then
                        state <= start;
                        bit_cnt <= 7;
                    end if;
                when start =>
                    sda_d <= start_cond;
                    if bit_cnt = 0 then
                        state <= addr;
                    else
                        bit_cnt <= bit_cnt - 1;
                    end if;
                when addr =>
                    sda_d <= data_xfer(addr_reg, sda_i)(bit_cnt);
                    if bit_cnt = 0 then
                        if rw_reg = '1' then
                            state <= data;
                            bit_cnt <= 7;
                            busy_d <= '1';
                        else
                            state <= stop;
                        end if;
                    else
                        bit_cnt <= bit_cnt - 1;
                    end if;
                when data =>
                    sda_d <= data_xfer(data_reg, sda_i)(bit_cnt);
                    if bit_cnt = 0 then
                        state <= stop;
                    else
                        bit_cnt <= bit_cnt - 1;
                    end if;
                when stop =>
                sda_d <= stop_cond;
                if scl_i = '1' then
                    state <= idle;
                    busy_d <= '0';
                end if;
        end case;
    end if;
end process;

-- I/O latches
sda_i <= sda;
scl_i <= scl;

end behavioral;
