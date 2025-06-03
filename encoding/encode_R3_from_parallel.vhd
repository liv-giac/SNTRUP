library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.constants.all;

-- Calculates the zq encoding according to the NTRU paper.
-- This compresses 4 2-bit small elments into a single 8 byte word
entity encode_R3_from_parallel is
	port(
		clock        : in  std_logic;
		reset        : in  std_logic;
		input        : in  std_logic_vector(3 downto 0);
		input_valid  : in  std_logic;
		output       : out std_logic_vector(7 downto 0);
		output_valid : out std_logic;
		done         : out std_logic
	);
end entity encode_R3_from_parallel;

architecture RTL of encode_R3_from_parallel is


	signal counter     : integer range 0 to p / 4;
	signal shift_reg   : std_logic_vector(7 downto 0);
	signal c0, c1      : std_logic_vector(1 downto 0);
	signal e0, e1      : std_logic_vector(1 downto 0);

	type type_state is (first_pair, second_pair, final);
	signal state : type_state;
begin
	fsm_process : process(clock, reset) is
	begin
		if reset = '1' then
			state        <= first_pair;
			counter      <= 0;
			output_valid <= '0';
			done         <= '0';
		elsif rising_edge(clock) then
			c0 <= input(1 downto 0);
			c1 <= input(3 downto 2);

			e0 <= std_logic_vector(signed(c0) + 1);
			e1 <= std_logic_vector(signed(c1) + 1);
			case state is
				when first_pair =>
					if input_valid = '1' then
						shift_reg <= e1 & e0 & shift_reg(7 downto 4);
						state     <= second_pair;
						output_valid <= '0';
					end if;
					done         <= '0';
				when second_pair =>
					if input_valid = '1' then
						shift_reg <= e1 & e0 & shift_reg(7 downto 4);
						counter   <= counter + 1;

						if counter + 1 = p / 4 then
							state <= final;
						else
							state <= first_pair;
						end if;
						output_valid <= '1';
					end if;
				when final =>
					state        <= first_pair;
					output_valid <= '1';
					done         <= '1';
					counter      <= 0;
			end case;
		end if;
	end process fsm_process;

	output         <= shift_reg;
end architecture RTL;
