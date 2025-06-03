library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity r3_reciprocal_parallel is
  port(
    clock           : in  std_logic;
    reset           : in  std_logic;
    start           : in  std_logic;
    input_data      : in  std_logic_vector(3 downto 0); -- Two 2-bit coefficients
    output_data     : out std_logic_vector(3 downto 0); -- Two 2-bit results
    output_valid    : out std_logic;
    ready           : out std_logic;
    is_invertable   : out std_logic;    
    done            : out std_logic
  );
end entity;

architecture RTL of r3_reciprocal_parallel is

  signal start_a, start_b : std_logic;
  signal done_a, done_b   : std_logic;
  signal valid_a, valid_b : std_logic;
  signal out_a, out_b     : std_logic_vector(1 downto 0);
  signal ready_a, ready_b         : std_logic;
  signal invertable_a, invertable_b : std_logic;
  constant offset_b : std_logic_vector(9 downto 0) := std_logic_vector(to_unsigned(384, 10));

begin

  reciprocal_a: entity work.r3_reciprocal
    port map(
      clock => clock,
      reset => reset,
      start => start_a,
      address_offset => std_logic_vector(to_unsigned(0, 10)),
      small_polynomial_in => input_data(1 downto 0),
      output_polynomial => out_a,
      output_valid => valid_a,
      done => done_a,
      ready => ready_a,
      is_invertable => invertable_a
    );

  reciprocal_b: entity work.r3_reciprocal
    port map(
      clock => clock,
      reset => reset,
      address_offset => offset_b,
      start => start_b,
      small_polynomial_in => input_data(3 downto 2),
      output_polynomial => out_b,
      output_valid => valid_b,
      done => done_b,
      ready => ready_b,
      is_invertable => invertable_b
    );

  -- Start both units simultaneously when start is asserted
  start_a <= start;
  start_b <= start;
  
  -- Output is valid only when both are done and have valid output
  output_valid <= valid_a and valid_b;
  done <= done_a and done_b;
  ready <= ready_a and ready_b;
  is_invertable <= invertable_a and invertable_b;
  -- Combine the outputs: out_b goes to MSBs
  output_data <= out_b & out_a;

end architecture RTL;
