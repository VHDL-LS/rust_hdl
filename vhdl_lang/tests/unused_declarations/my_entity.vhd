library ieee;
use ieee.std_logic_1164.all;

entity my_ent is
    port (
        foo : in std_logic;
        bar : out std_logic;
        baz : inout std_logic
    );
end my_ent;

architecture arch of my_ent is
begin
    bar <= foo;
end architecture arch;
