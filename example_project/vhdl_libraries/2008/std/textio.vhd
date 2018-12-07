-- Package texio as defined by IEEE 1076-2008

package textio is
  type LINE is access STRING;
  type TEXT is file of STRING;

  type SIDE is (RIGHT, LEFT);
  subtype WIDTH is NATURAL;
end package;
