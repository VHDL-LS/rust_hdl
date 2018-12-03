# Example Project
This example project (`vhdl_ls.toml`) demonstrates the Parser/Language Server on a medium project consiting of VUnit, OSVVM, UVVM and PoC.
In addition to this the project includes the standard VHDL library as well as the IEEE-libraries from GHDL.
The `setup.sh` script will clone all the repositories used by the example project.

This folder also contains a utility script `from_vunit_export.py`
which converts a VUnit `--export-json` file into a `vhdl_ls.toml` file
including the STD and IEEE libraries.
