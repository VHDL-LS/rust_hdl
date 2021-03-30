# Example Project
This example project (`vhdl_ls.toml`) demonstrates the Parser/Language Server on a medium-sized project consiting of VUnit, OSVVM, UVVM and PoC.

To setup the project run
```
git submodule init
```
To update the reference projects to their latest commits run
```
git submodule update --remote --merge
```
Note that the `vhdl_ls.toml` file will need to be updated to reflect any changes in the project files.

This folder also contains a utility script `from_vunit_export.py` which converts a VUnit `--export-json` file into a `vhdl_ls.toml` file including the STD and IEEE libraries.