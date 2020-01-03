// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn package_name_must_be_visible_in_package_instance() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
generic (const : natural);
end package;

package ipkg_err is new gpkg generic map (const => 0);
package ipkg_ok is new work.gpkg generic map (const => 0);

package nested is
package ipkg_err is new gpkg generic map (const => 0);
package ipkg_ok is new work.gpkg generic map (const => 0);
end package;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s("gpkg", 2), "No declaration of 'gpkg'"),
            Diagnostic::error(code.s("gpkg", 4), "No declaration of 'gpkg'"),
        ],
    );
}

#[test]
fn package_name_must_be_an_uninstantiated_package_in_package_instance() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
constant const : natural := 0;
end package;

package ipkg is new work.pkg generic map (const => 0);

package nested is
package ipkg2 is new work.pkg.const generic map (const => 0);
end package;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("work.pkg"),
                "'work.pkg' is not an uninstantiated generic package",
            ),
            Diagnostic::error(
                code.s1("work.pkg.const"),
                "'work.pkg.const' is not an uninstantiated generic package",
            ),
        ],
    );
}
