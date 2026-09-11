// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com
#![no_main]

use libfuzzer_sys::fuzz_target;
use vhdl_syntax::parser::parse;
use vhdl_syntax::syntax::DesignFileSyntax;

fn unparse(file: &DesignFileSyntax) -> Vec<u8> {
    let mut buf = Vec::new();
    file.write_to(&mut buf)
        .expect("writing to a Vec cannot fail");
    buf
}

fuzz_target!(|data: &[u8]| {
    let (file, _diagnostics) = parse(data);

    let unparsed = unparse(&file);
    assert!(
        unparsed == data,
        "round-trip mismatch\n input: {:?}\noutput: {:?}",
        String::from_utf8_lossy(data),
        String::from_utf8_lossy(&unparsed),
    );
});
