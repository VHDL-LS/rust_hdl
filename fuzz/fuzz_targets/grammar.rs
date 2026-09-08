// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

//! Fuzzes the parser against the modelled grammar

#![no_main]

use libfuzzer_sys::fuzz_target;
use rust_hdl_fuzz::{assert_parses, Design, Grammar, GrammarSource};
use std::sync::LazyLock;

/// The grammar `vhdl_syntax` models, `xtask/doc/vhdl-08-modified.ungram`.
pub struct Modelled;

impl GrammarSource for Modelled {
    fn grammar() -> &'static Grammar {
        static PREPARED: LazyLock<Grammar> = LazyLock::new(|| {
            Grammar::new(
                "xtask/doc/vhdl-08-modified.ungram",
                include_str!("../../xtask/doc/vhdl-08-modified.ungram"),
                &[
                    ("Name", b"name"),
                    ("SubtypeIndication", b"subtype_indication"),
                ],
            )
        });
        &PREPARED
    }
}

fuzz_target!(|design: Design<Modelled>| assert_parses(design));
