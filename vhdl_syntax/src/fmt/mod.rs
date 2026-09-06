//! Provides facilities to format nodes and tokens to encoded strings.
//! Note: this is **not** a VHDL-formatter implementation.
//!
//! VHDL file encoding is Latin-1, yet comments may carry arbitrary encoding.
//! This module provides means to write encoded text (i.e., UTF-8 or Latin-1)
//! to a file or string.

pub mod encoding;
pub mod impls;
pub mod write;
