//! Configuration flags for controlling serialization behavior of nodes and tokens.
//!
//! `SerdeFlags` allows fine-grained control over what gets included when serializing
//! VHDL syntax trees, such as trivia (whitespace, comments) and source location data.
//!
//! # Example
//!
//! ```
//! # use vhdl_syntax::serde::SerdeFlags;
//! let flags = SerdeFlags::default()
//!     .include_trivia(false)
//!     .include_loc(true);
//! 
//! // Serialized data will not include trivia information
//! assert!(!flags.includes_trivia());
//! 
//! // Serialized data will include source location
//! assert!(flags.includes_loc());
//! ```

/// Controls how the syntax nodes are being serialized.
#[derive(Debug, Copy, Clone)]
pub struct SerdeFlags {
    include_trivia: bool,
    include_loc: bool,
}

impl Default for SerdeFlags {
    fn default() -> Self {
        Self {
            include_trivia: true,
            include_loc: true,
        }
    }
}

impl SerdeFlags {
    /// Whether to include trivia (i.e., whitespaces, comments, e.t.c.) in the serialized output
    pub fn includes_trivia(&self) -> bool {
        self.include_trivia
    }

    /// Specifies whether trivia (i.e., whitespaces, comments, e.t.c.) should be included in the serialized output
    pub fn include_trivia(mut self, include: bool) -> Self {
        self.include_trivia = include;
        self
    }

    /// Whether to include source location
    pub fn includes_loc(&self) -> bool {
        self.include_loc
    }

    /// Specifies whether location information should be included in the serialized output
    pub fn include_loc(mut self, include: bool) -> Self {
        self.include_loc = include;
        self
    }
}
