use crate::ast::{AnyDesignUnit, AnyPrimaryUnit, AnySecondaryUnit, PackageBody};
use crate::formatting::buffer::Buffer;
use crate::HasTokenSpan;
use vhdl_lang::ast::PackageDeclaration;
use vhdl_lang::formatting::VHDLFormatter;
use vhdl_lang::{indented, TokenSpan};

impl VHDLFormatter<'_> {
    pub fn format_any_design_unit(&self, unit: &AnyDesignUnit, buffer: &mut Buffer, is_last: bool) {
        use AnyDesignUnit::*;
        match unit {
            Primary(primary) => self.format_any_primary_unit(primary, buffer),
            Secondary(secondary) => self.format_any_secondary_unit(secondary, buffer),
        }
        if !is_last {
            buffer.line_breaks(2);
        }
    }

    pub fn format_any_primary_unit(&self, unit: &AnyPrimaryUnit, buffer: &mut Buffer) {
        use AnyPrimaryUnit::*;
        match unit {
            Entity(entity) => self.format_entity(entity, buffer),
            Configuration(configuration) => self.format_configuration(configuration, buffer),
            Package(package) => self.format_package(package, buffer),
            PackageInstance(package_instance) => {
                self.format_package_instance(package_instance, buffer)
            }
            Context(context) => self.format_context(context, buffer),
        }
    }

    pub fn format_any_secondary_unit(&self, unit: &AnySecondaryUnit, buffer: &mut Buffer) {
        use AnySecondaryUnit::*;
        match unit {
            Architecture(architecture) => self.format_architecture(architecture, buffer),
            PackageBody(body) => self.format_package_body(body, buffer),
        }
    }

    pub fn format_package(&self, package: &PackageDeclaration, buffer: &mut Buffer) {
        self.format_context_clause(&package.context_clause, buffer);
        if let Some(item) = package.context_clause.last() {
            self.line_break_preserve_whitespace(item.span().end_token, buffer);
        }
        // package <ident> is
        self.format_token_span(
            TokenSpan::new(package.span.start_token, package.span.start_token + 2),
            buffer,
        );
        indented!(buffer, {
            if let Some(generic_clause) = &package.generic_clause {
                buffer.line_break();
                self.format_interface_list(generic_clause, buffer);
            }
            self.format_declarations(&package.decl, buffer);
        });
        buffer.line_break();
        self.format_token_span(
            TokenSpan::new(package.end_token, package.span.end_token - 1),
            buffer,
        );
        self.format_token_id(package.span.end_token, buffer);
    }

    pub fn format_package_body(&self, body: &PackageBody, buffer: &mut Buffer) {
        self.format_context_clause(&body.context_clause, buffer);
        if let Some(item) = body.context_clause.last() {
            self.line_break_preserve_whitespace(item.span().end_token, buffer);
        }
        // package body <ident> is
        self.format_token_span(
            TokenSpan::new(body.span.start_token, body.span.start_token + 3),
            buffer,
        );
        indented!(buffer, { self.format_declarations(&body.decl, buffer) });
        buffer.line_break();
        self.format_token_span(
            TokenSpan::new(body.end_token, body.span.end_token - 1),
            buffer,
        );
        self.format_token_id(body.span.end_token, buffer);
    }
}

#[cfg(test)]
mod test {
    use crate::analysis::tests::Code;
    use vhdl_lang::formatting::test_utils::check_formatted;

    fn check_package_formatted(input: &str) {
        check_formatted(
            input,
            input,
            Code::package_declaration,
            |formatter, package, buffer| formatter.format_package(package, buffer),
        );
    }

    #[test]
    fn format_simple_package() {
        check_package_formatted(
            "\
package foo is
end package;",
        );
        check_package_formatted(
            "\
package foo is
end package foo;",
        );
    }

    #[test]
    fn format_package_with_declarations() {
        check_package_formatted(
            "\
package pkg_name is
    type foo;
    constant bar: natural := 0;
end package;",
        );
    }

    #[test]
    fn format_package_with_generics() {
        check_package_formatted(
            "\
package pkg_name is
    generic (
        type foo;
        type bar
    );
end package;",
        );
    }

    #[test]
    fn format_package_with_context_clause() {
        check_package_formatted(
            "\
package pkg_name is
    generic (
        type foo;
        type bar
    );
end package;",
        );
    }

    fn check_context_formatted(input: &str) {
        check_formatted(
            input,
            input,
            Code::context_declaration,
            |formatter, package, buffer| formatter.format_context(package, buffer),
        );
    }

    #[test]
    fn check_simple_context() {
        check_context_formatted(
            "\
context ident is
end;",
        );
        check_context_formatted(
            "\
context ident is
end context;",
        );
        check_context_formatted(
            "\
context ident is
end ident;",
        );
        check_context_formatted(
            "\
context ident is
end context ident;",
        );
    }

    #[test]
    fn check_context_items() {
        check_context_formatted(
            "\
context ident is
    library foo;
    use foo.bar;
    context foo.ctx;
end context;",
        );
    }

    fn check_design_unit_formatted(input: &str) {
        check_formatted(
            input,
            input,
            Code::design_file,
            |formatter, file, buffer| {
                formatter.format_any_design_unit(&file.design_units[0].1, buffer, true)
            },
        );
    }

    #[test]
    fn design_unit_context_clause_preserve_whitespaces() {
        check_design_unit_formatted(
            "\
library lib;
use lib.foo.all;

package pkg_name is
end package;",
        );
        check_design_unit_formatted(
            "\
library lib;
use lib.foo.all;
package pkg_name is
end package;",
        );
        check_design_unit_formatted(
            "\
library lib;
use lib.foo.all;



package pkg_name is
end package;",
        );
    }

    #[test]
    fn check_package_body() {
        check_design_unit_formatted(
            "\
package body foo is
end package body;",
        )
    }

    #[test]
    fn check_whitespace_preservation_context() {
        check_design_unit_formatted(
            "\
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

library third_party;
use third_party.baz;

use work.foo.bar;

package body foo is
end package body;",
        )
    }

    #[test]
    fn check_whitespace_preservation_tokens_with_comments() {
        check_design_unit_formatted(
            "\
library ieee;

-- This is a comment
-- This is another comment
-- Third comment
library third_party;
use third_party.baz;

package body foo is
end package body;",
        )
    }

    #[test]
    fn check_whitespace_preservation_within_comments() {
        check_design_unit_formatted(
            "\
-- This is a comment


-- This ine appears later
-- Third comment
library third_party;
use third_party.baz;

package body foo is
end package body;",
        )
    }
}
