use crate::ast::ArchitectureBody;
use crate::formatting::buffer::Buffer;
use crate::formatting::VHDLFormatter;
use crate::{indented, HasTokenSpan, TokenSpan};

impl VHDLFormatter<'_> {
    pub fn format_architecture(&self, arch: &ArchitectureBody, buffer: &mut Buffer) {
        self.format_context_clause(&arch.context_clause, buffer);
        if let Some(item) = arch.context_clause.last() {
            self.line_break_preserve_whitespace(item.span().end_token, buffer);
        }
        let span = arch.span();
        // architecture <ident> of <ident> is
        self.format_token_span(TokenSpan::new(span.start_token, arch.is_token()), buffer);
        indented!(buffer, { self.format_declarations(&arch.decl, buffer) });
        buffer.line_break();
        self.format_token_id(arch.begin_token, buffer);
        indented!(buffer, {
            self.format_concurrent_statements(&arch.statements, buffer);
        });
        buffer.line_break();
        // end [architecture] [name];
        self.format_token_span(TokenSpan::new(arch.end_token, span.end_token - 1), buffer);
        self.format_token_id(span.end_token, buffer);
    }
}

#[cfg(test)]
mod test {
    use crate::syntax::test::Code;
    use vhdl_lang::formatting::test_utils::check_formatted;

    fn check_architecture_formatted(input: &str) {
        check_formatted(
            input,
            input,
            Code::architecture_body,
            |formatter, arch, buffer| formatter.format_architecture(arch, buffer),
        )
    }

    #[test]
    fn format_empty_architecture() {
        check_architecture_formatted(
            "\
architecture foo of bar is
begin
end foo;",
        );

        check_architecture_formatted(
            "\
architecture foo of bar is
begin
end architecture foo;",
        );

        check_architecture_formatted(
            "\
architecture foo of bar is
begin
end;",
        );
    }

    #[test]
    fn format_architecture_with_declarations() {
        check_architecture_formatted(
            "\
architecture foo of bar is
    constant x: foo := bar;
begin
end foo;",
        );

        check_architecture_formatted(
            "\
architecture foo of bar is
    constant x: foo := bar;
    signal y: bar := foobar;
begin
end foo;",
        );
    }

    #[test]
    fn format_full_architecture() {
        check_architecture_formatted(
            "\
architecture foo of bar is
    constant x: foo := bar;
    signal y: bar := foobar;
begin
    bar: process(clk) is
        variable z: baz;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                foo <= '0';
            else
                foo <= bar and baz;
            end if;
        end if;
    end process bar;
    y <= x; -- An assignment
end foo;",
        );
    }

    #[test]
    fn format_architecture_preserve_whitespace() {
        check_architecture_formatted(
            "\
architecture foo of bar is
    constant x: foo := bar;
    constant baz: char := '1';

    signal y: bar := foobar;
    signal foobar: std_logic := 'Z';

    shared variable sv: natural;
begin
end foo;",
        );
    }
}
