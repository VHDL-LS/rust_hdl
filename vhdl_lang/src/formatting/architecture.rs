use crate::ast::ArchitectureBody;
use crate::formatting::DesignUnitFormatter;
use crate::{HasTokenSpan, TokenSpan};

impl DesignUnitFormatter<'_> {
    pub fn format_architecture(&self, arch: &ArchitectureBody, buffer: &mut String) {
        self.format_context_clause(&arch.context_clause, buffer);
        let span = arch.span();
        // architecture <ident> of <ident> is
        self.format_token_span(TokenSpan::new(span.start_token, arch.is_token()), buffer);
        self.increase_indentation();
        self.format_declarations(&arch.decl, buffer);
        self.decrease_indentation();
        self.newline(buffer);
        self.format_token_id(arch.begin_token, buffer);
        self.newline(buffer);
        self.increase_indentation();
        self.format_concurrent_statements(&arch.statements, buffer);
        self.decrease_indentation();
        // end [architecture] [name];
        self.format_token_span(TokenSpan::new(arch.end_token, span.end_token - 1), buffer);
        self.format_token_id(span.end_token, buffer);
    }
}

#[cfg(test)]
mod test {
    use crate::formatting::DesignUnitFormatter;
    use crate::syntax::test::Code;

    fn check_architecture_formatted(input: &str, expected: &str) {
        let code = Code::new(input);
        let arch = code.architecture_body();
        let tokens = code.tokenize();
        let formatter = DesignUnitFormatter::new(&tokens);
        let mut buffer = String::new();
        formatter.format_architecture(&arch, &mut buffer);
        assert_eq!(&buffer, expected);
    }

    #[test]
    fn format_empty_architecture() {
        check_architecture_formatted(
            "architecture foo of bar is begin end foo;",
            "\
architecture foo of bar is
begin
end foo;",
        );

        check_architecture_formatted(
            "architecture foo of bar is begin end architecture foo;",
            "\
architecture foo of bar is
begin
end architecture foo;",
        );

        check_architecture_formatted(
            "architecture foo of bar is begin end;",
            "\
architecture foo of bar is
begin
end;",
        );
    }

    #[test]
    fn format_architecture_with_declarations() {
        check_architecture_formatted(
            "
architecture foo of bar is
constant x: foo := bar;
begin
end foo;",
            "\
architecture foo of bar is
    constant x: foo := bar;
begin
end foo;",
        );

        check_architecture_formatted(
            "
architecture foo of bar is
constant x: foo := bar;
signal y : bar := foobar;
begin
end foo;",
            "\
architecture foo of bar is
    constant x: foo := bar;
    signal y: bar := foobar;
begin
end foo;",
        );
    }
}
