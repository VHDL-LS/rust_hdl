// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2020, Olof Kraigher olof.kraigher@gmail.com

use lsp_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind, Url};
use vhdl_lang::ast::*;
use vhdl_lang::VHDLParser;

pub fn nested_document_symbol_response_from_file(uri: &Url) -> Option<DocumentSymbolResponse> {
    if let Some(design_file) = parse_file(uri) {
        Some(nested_document_symbol_response(&design_file))
    } else {
        None
    }
}

pub fn nested_document_symbol_response(design_file: &DesignFile) -> DocumentSymbolResponse {
    let mut response = vec![];
    for design_unit in design_file.design_units.iter() {
        response.push(design_unit.document_symbol());
    }
    DocumentSymbolResponse::from(response)
}

fn symbol_kind(entity_class: EntityClass) -> SymbolKind {
    match entity_class {
        EntityClass::Entity => SymbolKind::Interface,
        EntityClass::Architecture => SymbolKind::Class,
        EntityClass::Configuration => SymbolKind::Constructor,
        EntityClass::Package => SymbolKind::Package,
        _ => SymbolKind::Unknown,
    }
}

fn symbol_kind_for_context() -> SymbolKind {
    SymbolKind::Namespace
}

trait HasDocumentSymbol {
    fn document_symbol(&self) -> DocumentSymbol;
}

impl HasDocumentSymbol for AnyDesignUnit {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            AnyDesignUnit::Primary(ref unit) => unit.document_symbol(),
            AnyDesignUnit::Secondary(ref unit) => unit.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for AnyPrimaryUnit {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            AnyPrimaryUnit::Entity(ref unit) => unit.document_symbol(),
            AnyPrimaryUnit::Configuration(ref unit) => unit.document_symbol(),
            AnyPrimaryUnit::Package(ref unit) => unit.document_symbol(),
            AnyPrimaryUnit::PackageInstance(ref unit) => unit.document_symbol(),
            AnyPrimaryUnit::Context(ref unit) => unit.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for EntityDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("entity")),
            kind: symbol_kind(EntityClass::Entity),
            deprecated: None,
            range: to_lsp_range(self.source_range.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for ConfigurationDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("configuration")),
            kind: symbol_kind(EntityClass::Configuration),
            deprecated: None,
            range: to_lsp_range(self.source_range.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for PackageDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("package")),
            kind: symbol_kind(EntityClass::Package),
            deprecated: None,
            range: to_lsp_range(self.source_range.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for PackageInstantiation {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("package instance")),
            kind: symbol_kind(EntityClass::Package),
            deprecated: None,
            range: to_lsp_range(self.source_range.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for ContextDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("context")),
            kind: symbol_kind_for_context(),
            deprecated: None,
            range: to_lsp_range(self.source_range.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for AnySecondaryUnit {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            AnySecondaryUnit::PackageBody(ref unit) => unit.document_symbol(),
            AnySecondaryUnit::Architecture(ref unit) => unit.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for PackageBody {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.item.name_utf8(),
            detail: Some(String::from("package body")),
            kind: symbol_kind(EntityClass::Package),
            deprecated: None,
            range: to_lsp_range(self.source_range.range()),
            selection_range: to_lsp_range(self.ident.item.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for ArchitectureBody {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(format!(
                "architecture of {}",
                self.entity_name.item.item.name().to_string()
            )),
            kind: symbol_kind(EntityClass::Architecture),
            deprecated: None,
            range: to_lsp_range(self.source_range.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

fn parse_file(uri: &Url) -> Option<DesignFile> {
    match uri.to_file_path() {
        Ok(url) => {
            let mut diagnostics = vec![];
            match VHDLParser::default().parse_design_file(&url, &mut diagnostics) {
                Ok((_, design_file)) => Some(design_file),
                Err(_) => None,
            }
        }
        _ => None,
    }
}

fn to_lsp_pos(position: vhdl_lang::Position) -> lsp_types::Position {
    lsp_types::Position {
        line: position.line as u64,
        character: position.character as u64,
    }
}

fn to_lsp_range(range: vhdl_lang::Range) -> lsp_types::Range {
    lsp_types::Range {
        start: to_lsp_pos(range.start),
        end: to_lsp_pos(range.end),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types;
    use pretty_assertions::assert_eq;
    use std::io::Write;
    use std::path::Path;
    use tempfile::NamedTempFile;
    use vhdl_lang::Source;

    fn parse_str(code: &str) -> DesignFile {
        let mut diagnostics = vec![];
        let source = Source::inline(Path::new("mockpath"), code);
        let parser = VHDLParser::default();
        let design_file = parser.parse_design_source(&source, &mut diagnostics);
        for err in diagnostics.iter() {
            println!("{}", err.show());
        }
        if diagnostics.len() > 0 {
            panic!("Found errors");
        }
        design_file
    }

    fn range(start: (u64, u64), end: (u64, u64)) -> lsp_types::Range {
        let (start_line, start_character) = start;
        let (end_line, end_character) = end;
        lsp_types::Range {
            start: lsp_types::Position::new(start_line, start_character),
            end: lsp_types::Position::new(end_line, end_character),
        }
    }

    fn write_source_file(code: &str) -> (Url, NamedTempFile) {
        let mut file = NamedTempFile::new().unwrap();
        file.write_all(code.as_bytes()).unwrap();
        (
            Url::from_file_path(file.path().canonicalize().unwrap()).unwrap(),
            file,
        )
    }

    #[test]
    fn entity_declaration() {
        let design_file = parse_str(
            "
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent1 is
end;
",
        );
        assert_eq!(design_file.design_units.len(), 1);
        let entity = match design_file.design_units.first().unwrap() {
            AnyDesignUnit::Primary(primary) => match primary {
                AnyPrimaryUnit::Entity(entity) => entity,
                _ => panic!("expected entity"),
            },
            _ => panic!("expected entity"),
        };

        assert_eq!(
            entity.document_symbol(),
            DocumentSymbol {
                name: String::from("ent1"),
                detail: Some(String::from("entity")),
                kind: SymbolKind::Interface,
                deprecated: None,
                range: range((1, 0), (6, 4)),
                selection_range: range((5, 7), (5, 11)),
                children: None,
            }
        );
    }

    #[test]
    fn configuration_declaration() {
        let design_file = parse_str(
            "
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

configuration cfg of entity_name is
    for rtl(0)
    end for;
end;
",
        );
        assert_eq!(design_file.design_units.len(), 1);
        let configuration = match design_file.design_units.first().unwrap() {
            AnyDesignUnit::Primary(primary) => match primary {
                AnyPrimaryUnit::Configuration(configuration) => configuration,
                _ => panic!("expected configuration declaration"),
            },
            _ => panic!("expected configuration declaration"),
        };

        assert_eq!(
            configuration.document_symbol(),
            DocumentSymbol {
                name: String::from("cfg"),
                detail: Some(String::from("configuration")),
                kind: SymbolKind::Constructor,
                deprecated: None,
                range: range((1, 0), (8, 4)),
                selection_range: range((5, 14), (5, 17)),
                children: None,
            }
        );
    }

    #[test]
    fn package_declaration() {
        let design_file = parse_str(
            "
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pkg is
end;
",
        );
        assert_eq!(design_file.design_units.len(), 1);
        let package = match design_file.design_units.first().unwrap() {
            AnyDesignUnit::Primary(primary) => match primary {
                AnyPrimaryUnit::Package(package) => package,
                _ => panic!("expected package declaration"),
            },
            _ => panic!("expected package declaration"),
        };

        assert_eq!(
            package.document_symbol(),
            DocumentSymbol {
                name: String::from("pkg"),
                detail: Some(String::from("package")),
                kind: SymbolKind::Package,
                deprecated: None,
                range: range((1, 0), (6, 4)),
                selection_range: range((5, 8), (5, 11)),
                children: None,
            }
        );
    }

    #[test]
    fn package_instantiation() {
        let design_file = parse_str(
            "
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pkg_inst is new work.pkg
    generic map(
        gen => 1
    );
",
        );
        assert_eq!(design_file.design_units.len(), 1);
        let package_instance = match design_file.design_units.first().unwrap() {
            AnyDesignUnit::Primary(primary) => match primary {
                AnyPrimaryUnit::PackageInstance(package_instance) => package_instance,
                _ => panic!("expected package declaration"),
            },
            _ => panic!("expected package declaration"),
        };

        assert_eq!(
            package_instance.document_symbol(),
            DocumentSymbol {
                name: String::from("pkg_inst"),
                detail: Some(String::from("package instance")),
                kind: SymbolKind::Package,
                deprecated: None,
                range: range((1, 0), (8, 6)),
                selection_range: range((5, 8), (5, 16)),
                children: None,
            }
        );
    }

    #[test]
    fn context_declaration() {
        let design_file = parse_str(
            "
context ctx is
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
end;
",
        );
        assert_eq!(design_file.design_units.len(), 1);
        let context = match design_file.design_units.first().unwrap() {
            AnyDesignUnit::Primary(primary) => match primary {
                AnyPrimaryUnit::Context(context) => context,
                _ => panic!("expected package declaration"),
            },
            _ => panic!("expected package declaration"),
        };

        assert_eq!(
            context.document_symbol(),
            DocumentSymbol {
                name: String::from("ctx"),
                detail: Some(String::from("context")),
                kind: SymbolKind::Namespace,
                deprecated: None,
                range: range((1, 0), (5, 4)),
                selection_range: range((1, 8), (1, 11)),
                children: None,
            }
        );
    }

    #[test]
    fn package_body() {
        let design_file = parse_str(
            "
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package body pkg is
end;
",
        );
        assert_eq!(design_file.design_units.len(), 1);
        let package_body = match design_file.design_units.first().unwrap() {
            AnyDesignUnit::Secondary(secondary) => match secondary {
                AnySecondaryUnit::PackageBody(package_body) => package_body,
                _ => panic!("expected package declaration"),
            },
            _ => panic!("expected package declaration"),
        };

        assert_eq!(
            package_body.document_symbol(),
            DocumentSymbol {
                name: String::from("pkg"),
                detail: Some(String::from("package body")),
                kind: SymbolKind::Package,
                deprecated: None,
                range: range((1, 0), (6, 4)),
                selection_range: range((5, 13), (5, 16)),
                children: None,
            }
        );
    }

    #[test]
    fn architecture_body() {
        let design_file = parse_str(
            "
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture rtl of ent is
begin
end;
",
        );
        assert_eq!(design_file.design_units.len(), 1);
        let architecture = match design_file.design_units.first().unwrap() {
            AnyDesignUnit::Secondary(secondary) => match secondary {
                AnySecondaryUnit::Architecture(architecture) => architecture,
                _ => panic!("expected package declaration"),
            },
            _ => panic!("expected package declaration"),
        };

        assert_eq!(
            architecture.document_symbol(),
            DocumentSymbol {
                name: String::from("rtl"),
                detail: Some(String::from("architecture of ent")),
                kind: SymbolKind::Class,
                deprecated: None,
                range: range((1, 0), (7, 4)),
                selection_range: range((5, 13), (5, 16)),
                children: None,
            }
        );
    }

    #[test]
    fn test_nested_document_symbol_response_from_file() {
        let (source_url, _file) = write_source_file(
            "
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent1 is
end;
",
        );
        let response = nested_document_symbol_response_from_file(&source_url).unwrap();
        assert_eq!(
            response,
            DocumentSymbolResponse::from(vec![DocumentSymbol {
                name: String::from("ent1"),
                detail: Some(String::from("entity")),
                kind: SymbolKind::Interface,
                deprecated: None,
                range: range((1, 0), (6, 4)),
                selection_range: range((5, 7), (5, 11)),
                children: None,
            }])
        );
    }
}
