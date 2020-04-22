use lsp_types::{DocumentSymbol, SymbolKind};
use vhdl_lang::ast::*;

pub trait HasDocumentSymbol {
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

impl HasDocumentSymbol for AnySecondaryUnit {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            AnySecondaryUnit::PackageBody(ref unit) => unit.document_symbol(),
            AnySecondaryUnit::Architecture(ref unit) => unit.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for EntityDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        let mut children = Vec::new();
        if let Some(ref generics) = self.generic_clause {
            let mut symbol = generics.document_symbol();
            symbol.name = String::from("Generics");
            symbol.kind = SymbolKind::Array;
            children.push(symbol);
        }
        if let Some(ref ports) = self.port_clause {
            let mut symbol = ports.document_symbol();
            symbol.name = String::from("Ports");
            symbol.kind = SymbolKind::Struct;
            children.push(symbol);
        }

        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("Entity")),
            kind: SymbolKind::Interface,
            deprecated: None,
            range: {
                if let Some(ref pos) = self.pos {
                    to_lsp_range(pos.range())
                } else {
                    to_lsp_range(self.ident.pos.range())
                }
            },
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: none_if_empty(children),
        }
    }
}

impl HasDocumentSymbol for ConfigurationDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("Configuration")),
            kind: SymbolKind::Class, // @TODO: Map primary units to diverse SymbolKinds?
            deprecated: None,
            // Range should be "full range"
            range: to_lsp_range(self.ident.pos.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for PackageDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("Package")),
            kind: SymbolKind::Module, // @TODO: Map primary units to diverse SymbolKinds?
            deprecated: None,
            // Range should be "full range"
            range: to_lsp_range(self.ident.pos.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for PackageInstantiation {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("Package instance")),
            kind: SymbolKind::Package, // @TODO: Map primary units to diverse SymbolKinds?
            deprecated: None,
            // Range should be "full range"
            range: to_lsp_range(self.ident.pos.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for ContextDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("Context")),
            kind: SymbolKind::Namespace, // @TODO: Map primary units to diverse SymbolKinds?
            deprecated: None,
            // Range should be "full range"
            range: to_lsp_range(self.ident.pos.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for PackageBody {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.item.name_utf8(),
            detail: Some(String::from("Package body")),
            kind: SymbolKind::Package,
            deprecated: None,
            // Range should be "full range"
            range: to_lsp_range(self.ident.item.pos.range()),
            selection_range: to_lsp_range(self.ident.item.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for ArchitectureBody {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from(format!(
                "Architecture of {}",
                self.entity_name.item.item.name().to_string()
            ))),
            kind: SymbolKind::Class,
            deprecated: None,
            // Range should be "full range"
            range: to_lsp_range(self.ident.pos.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

fn interface_declaration_range(interface: &InterfaceDeclaration) -> lsp_types::Range {
    to_lsp_range({
        match interface {
            InterfaceDeclaration::Object(ref object) => object.ident.pos.range(),
            InterfaceDeclaration::File(ref file) => file.ident.pos.range(),
            InterfaceDeclaration::Type(ref ident) => ident.pos.range(),
            InterfaceDeclaration::Subprogram(ref subprogram_declaration, _) => {
                subprogram_declaration.designator().pos.range()
            }
            InterfaceDeclaration::Package(ref package) => package.ident.pos.range(),
        }
    })
}

impl HasDocumentSymbol for ContextClause {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from("Context clause"),
            detail: None,
            kind: SymbolKind::Namespace,
            deprecated: None,
            range: to_lsp_range(self.pos.as_ref().unwrap().range()),
            selection_range: to_lsp_range(self.pos.as_ref().unwrap().range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for Vec<InterfaceDeclaration> {
    fn document_symbol(&self) -> DocumentSymbol {
        let range = {
            if let Some(interface) = self.get(0) {
                lsp_types::Range {
                    start: interface_declaration_range(interface).start,
                    end: interface_declaration_range(self.last().unwrap()).end,
                }
            } else {
                lsp_types::Range {
                    start: lsp_types::Position {
                        character: 0,
                        line: 0,
                    },
                    end: lsp_types::Position {
                        character: 0,
                        line: 0,
                    },
                }
            }
        };

        DocumentSymbol {
            name: String::from("Interface declaration"),
            detail: None,
            kind: SymbolKind::Namespace,
            deprecated: None,
            // Range should be "full range"
            range: range,
            selection_range: range,
            children: none_if_empty(self.iter().map(|i| i.document_symbol()).collect()),
        }
    }
}

impl HasDocumentSymbol for InterfaceDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            InterfaceDeclaration::Object(ref object) => object.document_symbol(),
            InterfaceDeclaration::File(ref file) => file.document_symbol(),
            InterfaceDeclaration::Type(ref ident) => {
                DocumentSymbol {
                    name: ident.item.name_utf8(),
                    detail: Some(String::from("Type")),
                    kind: SymbolKind::TypeParameter,
                    deprecated: None,
                    // Range should be "full range"
                    range: to_lsp_range(ident.pos.range()),
                    selection_range: to_lsp_range(ident.pos.range()),
                    children: None,
                }
            }
            InterfaceDeclaration::Subprogram(ref subprogram_declaration, _) => {
                subprogram_declaration.document_symbol()
            }
            InterfaceDeclaration::Package(ref package) => package.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for InterfaceObjectDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: {
                match self.class {
                    ObjectClass::Constant => None,
                    _ => Some(String::from(format!(": {}", mode_to_string(&self.mode)))), // @TODO signal_kind when implemented
                }
            },
            kind: {
                match self.class {
                    ObjectClass::Signal => SymbolKind::Field,
                    ObjectClass::Constant => SymbolKind::Constant,
                    ObjectClass::Variable => SymbolKind::Variable,
                    ObjectClass::SharedVariable => SymbolKind::Variable,
                }
            },
            deprecated: None,
            // Range should be "full range"
            range: to_lsp_range(self.ident.pos.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for InterfaceFileDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("File")),
            kind: SymbolKind::File,
            deprecated: None,
            // Range should be "full range"
            range: to_lsp_range(self.ident.pos.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

impl HasDocumentSymbol for SubprogramDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            SubprogramDeclaration::Procedure(procedure) => DocumentSymbol {
                name: match procedure.designator.item {
                    SubprogramDesignator::Identifier(ref symbol) => symbol.name_utf8(),
                    SubprogramDesignator::OperatorSymbol(ref latin1string) => {
                        format!("\"{}\"", latin1string.to_string())
                    }
                },
                detail: Some(String::from("Procedure")),
                kind: SymbolKind::Method,
                deprecated: None,
                // Range should be "full range"
                range: to_lsp_range(procedure.designator.pos.range()),
                selection_range: to_lsp_range(procedure.designator.pos.range()),
                children: None,
            },
            SubprogramDeclaration::Function(function) => DocumentSymbol {
                name: match function.designator.item {
                    SubprogramDesignator::Identifier(ref symbol) => symbol.name_utf8(),
                    SubprogramDesignator::OperatorSymbol(ref latin1string) => {
                        format!("\"{}\"", latin1string.to_string())
                    }
                },
                detail: Some(String::from("Function")),
                kind: SymbolKind::Function,
                deprecated: None,
                // Range should be "full range"
                range: to_lsp_range(function.designator.pos.range()),
                selection_range: to_lsp_range(function.designator.pos.range()),
                children: None,
            },
        }
    }
}

impl HasDocumentSymbol for InterfacePackageDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("Package")),
            kind: SymbolKind::Package,
            deprecated: None,
            // Range should be "full range"
            range: to_lsp_range(self.ident.pos.range()),
            selection_range: to_lsp_range(self.ident.pos.range()),
            children: None,
        }
    }
}

// @TODO: This is a copy from vhdl_server.rs
fn to_lsp_pos(position: vhdl_lang::Position) -> lsp_types::Position {
    lsp_types::Position {
        line: position.line as u64,
        character: position.character as u64,
    }
}

// @TODO: This is a copy from vhdl_server.rs
fn to_lsp_range(range: vhdl_lang::Range) -> lsp_types::Range {
    lsp_types::Range {
        start: to_lsp_pos(range.start),
        end: to_lsp_pos(range.end),
    }
}

fn none_if_empty<T>(vector: Vec<T>) -> Option<Vec<T>> {
    if vector.is_empty() {
        None
    } else {
        Some(vector)
    }
}

fn mode_to_string(mode: &Mode) -> String {
    String::from(match mode {
        Mode::In => "in",
        Mode::Out => "out",
        Mode::InOut => "inout",
        Mode::Buffer => "buffer",
        Mode::Linkage => "linkage",
    })
}
