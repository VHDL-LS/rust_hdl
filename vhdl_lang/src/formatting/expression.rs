use crate::ast::token_range::WithTokenSpan;
use crate::ast::{ResolutionIndication, SubtypeIndication, TypeMark};
use crate::formatting::DesignUnitFormatter;

impl DesignUnitFormatter<'_> {
    pub fn format_subtype(&self, subtype: &SubtypeIndication, buffer: &mut String) {
        match &subtype.resolution {
            ResolutionIndication::Unresolved => {}
            _ => unimplemented!(),
        }
        self.format_type_mark(&subtype.type_mark, buffer);
        if let Some(constraint) = &subtype.constraint {
            unimplemented!();
        }
    }

    pub fn format_type_mark(&self, type_mark: &WithTokenSpan<TypeMark>, buffer: &mut String) {
        if let Some(attribute) = type_mark.item.attr {
            unimplemented!();
        }
        self.format_name(&type_mark.item.name, buffer);
    }
}
