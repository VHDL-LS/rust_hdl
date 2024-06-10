use crate::vhdl_server::{
    from_lsp_pos, srcpos_to_location, to_lsp_range, uri_to_file_name, VHDLServer,
};
use lsp_types::{
    PrepareRenameResponse, RenameParams, TextDocumentPositionParams, TextEdit, Url, WorkspaceEdit,
};
use std::collections::HashMap;
use vhdl_lang::ast::Designator;

impl VHDLServer {
    pub fn prepare_rename(
        &mut self,
        params: &TextDocumentPositionParams,
    ) -> Option<PrepareRenameResponse> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;

        let (pos, ent) = self
            .project
            .item_at_cursor(&source, from_lsp_pos(params.position))?;

        if let Designator::Identifier(_) = ent.designator() {
            Some(PrepareRenameResponse::Range(to_lsp_range(pos.range)))
        } else {
            // It does not make sense to rename operator symbols and character literals
            // Also they have different representations that would not be handled consistently
            // Such as function "+"(arg1, arg2 : integer) but used as foo + bar
            None
        }
    }

    pub fn rename(&mut self, params: &RenameParams) -> Option<WorkspaceEdit> {
        let source = self.project.get_source(&uri_to_file_name(
            &params.text_document_position.text_document.uri,
        ))?;

        let ent = self.project.find_declaration(
            &source,
            from_lsp_pos(params.text_document_position.position),
        )?;

        let mut changes: HashMap<Url, Vec<TextEdit>> = Default::default();

        for srcpos in self.project.find_all_references(ent) {
            let loc = srcpos_to_location(&srcpos);
            changes.entry(loc.uri).or_default().push(TextEdit {
                range: loc.range,
                new_text: params.new_name.clone(),
            });
        }

        Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        })
    }
}
