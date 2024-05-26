use crate::vhdl_server::{
    from_lsp_pos, from_lsp_range, srcpos_to_location, uri_to_file_name, NonProjectFileHandling,
    VHDLServer,
};
use lsp_types::{
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionResponse, Hover,
    HoverContents, Location, MarkupContent, MarkupKind, ReferenceParams, TextDocumentItem,
    TextDocumentPositionParams,
};
use vhdl_lang::{Message, Source};

impl VHDLServer {
    pub fn text_document_did_open_notification(&mut self, params: &DidOpenTextDocumentParams) {
        let TextDocumentItem { uri, text, .. } = &params.text_document;
        let file_name = uri_to_file_name(uri);
        if let Some(source) = self.project.get_source(&file_name) {
            source.change(None, text);
            self.project.update_source(&source);
            self.publish_diagnostics();
        } else {
            match self.settings.non_project_file_handling {
                NonProjectFileHandling::Ignore => {}
                NonProjectFileHandling::Analyze => {
                    self.message(Message::warning(format!(
                        "Opening file {} that is not part of the project",
                        file_name.to_string_lossy()
                    )));
                    self.project
                        .update_source(&Source::inline(&file_name, text));
                    self.publish_diagnostics();
                }
            }
        }
    }

    pub fn text_document_did_change_notification(&mut self, params: &DidChangeTextDocumentParams) {
        let file_name = uri_to_file_name(&params.text_document.uri);
        if let Some(source) = self.project.get_source(&file_name) {
            for content_change in params.content_changes.iter() {
                let range = content_change.range.map(from_lsp_range);
                source.change(range.as_ref(), &content_change.text);
            }
            self.project.update_source(&source);
            self.publish_diagnostics();
        } else if self.settings.non_project_file_handling != NonProjectFileHandling::Ignore {
            self.message(Message::error(format!(
                "Changing file {} that is not part of the project",
                file_name.to_string_lossy()
            )));
        }
    }

    pub fn text_document_declaration(
        &mut self,
        params: &TextDocumentPositionParams,
    ) -> Option<Location> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;

        let ent = self
            .project
            .find_declaration(&source, from_lsp_pos(params.position))?;
        Some(srcpos_to_location(ent.decl_pos()?))
    }

    pub fn text_document_definition(
        &mut self,
        params: &TextDocumentPositionParams,
    ) -> Option<Location> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;

        let ent = self
            .project
            .find_definition(&source, from_lsp_pos(params.position))?;
        Some(srcpos_to_location(ent.decl_pos()?))
    }

    pub fn text_document_implementation(
        &mut self,
        params: &TextDocumentPositionParams,
    ) -> Option<GotoDefinitionResponse> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;

        let ents = self
            .project
            .find_implementation(&source, from_lsp_pos(params.position));

        Some(GotoDefinitionResponse::Array(
            ents.into_iter()
                .filter_map(|ent| ent.decl_pos().map(srcpos_to_location))
                .collect(),
        ))
    }

    pub fn text_document_hover(&mut self, params: &TextDocumentPositionParams) -> Option<Hover> {
        let source = self
            .project
            .get_source(&uri_to_file_name(&params.text_document.uri))?;
        let ent = self
            .project
            .find_declaration(&source, from_lsp_pos(params.position))?;

        let value = self.project.format_declaration(ent)?;

        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("```vhdl\n{value}\n```"),
            }),
            range: None,
        })
    }

    pub fn text_document_references(&mut self, params: &ReferenceParams) -> Vec<Location> {
        let ent = self
            .project
            .get_source(&uri_to_file_name(
                &params.text_document_position.text_document.uri,
            ))
            .and_then(|source| {
                self.project.find_declaration(
                    &source,
                    from_lsp_pos(params.text_document_position.position),
                )
            });

        if let Some(ent) = ent {
            self.project
                .find_all_references(ent)
                .iter()
                .map(srcpos_to_location)
                .collect()
        } else {
            Vec::new()
        }
    }
}
