// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use convert_case::{Case, Casing};
use serde::de::{MapAccess, Visitor};
use serde::{Deserialize, Deserializer, Serialize};
use std::fmt;

#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Node {
    pub name: String,
    pub contents: NodeContents,
}

impl Node {
    pub fn new(name: impl Into<String>, contents: NodeContents) -> Node {
        Node {
            name: name.into(),
            contents,
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Nodes(Vec<Node>);

impl From<Nodes> for Vec<Node> {
    fn from(value: Nodes) -> Self {
        value.0
    }
}

impl IntoIterator for Nodes {
    type Item = Node;
    type IntoIter = <Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

struct NodesVisitor;

impl<'de> Visitor<'de> for NodesVisitor {
    type Value = Nodes;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a node")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut res = Vec::with_capacity(map.size_hint().unwrap_or(0));
        while let Some((key, value)) = map.next_entry::<String, NodeContents>()? {
            res.push(Node {
                name: key,
                contents: value,
            })
        }
        Ok(Nodes(res))
    }
}

impl<'de> Deserialize<'de> for Nodes {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(NodesVisitor)
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub enum NodeContents {
    Sequence(Vec<NodeOrToken>),
    Choice(Vec<NodeOrToken>),
    Builtin,
    /// A node whose children are captured by the parser as raw tokens rather than
    /// being composed from sub-nodes. Never produces an empty green node at runtime.
    RawTokens,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
#[serde(untagged)]
pub enum NodeOrToken {
    Node(NodeRef),
    Token(TokenRef),
    Keyword(KeywordRef),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct NodeRef {
    pub node: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub terminated: Option<String>,
    /// Custom name. If none, the node's name is taken.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub repeated: bool,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub parenthesized: bool,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub optional: bool,
}

impl NodeRef {
    pub fn name(&self) -> String {
        if self.name.is_none() && self.repeated {
            format!("{}s", self.node.to_case(Case::Snake))
        } else {
            self.name
                .clone()
                .unwrap_or_else(|| self.node.to_case(Case::Snake))
        }
    }

    pub fn kind(&self) -> String {
        self.node.clone()
    }
}

impl From<String> for NodeRef {
    fn from(value: String) -> Self {
        Self {
            node: value,
            name: None,
            terminated: None,
            parenthesized: false,
            repeated: false,
            optional: false,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct TokenRef {
    pub token: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub repeated: bool,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub optional: bool,
}

fn token_str_name(name: Option<String>, token: &str) -> String {
    name.unwrap_or_else(|| token.to_owned())
}

impl TokenRef {
    pub fn name(&self) -> String {
        token_str_name(self.name.clone(), &self.token)
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct KeywordRef {
    pub keyword: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub optional: bool,
}

impl KeywordRef {
    pub fn name(&self) -> String {
        token_str_name(self.name.clone(), &self.keyword)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_node_with_optional_true() {
        // The YAML format uses explicit tags like !Sequence / !Choice
        let yaml = r#"
TestNode: !Sequence
  - node: ChildNode
    optional: true
"#;
        let nodes: Nodes = serde_yml::from_str(yaml).expect("parse failed");
        let nodes: Vec<Node> = nodes.into();
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].name, "TestNode");
        let NodeContents::Sequence(ref items) = nodes[0].contents else {
            panic!("expected Sequence");
        };
        assert_eq!(items.len(), 1);
        let NodeOrToken::Node(ref node_ref) = items[0] else {
            panic!("expected Node");
        };
        assert!(node_ref.optional);
    }

    #[test]
    fn parse_node_without_optional_defaults_false() {
        let yaml = r#"
TestNode: !Sequence
  - node: ChildNode
"#;
        let nodes: Nodes = serde_yml::from_str(yaml).expect("parse failed");
        let nodes: Vec<Node> = nodes.into();
        let NodeContents::Sequence(ref items) = nodes[0].contents else {
            panic!("expected Sequence");
        };
        let NodeOrToken::Node(ref node_ref) = items[0] else {
            panic!("expected Node");
        };
        assert!(!node_ref.optional);
    }

    #[test]
    fn parse_token_with_optional_true() {
        let yaml = r#"
TestNode: !Sequence
  - token: SemiColon
    optional: true
"#;
        let nodes: Nodes = serde_yml::from_str(yaml).expect("parse failed");
        let nodes: Vec<Node> = nodes.into();
        let NodeContents::Sequence(ref items) = nodes[0].contents else {
            panic!("expected Sequence");
        };
        let NodeOrToken::Token(ref token_ref) = items[0] else {
            panic!("expected Token");
        };
        assert!(token_ref.optional);
    }

    #[test]
    fn parse_keyword_with_optional_defaults_false() {
        let yaml = r#"
TestNode: !Sequence
  - keyword: Entity"#;
        let nodes: Nodes = serde_yml::from_str(yaml).expect("parse failed");
        let nodes: Vec<Node> = nodes.into();
        let NodeContents::Sequence(ref items) = nodes[0].contents else {
            panic!("expected Sequence");
        };
        let NodeOrToken::Keyword(ref kw_ref) = items[0] else {
            panic!("expected Keyword");
        };
        assert!(!kw_ref.optional);
    }
}
