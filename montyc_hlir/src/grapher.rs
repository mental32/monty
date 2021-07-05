use std::iter::Peekable;

use montyc_parser::{
    ast::{self, Statement},
    spanned::Spanned,
    AstObject, AstVisitor,
};

use petgraph::graph::{DiGraph, NodeIndex};

pub type AstNodeGraph = DiGraph<ast::AstNode, ()>;

pub struct NewType<T>(pub T);

pub struct NodeGrapher<'a> {
    graph: AstNodeGraph,
    last: Option<NodeIndex<u32>>,
    seq: Peekable<std::slice::Iter<'a, Spanned<Statement>>>,
}

impl NodeGrapher<'_> {
    fn consume(mut self) -> AstNodeGraph {
        while let Some(part) = self.seq.next() {
            part.visit_with(&mut self);
        }

        self.graph
    }

    fn add_node(&mut self, node: &impl AstObject) {
        let to = self.graph.add_node(node.into_ast_node());

        if let Some(previous) = self.last {
            self.graph.add_edge(previous, to, ());
        }

        self.last.replace(to);
    }
}

impl AstVisitor for NodeGrapher<'_> {
    fn visit_any(&mut self, node: &dyn AstObject) {
        self.add_node(&node.into_ast_node());
    }

    fn visit_ifstmt(&mut self, ifstmt: &ast::IfChain) {
        let mut predecessor = self.last.take();
        let mut branch_leaves = vec![];

        // Take the next node in the sequence, can never be none because we artificially
        // append one extra `Statement::Pass` node.
        let after = self.seq.next().unwrap();

        after.visit_with(self);

        let after_ix = self
            .last
            .take()
            .expect("The node after this ifstmt should have set `self.last`");

        while let Some((edge, _)) = self
            .graph
            .neighbors_directed(after_ix, petgraph::EdgeDirection::Outgoing)
            .detach()
            .next(&self.graph)
        {
            self.graph.remove_edge(edge);
        }

        for branch in ifstmt.branches.iter() {
            self.last = predecessor;
            self.add_node(branch);
            predecessor = self.last.clone();

            let (last, first) = branch.inner.body.split_last().unwrap();

            for part in first {
                part.visit_with(self);
            }

            last.visit_with(self);

            let leaf = self.last.unwrap();

            self.graph.add_edge(leaf, after_ix, ());

            branch_leaves.push(leaf);
        }

        if let Some(orelse) = &ifstmt.orelse {
            self.last = predecessor;

            let (last, first) = orelse.split_last().unwrap();

            for part in first {
                part.visit_with(self);
            }

            last.visit_with(self);

            let leaf = self.last.unwrap();

            self.graph.add_edge(leaf, after_ix, ());
        }

        self.last.replace(after_ix);
    }
}


impl From<NewType<ast::FunctionDef>> for AstNodeGraph {
    fn from(NewType(mut funcdef): NewType<ast::FunctionDef>) -> Self {
        // Insert a universal "entry" node.
        funcdef.body.insert(
            0,
            Spanned {
                span: 0..0,
                inner: ast::Statement::Pass,
            },
        );

        // Insert a universal "exit" node.
        funcdef.body.push(Spanned {
            span: 0..0,
            inner: ast::Statement::Pass,
        });

        funcdef.body.retain(|node| {
            !matches!(
                node.inner,
                ast::Statement::Expr(ast::Expr::Primary(Spanned {
                    inner: ast::Primary::Atomic(Spanned {
                        inner: ast::Atom::Comment(_),
                        ..
                    }),
                    ..
                }))
            )
        });

        let graph = AstNodeGraph::new();
        let grapher = NodeGrapher {
            graph,
            last: None,
            seq: funcdef.body.iter().peekable(),
        };

        grapher.consume()
    }
}


impl From<NewType<ast::Module>> for AstNodeGraph {
    fn from(NewType(mut module): NewType<ast::Module>) -> Self {
        // Insert a universal "entry" node.
        module.body.insert(
            0,
            Spanned {
                span: 0..0,
                inner: ast::Statement::Pass,
            },
        );

        // Insert a universal "exit" node.
        module.body.push(Spanned {
            span: 0..0,
            inner: ast::Statement::Pass,
        });

        module.body.retain(|node| {
            !matches!(
                node.inner,
                ast::Statement::Expr(ast::Expr::Primary(Spanned {
                    inner: ast::Primary::Atomic(Spanned {
                        inner: ast::Atom::Comment(_),
                        ..
                    }),
                    ..
                }))
            )
        });

        let graph = AstNodeGraph::new();
        let grapher = NodeGrapher {
            graph,
            last: None,
            seq: module.body.iter().peekable(),
        };

        grapher.consume()
    }
}
