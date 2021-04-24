use std::collections::BTreeMap;
use std::collections::BTreeSet;

use crate::prelude::*;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[repr(transparent)]
pub struct BlockId(usize);

#[derive(Debug, Default)]
pub struct Block<T> {
    pub preds: BTreeSet<BlockId>,
    pub succs: BTreeSet<BlockId>,
    pub nodes: Vec<T>,
}

#[derive(Debug)]
pub struct LayoutIter<'a, T> {
    start: BlockId,
    pending: Vec<BlockId>,
    layout: &'a Layout<T>,
}

impl<'a, T> Iterator for LayoutIter<'a, T> {
    type Item = (BlockId, &'a Block<T>);

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.pending.pop()?;
        let block = self.layout.blocks.get(&current).unwrap();
        Some((current, block))
    }
}

/// Represent then layout of a given AST.
///
/// Inspired by `cranelift_codegen::ir::Layout` this
/// structure similarly only determines:
///
/// * The order of blocks in a given scope, and
/// * The order of nodes within a block.
///
#[derive(Debug)]
pub struct Layout<T> {
    pub start: BlockId,
    pub end: BlockId,
    pub(in crate) blocks: BTreeMap<BlockId, Block<T>>,
}

impl<T> Layout<T> {
    pub fn new() -> Self {
        let mut blocks = BTreeMap::default();

        blocks.insert(
            BlockId(0),
            Block {
                nodes: vec![],
                preds: BTreeSet::default(),
                succs: BTreeSet::default(),
            },
        );

        blocks.insert(
            BlockId(1),
            Block {
                nodes: vec![],
                preds: BTreeSet::default(),
                succs: BTreeSet::default(),
            },
        );

        Self {
            start: BlockId(0),
            end: BlockId(1),
            blocks,
        }
    }

    pub fn iter_from(&self, start: BlockId) -> impl Iterator<Item = (BlockId, &Block<T>)> {
        let mut pending = vec![];
        let mut processing = vec![start];

        while let Some(current) = processing.pop() {
            let block = self.blocks.get(&current).unwrap();

            for pred in block.succs.iter().cloned() {
                pending.push(pred);
                processing.push(pred);
            }
        }

        LayoutIter {
            start,
            pending,
            layout: self,
        }
    }

    pub fn rev_iter(&self, start: BlockId) -> impl Iterator<Item = (BlockId, &Block<T>)> {
        let mut pending = vec![];
        let mut processing = vec![start];

        while let Some(current) = processing.pop() {
            let block = self.blocks.get(&current).unwrap();

            for pred in block.preds.iter().cloned() {
                pending.push(pred);
                processing.push(pred);
            }
        }

        LayoutIter {
            start,
            pending,
            layout: self,
        }
    }

    pub fn insert_into_new_block(&mut self, t: T) -> BlockId {
        let block = self.create_new_block();

        self.blocks.get_mut(&block).unwrap().nodes.push(t);

        block
    }

    /// Create a sub-layout by translating an external layout.
    pub fn with_sublayout_from(&mut self, t: impl Lower<Output = Layout<T>>) -> (BlockId, BlockId)
    where
        T: Clone,
    {
        let roughly = t.lower();
        self.with_sublayout(move |superlayout, sublayout| {
            let mut translate = BTreeMap::new();

            for old_id in roughly.blocks.keys().cloned() {
                if old_id == roughly.start {
                    translate.insert(old_id, sublayout.start);
                } else if old_id == roughly.end {
                    translate.insert(old_id, sublayout.end);
                } else {
                    translate.insert(old_id, sublayout.create_new_block());
                }
            }

            for (old_id, old_block) in roughly.blocks.iter() {
                let new_id = translate[old_id];
                let old_id = *old_id;

                let new_block = sublayout
                    .blocks
                    .get_mut(&new_id)
                    .or(superlayout.blocks.get_mut(&new_id))
                    .unwrap();

                if old_id == roughly.start {
                    new_block.succs = old_block.succs.iter().map(|v| translate[v]).collect();
                } else if old_id == roughly.end {
                    new_block.preds = old_block.preds.iter().map(|v| translate[v]).collect();
                } else {
                    new_block.preds = old_block.preds.iter().map(|v| translate[v]).collect();
                    new_block.succs = old_block.succs.iter().map(|v| translate[v]).collect();
                    new_block.nodes = old_block.nodes.iter().cloned().collect();
                };
            }
        })
    }

    pub fn with_sublayout(&mut self, f: impl Fn(&mut Self, &mut Self)) -> (BlockId, BlockId) {
        let start = self.create_new_block();
        let end = self.create_new_block();

        let mut subgraph = Self {
            start,
            end,
            blocks: BTreeMap::default(),
        };

        subgraph
            .blocks
            .insert(start, self.blocks.remove(&start).unwrap());

        subgraph
            .blocks
            .insert(end, self.blocks.remove(&end).unwrap());

        f(self, &mut subgraph);

        self.blocks.extend(subgraph.blocks);

        (start, end)
    }

    pub fn with_block<U>(&mut self, f: impl Fn(&mut Self, BlockId) -> U) -> U {
        let block = self.create_new_block();

        f(self, block)
    }

    pub fn succeed(&mut self, from: BlockId, to: BlockId) {
        self.blocks
            .get_mut(&from)
            .map(|block| block.succs.insert(to))
            .unwrap();
        self.blocks
            .get_mut(&to)
            .map(|block| block.preds.insert(from))
            .unwrap();
    }

    pub fn insert_block(&mut self, key: BlockId, value: Option<Block<T>>) -> Option<Block<T>> {
        let block = match value {
            Some(inner) => inner,
            None => Block {
                preds: BTreeSet::new(),
                succs: BTreeSet::new(),
                nodes: vec![],
            },
        };

        self.blocks.insert(key, block)
    }

    pub fn create_new_block(&mut self) -> BlockId {
        let block = Block {
            preds: BTreeSet::new(),
            succs: BTreeSet::new(),
            nodes: vec![],
        };

        let key = self
            .blocks
            .keys()
            .max()
            .map(|BlockId(n)| n + 1)
            .unwrap_or(2);

        let key = BlockId(key);

        self.blocks.insert(key, block);

        key
    }

    pub fn reduce_forwarding_edges(&mut self) {
        // get all blocks that make up a forward edge (a block that only has a direct jump `x -> y` and no body)
        let is_forward_edge = |(id, block): (&BlockId, &Block<T>)| {
            if block.nodes.is_empty() && (block.succs.len() == 1 && block.preds.len() == 1) {
                let from = block.preds.iter().next().cloned().unwrap();
                let to = block.succs.iter().next().cloned().unwrap();

                Some((*id, from, to))
            } else {
                None
            }
        };

        let forwarding_edges = self
            .blocks
            .iter()
            .filter_map(is_forward_edge)
            .collect::<Vec<_>>();

        // now remove them and patch the surrounding blocks.
        for (to_be_dropped, from, to) in forwarding_edges {
            let _ = self.blocks.remove(&to_be_dropped);

            log::trace!(
                "layouyt:reduce_forwarding_edges forwarding({:?} -> {:?}) = {:?}",
                from,
                to,
                to_be_dropped
            );

            for block in self.blocks.values_mut() {
                if block.succs.remove(&to_be_dropped) {
                    block.succs.insert(to);
                }

                if block.preds.remove(&to_be_dropped) {
                    block.preds.insert(from);
                }
            }
        }
    }
}
