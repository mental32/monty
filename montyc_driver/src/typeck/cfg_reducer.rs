use montyc_core::MontyResult;

use crate::global_context::SessionContext;

pub trait GraphLike<IndexT> {
    fn n_nodes(&self) -> usize;
}

pub(crate) trait CFGReducer {
    type IndexT: Eq + Ord + std::fmt::Debug + Copy;
    type InputGraphT: GraphLike<Self::IndexT>;
    type OutputT;

    fn make_output(&self) -> Self::OutputT;

    fn cfg_ref(&self) -> &Self::InputGraphT;
    fn cfg_mut_ref(&mut self) -> &mut Self::InputGraphT;

    fn visit_block(
        &mut self,
        cx: &SessionContext,
        output: &mut Self::OutputT,
        ix: Self::IndexT,
        errors: &mut Vec<montyc_core::error::TypeError>,
    ) -> MontyResult<Vec<Self::IndexT>>;

    fn visit(&mut self, cx: &SessionContext, entry: Self::IndexT) -> MontyResult<Self::OutputT> {
        let cfg_capacity = self.cfg_ref().n_nodes();

        let mut blocks_processed = Vec::with_capacity(cfg_capacity);
        let mut blocks_to_analyze = std::collections::VecDeque::with_capacity(cfg_capacity);

        blocks_to_analyze.push_front(entry);

        let mut errors = Vec::with_capacity(16);
        let mut output = self.make_output();

        while let Some(ix) = blocks_to_analyze.pop_front() {
            errors.clear();

            let insert_at = match blocks_processed.binary_search(&ix) {
                Ok(_) => continue,
                Err(index) => index,
            };

            let output_nodes = self.visit_block(cx, &mut output, ix, &mut errors)?;

            if let [] = errors.as_slice() {
                blocks_processed.insert(insert_at, ix);

                for node in output_nodes {
                    match blocks_processed.binary_search(&node) {
                        Ok(_) => continue,
                        Err(_) => {
                            if let Err(index) = blocks_to_analyze.binary_search(&node) {
                                blocks_to_analyze.insert(index, node)
                            }
                        }
                    }
                }

                tracing::trace!("blocks to analyze: {:?}", blocks_to_analyze);
            } else {
                todo!("{:#?}", errors)
            }
        }

        Ok(output)
    }
}
