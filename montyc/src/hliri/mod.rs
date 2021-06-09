use crate::prelude::GlobalContext;

pub struct Runtime;

impl Runtime {

    pub fn exec(
        &mut self,
        gcx: &mut GlobalContext,
        value: &mut montyc_hlir::ModuleObject,
    ) {

    }
}
