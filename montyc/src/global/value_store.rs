use montyc_core::utils::SSAMap;

pub type ValueId = usize;

/// A structure used to track and cache the results of `montyc_hlir::Value` typecheck/inference/whatever.
#[derive(Default, Debug)]
pub struct GlobalValueStore {
    values: SSAMap<ValueId, montyc_hlir::Value>,
}
