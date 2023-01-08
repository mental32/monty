use super::*;

#[derive(Debug, Clone)]
pub(crate) struct CgFuncData {
    pub name: ExternalName,
    pub sig: Signature,
    pub value_id: TaggedValueId<{ FUNCTION }>,
    pub cfg: montyc_core::codegen::CgBlockCFG<Constant>,
}

#[derive(Debug, Default)]
pub(crate) struct CgData {
    /// A mapping of function value indecies to `Func`.
    funcs: MapT<ValueId, CgFuncData>,

    /// functions that are defined, arranged from left-to-right as they are declared.
    ///
    /// The docs for cranelifts `ExternalName` lie!
    /// The inner values are not "arbitrary" and do have special meaning when using `cranelift_object` wrt FuncIds.
    ///
    /// We need to keep the order of functions when we include them so that the declare, define, and importing logic works correctly.
    /// At the moment we can simply store a tuple of `(name, linkage, signature)`
    ///
    _funcs_ordered_by_definition: Vec<(Box<str>, Linkage, Signature, Option<ValueId>)>,

    /// Data storage. TODO
    _data: (),
}

impl CgData {
    pub(crate) fn funcs_ordered(
        &self,
    ) -> impl Iterator<Item = &(Box<str>, Linkage, Signature, Option<ValueId>)> {
        self._funcs_ordered_by_definition.iter()
    }

    pub(crate) fn iter_func_data(&self) -> impl Iterator<Item = (&ValueId, &CgFuncData)> {
        self.funcs.iter()
    }

    pub fn decl_foreign_function(
        &mut self,
        name: &str,
        signature: Signature,
        value_id: Option<ValueId>,
    ) -> ExternalName {
        let stringy_name = name.to_string().into_boxed_str();
        let name = ExternalName::User {
            namespace: 0,
            index: self._funcs_ordered_by_definition.len() as u32,
        };

        self._funcs_ordered_by_definition.push((
            stringy_name,
            Linkage::Import,
            signature,
            value_id,
        ));

        name
    }

    pub fn insert_function(
        &mut self,
        value_id: TaggedValueId<{ FUNCTION }>,
        cfg: montyc_core::codegen::CgBlockCFG<Constant>,
        signature: Signature,
    ) -> ExternalName {
        let name = ExternalName::User {
            namespace: 0,
            index: self._funcs_ordered_by_definition.len() as u32,
        };

        let stringy_name = format!("value_{}", value_id.0 .0);

        self._funcs_ordered_by_definition.push((
            stringy_name.into(),
            Linkage::Local,
            signature.clone(),
            Some(value_id.0),
        ));

        let func = CgFuncData {
            value_id,
            cfg,
            name: name.clone(),
            sig: signature,
        };

        self.funcs.insert(value_id.0, func);

        name
    }
}
