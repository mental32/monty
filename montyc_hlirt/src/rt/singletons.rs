use core::fmt;

use ahash::AHashMap;

use crate::ObjectId;

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) enum DynamicSingleton {
    Int(i64),
    Str(u64),
}

#[derive(Default, Clone)]
pub struct Singletons {
    // Modules
    pub builtins: ObjectId,
    pub monty: ObjectId,
    pub sys: ObjectId,

    // Classes
    pub function_class: ObjectId,
    pub module_class: ObjectId,
    pub string_class: ObjectId,
    pub bool_class: ObjectId,
    pub int_class: ObjectId,
    pub type_class: ObjectId,
    pub object_class: ObjectId,
    pub float_class: ObjectId,
    pub none_class: ObjectId,
    pub ellipsis_class: ObjectId,
    pub bytes_class: ObjectId,
    pub list_class: ObjectId,
    pub dict_class: ObjectId,

    // Constants
    pub none_v: ObjectId,
    pub false_v: ObjectId,
    pub true_v: ObjectId,
    pub ellipsis_v: ObjectId,

    // Dyanmic singletons
    //
    // (things like strings or integers that are lazilly created but kept around.)
    //
    pub(crate) dynamic: AHashMap<DynamicSingleton, ObjectId>,
}

impl fmt::Debug for Singletons {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Singletons")
            .field("builtins", &self.builtins)
            .field("monty", &self.monty)
            .field("sys", &self.sys)
            .field("function_class", &self.function_class)
            .field("module_class", &self.module_class)
            .field("string_class", &self.string_class)
            .field("bool_class", &self.bool_class)
            .field("int_class", &self.int_class)
            .field("type_class", &self.type_class)
            .field("object_class", &self.object_class)
            .field("float_class", &self.float_class)
            .field("none_class", &self.none_class)
            .field("ellipsis_class", &self.ellipsis_class)
            .field("bytes_class", &self.bytes_class)
            .field("list_class", &self.list_class)
            .field("dict_class", &self.dict_class)
            .field("none_v", &self.none_v)
            .field("false_v", &self.false_v)
            .field("true_v", &self.true_v)
            .field("ellipsis_v", &self.ellipsis_v)
            .finish()
    }
}
