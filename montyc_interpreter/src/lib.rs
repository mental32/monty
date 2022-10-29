#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ObjectId;

#[derive(Debug)]
pub struct Object {
    type_id: ObjectId,
    dict: ahash::AHashMap<ObjectId, ObjectId>,
}

impl Object {
    pub fn type_eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
    }
}
