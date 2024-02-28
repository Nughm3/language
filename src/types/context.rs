use ahash::{HashMap, RandomState};
use id_arena::{Arena, Id};

use super::{Type, TypeError, TypeResult};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Key(u32);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Tag {
    Unknown,
    Eq,
    Int,
    Bool,
    Void,
    Function,
}

pub type Var = Id<Key>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeInfo {
    Unknown,
    Eq(Var),
    Int,
    Bool,
    Void,
    Function(Vec<Var>, Var),
}

#[derive(Debug, Default, Clone)]
pub struct Context {
    hasher: RandomState,
    map: HashMap<u64, Key>,

    tags: Vec<Tag>,
    data: Vec<Option<u32>>,

    eq: Vec<Var>,

    function_params: Vec<Vec<Var>>,
    function_return_types: Vec<Var>,

    vars: Arena<Key>,
}

impl Context {
    pub fn new() -> Self {
        Context::default()
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.vars = Arena::new();
    }

    pub fn insert(&mut self, info: TypeInfo) -> Var {
        let key = self.intern(info);
        self.vars.alloc(key)
    }

    pub fn get(&self, var: Var) -> TypeInfo {
        self.resolve(self.vars[var]).expect("not interned")
    }

    pub fn unify(&mut self, a: Var, b: Var) -> TypeResult<()> {
        use TypeInfo::*;

        match (
            self.resolve(self.vars[a]).expect("not interned"),
            self.resolve(self.vars[b]).expect("not interned"),
        ) {
            (Eq(a), _) => self.unify(a, b)?,
            (_, Eq(b)) => self.unify(a, b)?,

            (Unknown, _) => {
                self.vars[a] = self.intern(TypeInfo::Eq(b));
                self.unify(a, b)?;
            }
            (_, Unknown) => {
                self.vars[b] = self.intern(TypeInfo::Eq(a));
                self.unify(a, b)?;
            }

            (left, right) => {
                let (a, b) = (self.vars[a], self.vars[b]);

                if a != b {
                    match (left, right) {
                        (Function(pa, ra), Function(pb, rb)) if pa.len() == pb.len() => {
                            for (a, b) in std::iter::zip(pa, pb) {
                                self.unify(a, b)?;
                            }

                            self.unify(ra, rb)?;
                        }
                        _ => return Err(TypeError::Mismatch(a, b)),
                    }
                }
            }
        }

        Ok(())
    }

    pub fn reconstruct(&self, var: Var) -> TypeResult<Type> {
        Ok(match self.resolve(self.vars[var]).expect("not interned") {
            TypeInfo::Unknown => return Err(TypeError::CannotInfer),
            TypeInfo::Eq(inner) => self.reconstruct(inner)?,
            TypeInfo::Int => Type::Int,
            TypeInfo::Bool => Type::Bool,
            TypeInfo::Void => Type::Void,
            TypeInfo::Function(params, return_type) => Type::Function(
                params
                    .into_iter()
                    .map(|param| self.reconstruct(param))
                    .collect::<TypeResult<_>>()?,
                Box::new(self.reconstruct(return_type)?),
            ),
        })
    }

    pub fn intern(&mut self, info: TypeInfo) -> Key {
        let hash = self.hasher.hash_one(&info);
        if let Some(key) = self.map.get(&hash) {
            return *key;
        }

        let key = Key(self.map.len() as u32);
        self.map.insert(hash, key);

        match info {
            TypeInfo::Unknown => {
                self.tags.push(Tag::Unknown);
                self.data.push(None);
            }
            TypeInfo::Eq(other) => {
                self.tags.push(Tag::Eq);
                self.data.push(Some(self.eq.len() as u32));
                self.eq.push(other);
            }
            TypeInfo::Int => {
                self.tags.push(Tag::Int);
                self.data.push(None);
            }
            TypeInfo::Bool => {
                self.tags.push(Tag::Bool);
                self.data.push(None);
            }
            TypeInfo::Void => {
                self.tags.push(Tag::Void);
                self.data.push(None);
            }
            TypeInfo::Function(params, return_type) => {
                self.tags.push(Tag::Function);
                self.data.push(Some(self.function_params.len() as u32));

                self.function_params.push(params.clone());
                self.function_return_types.push(return_type);

                debug_assert_eq!(self.function_params.len(), self.function_return_types.len());
            }
        }

        key
    }

    pub fn resolve(&self, Key(key_idx): Key) -> Option<TypeInfo> {
        let key_idx = key_idx as usize;

        Some(match self.tags.get(key_idx)? {
            Tag::Unknown => TypeInfo::Unknown,
            Tag::Eq => {
                let idx = self.data.get(key_idx).copied().flatten()? as usize;
                TypeInfo::Eq(*self.eq.get(idx)?)
            }
            Tag::Int => TypeInfo::Int,
            Tag::Bool => TypeInfo::Bool,
            Tag::Void => TypeInfo::Void,
            Tag::Function => {
                let idx = self.data.get(key_idx).copied().flatten()? as usize;
                let params = self.function_params.get(idx)?;
                let return_type = self.function_return_types.get(idx)?;
                TypeInfo::Function(params.clone(), *return_type)
            }
        })
    }
}
