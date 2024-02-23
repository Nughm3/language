use id_arena::Arena;

use super::*;

#[derive(Debug, Default)]
pub struct Context {
    vars: Arena<TypeInfo>,
}

impl Context {
    pub fn new() -> Self {
        Context::default()
    }

    pub fn insert(&mut self, ty: TypeInfo) -> TypeId {
        self.vars.alloc(ty)
    }

    pub fn unify(&mut self, a: TypeId, b: TypeId) -> TypeResult<()> {
        use TypeInfo::*;

        match (self.vars[a].clone(), self.vars[b].clone()) {
            (Eq(a), _) => self.unify(a, b)?,
            (_, Eq(b)) => self.unify(a, b)?,

            (Unknown, _) => self.vars[a] = Eq(b),
            (_, Unknown) => self.vars[b] = Eq(a),

            (Function(pa, ra), Function(pb, rb)) if pa.len() == pb.len() => {
                for (a, b) in std::iter::zip(pa, pb) {
                    self.unify(a, b)?;
                }

                self.unify(ra, rb)?;
            }

            (a, b) => {
                if a != b {
                    return Err(TypeError::Mismatch(a, b));
                }
            }
        }

        Ok(())
    }

    pub fn get(&self, id: TypeId) -> &TypeInfo {
        &self.vars[id]
    }

    pub fn reconstruct(&self, id: TypeId) -> TypeResult<Type> {
        use TypeInfo::*;

        Ok(match &self.vars[id] {
            Unknown => return Err(TypeError::CannotInfer),
            Eq(id) => self.reconstruct(*id)?,
            Int => Type::Int,
            Bool => Type::Bool,
            Void => Type::Void,
            Function(params, return_type) => Type::Function(
                params
                    .iter()
                    .map(|p| self.reconstruct(*p))
                    .collect::<TypeResult<Vec<Type>>>()?,
                Box::new(self.reconstruct(*return_type)?),
            ),
        })
    }

    pub fn finish(self) -> TypeResult<Vec<Type>> {
        self.vars
            .iter()
            .map(|(id, _)| self.reconstruct(id))
            .collect()
    }
}
