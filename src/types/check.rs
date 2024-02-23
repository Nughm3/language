use ahash::{HashMap, HashMapExt};

use super::*;
use crate::{
    ast::{InfixOp, PrefixOp},
    hir,
};

#[derive(Debug)]
pub struct TypeTable {
    types: Vec<Type>,
    functions: HashMap<hir::FunctionId, TypeId>,
    bindings: HashMap<hir::BindingId, TypeId>,
    exprs: HashMap<hir::ExprId, TypeId>,
}

impl TypeTable {
    pub fn typecheck(hir: &hir::Hir) -> TypeResult<Self> {
        let mut checker = TypeChecker {
            hir,
            ctx: context::Context::new(),
            current_function: None,
            function_stack: Vec::new(),
            functions: HashMap::new(),
            bindings: HashMap::new(),
            exprs: HashMap::new(),
        };

        for (module_id, _) in hir.modules.iter() {
            checker.module(module_id)?;
        }

        Ok(TypeTable {
            types: checker.ctx.finish()?,
            functions: checker.functions,
            bindings: checker.bindings,
            exprs: checker.exprs,
        })
    }

    pub fn function(&self, id: hir::FunctionId) -> Option<&Type> {
        self.functions.get(&id).map(|idx| &self.types[idx.index()])
    }

    pub fn binding(&self, id: hir::BindingId) -> Option<&Type> {
        self.bindings.get(&id).map(|idx| &self.types[idx.index()])
    }

    pub fn expr(&self, id: hir::ExprId) -> Option<&Type> {
        self.exprs.get(&id).map(|idx| &self.types[idx.index()])
    }
}

#[derive(Debug)]
struct TypeChecker<'a> {
    hir: &'a hir::Hir,
    ctx: context::Context,
    current_function: Option<(Vec<TypeId>, TypeId)>,
    function_stack: Vec<hir::FunctionId>,
    functions: HashMap<hir::FunctionId, TypeId>,
    bindings: HashMap<hir::BindingId, TypeId>,
    exprs: HashMap<hir::ExprId, TypeId>,
}

impl<'a> TypeChecker<'a> {
    fn module(&mut self, id: hir::ModuleId) -> TypeResult<()> {
        self.hir[id].functions.iter().copied().try_for_each(|id| {
            let function_type = self.function(id)?;
            self.functions.insert(id, function_type);
            Ok(())
        })
    }

    fn function(&mut self, id: hir::FunctionId) -> TypeResult<TypeId> {
        if let Some(ty) = self.functions.get(&id) {
            return Ok(*ty);
        }

        let function = &self.hir[id];

        let hir::Type::Function {
            params,
            return_type,
        } = &function.signature
        else {
            unreachable!("function signature must be function type");
        };

        let params: Vec<TypeId> = params.iter().map(|ty| self.lower_type(ty)).collect();
        let return_type = self.lower_type(return_type);

        let prev_signature = self.current_function.replace((params.clone(), return_type));
        self.function_stack.push(id);

        let block_type = self.block(function.body)?;
        self.ctx.unify(block_type, return_type)?;

        self.current_function = prev_signature;
        self.function_stack.pop();
        Ok(self.ctx.insert(TypeInfo::Function(params, return_type)))
    }

    fn block(&mut self, id: hir::BlockId) -> TypeResult<TypeId> {
        let hir::Block {
            functions,
            stmts,
            tail,
        } = &self.hir[id];

        stmts.iter().try_for_each(|id| self.stmt(*id))?;
        functions.iter().try_for_each(|id| {
            let function_type = self.function(*id)?;
            self.functions.insert(*id, function_type);
            Ok(())
        })?;

        if let Some(tail) = tail {
            self.expr(*tail)
        } else {
            Ok(self.ctx.insert(TypeInfo::Void))
        }
    }

    fn stmt(&mut self, id: hir::StmtId) -> TypeResult<()> {
        match &self.hir[id] {
            hir::Stmt::Expr(expr) => {
                self.expr(*expr)?;
            }
            hir::Stmt::Let(binding) => {
                let hir::Binding { ty, value, .. } = &self.hir[*binding];

                let value_type = self.expr(*value)?;

                if let Some(annotation) = ty {
                    let annotation = self.lower_type(annotation);
                    self.ctx.unify(value_type, annotation)?;
                }

                self.bindings.insert(*binding, value_type);
            }
            hir::Stmt::Return(Some(return_value)) => {
                let return_type = self.expr(*return_value)?;
                let (_, expected) = self
                    .current_function
                    .as_ref()
                    .expect("return outside of function");
                self.ctx.unify(return_type, *expected)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn expr(&mut self, id: hir::ExprId) -> TypeResult<TypeId> {
        let ty = match &self.hir[id] {
            hir::Expr::Block(block) => self.block(*block)?,
            hir::Expr::Decision {
                conditions,
                branches,
                default,
            } => {
                let bool_type = self.ctx.insert(TypeInfo::Bool);
                for condition in conditions.iter().copied() {
                    let condition_type = self.expr(condition)?;
                    self.ctx.unify(condition_type, bool_type)?;
                }

                branches.iter().chain(default).try_fold(
                    self.ctx.insert(TypeInfo::Unknown),
                    |a, b| {
                        let b = self.block(*b)?;
                        self.ctx.unify(a, b)?;
                        Ok(b)
                    },
                )?
            }
            hir::Expr::Loop(l) => {
                self.block(*l)?;
                self.ctx.insert(TypeInfo::Void)
            }
            hir::Expr::Prefix { op, expr } => {
                let expr = self.expr(*expr)?;

                let bool_type = self.ctx.insert(TypeInfo::Bool);
                let int_type = self.ctx.insert(TypeInfo::Int);

                match op {
                    PrefixOp::Not => self.ctx.unify(expr, bool_type)?,
                    PrefixOp::Negate => self.ctx.unify(expr, int_type)?,
                }

                expr
            }
            hir::Expr::Infix { lhs, op, rhs } => {
                let ty = {
                    let lhs = self.expr(*lhs)?;
                    let rhs = self.expr(*rhs)?;
                    self.ctx.unify(lhs, rhs)?; // All infix operations require both sides to be the same type for now

                    lhs
                };

                let bool_type = self.ctx.insert(TypeInfo::Bool);
                let int_type = self.ctx.insert(TypeInfo::Int);

                match op {
                    InfixOp::Add | InfixOp::Sub | InfixOp::Mul | InfixOp::Div | InfixOp::Rem => {
                        self.ctx.unify(ty, int_type)?;
                        int_type
                    }
                    InfixOp::Lt | InfixOp::Le | InfixOp::Gt | InfixOp::Ge => {
                        self.ctx.unify(ty, int_type)?;
                        bool_type
                    }
                    InfixOp::LogicAnd | InfixOp::LogicOr => {
                        self.ctx.unify(ty, bool_type)?;
                        bool_type
                    }
                    InfixOp::Eq | InfixOp::Ne => bool_type,
                    InfixOp::Assign => ty,
                }
            }
            hir::Expr::Call { function, args } => {
                let expr_id = self.expr(*function)?;

                let return_type = if let TypeInfo::Function(_, r) = self.ctx.get(expr_id) {
                    *r
                } else {
                    return Err(TypeError::NotCallable);
                };

                let function = TypeInfo::Function(
                    args.iter()
                        .map(|expr| self.expr(*expr))
                        .collect::<TypeResult<_>>()?,
                    self.ctx.insert(TypeInfo::Unknown),
                );

                let call_id = self.ctx.insert(function);
                self.ctx.unify(expr_id, call_id)?;

                return_type
            }
            hir::Expr::Int(_) => self.ctx.insert(TypeInfo::Int),
            hir::Expr::Bool(_) => self.ctx.insert(TypeInfo::Bool),
            hir::Expr::Ref(r#ref) => match *r#ref {
                hir::Ref::Function(function) => match &self.current_function {
                    Some((params, return_type)) if self.function_stack.contains(&function) => self
                        .ctx
                        .insert(TypeInfo::Function(params.clone(), *return_type)),
                    _ => self.function(function)?,
                },
                hir::Ref::Local(binding) => self.bindings[&binding],
                hir::Ref::Param(idx) => {
                    let (params, _) = self.current_function.as_ref().expect("not in a function");
                    params[idx]
                }
                hir::Ref::Late(_) => unreachable!("unfinished late binding while type checking"),
                hir::Ref::Error => unreachable!("unresolved name while type checking"),
            },
        };

        self.exprs.insert(id, ty);
        Ok(ty)
    }

    fn lower_type(&mut self, ty: &hir::Type) -> TypeId {
        let ty = match ty {
            hir::Type::Int => TypeInfo::Int,
            hir::Type::Bool => TypeInfo::Bool,
            hir::Type::Void => TypeInfo::Void,
            hir::Type::Function {
                params,
                return_type,
            } => {
                let params = params.iter().map(|param| self.lower_type(param)).collect();
                let return_type = self.lower_type(return_type);
                TypeInfo::Function(params, return_type)
            }
        };

        self.ctx.insert(ty)
    }
}
