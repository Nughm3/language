use ariadne::{Report, ReportKind};

use super::{resolve::*, Node};
use crate::{ast, hir, span::Span};

#[derive(Debug, Default)]
pub struct Builder<'a> {
    hir: hir::Hir,
    resolver: Resolver,
    errors: Vec<Report<'a, Span>>,
}

#[derive(Debug, Default)]
struct Resolver {
    values: Env<hir::Ref>,
    late_bindings: Vec<hir::Ref>,
    late_exprs: Vec<hir::ExprId>,
}

impl Resolver {
    fn reset(&mut self) {
        self.values = Env::new();
        self.late_bindings.clear();
        self.late_exprs.clear();
    }

    fn late_bind(&mut self, name: ast::Ident) -> Result<hir::Ref, AlreadyBound> {
        let r#ref = hir::Ref::Late(self.late_bindings.len());
        self.values.bind(name, r#ref)?;
        self.late_bindings.push(r#ref);
        Ok(r#ref)
    }

    fn late_finish(&mut self, r#ref: hir::Ref, value: hir::Ref) {
        let hir::Ref::Late(idx) = r#ref else {
            unreachable!("tried to complete late binding more than once");
        };

        self.late_bindings[idx] = value;
    }
}

impl<'a> Builder<'a> {
    pub fn new() -> Self {
        Builder::default()
    }

    pub fn finish(self) -> Result<hir::Hir, Vec<Report<'a, Span>>> {
        if self.errors.is_empty() {
            Ok(self.hir)
        } else {
            Err(self.errors)
        }
    }

    pub fn lower(&mut self, file: ast::File) {
        self.resolver.reset();
        self.file(file);

        for expr in self.resolver.late_exprs.iter() {
            let hir::Expr::Ref(r#ref) = &mut self.hir[*expr] else {
                unreachable!("not reference expression");
            };

            let hir::Ref::Late(idx) = r#ref else {
                unreachable!("not late binding");
            };

            *r#ref = self.resolver.late_bindings[*idx];
        }
    }

    fn create_node<N: Node>(&mut self, node: N) -> N::Id {
        node.alloc(&mut self.hir)
    }

    fn file(&mut self, ast::File(functions): ast::File) {
        self.resolver.values.scope(Scope::new());

        let mut function_bindings = Vec::with_capacity(functions.len());
        for function in functions.iter() {
            match self.resolver.late_bind(function.name) {
                Ok(r#ref) => function_bindings.push(r#ref),
                Err(AlreadyBound) => self.errors.push(
                    Report::build(ReportKind::Error, (), 0) // TODO: span tracking
                        .with_message(format_args!(
                            "function '{}' has already been defined in this scope",
                            function.name
                        ))
                        .finish(),
                ),
            }
        }

        for (function, r#ref) in std::iter::zip(functions, function_bindings) {
            let function_id = self.function(function);
            self.resolver
                .late_finish(r#ref, hir::Ref::Function(function_id));
        }

        self.resolver.values.unscope();
    }

    fn function(
        &mut self,
        ast::Function {
            name,
            params,
            return_type,
            body,
        }: ast::Function,
    ) -> hir::FunctionId {
        self.resolver.values.scope(Scope::new().restrict());

        for (idx, (name, _)) in params.iter().enumerate() {
            if let Err(AlreadyBound) = self.resolver.values.bind(*name, hir::Ref::Param(idx)) {
                self.errors.push(
                    Report::build(ReportKind::Error, (), 0) // TODO: span tracking
                        .with_message(format_args!(
                            "parameter '{name}' has already been defined for this function"
                        ))
                        .finish(),
                )
            }
        }

        let function = hir::Function {
            name,
            signature: hir::Type::Function {
                params: params.into_iter().map(|(_, ty)| r#type(ty)).collect(),
                return_type: Box::new(return_type.map(r#type).unwrap_or(hir::Type::Void)),
            },
            body: self.block(body),
        };

        let id = self.create_node(function);
        self.resolver.values.unscope();
        id
    }

    fn block(
        &mut self,
        ast::Block {
            stmts,
            functions,
            tail,
        }: ast::Block,
    ) -> hir::BlockId {
        self.resolver.values.scope(Scope::new());

        let mut function_bindings = Vec::with_capacity(functions.len());
        for function in functions.iter() {
            match self.resolver.late_bind(function.name) {
                Ok(id) => function_bindings.push(id),
                Err(AlreadyBound) => self.errors.push(
                    Report::build(ReportKind::Error, (), 0) // TODO: span tracking
                        .with_message(format_args!(
                            "function '{}' has already been defined in this scope",
                            function.name
                        ))
                        .finish(),
                ),
            }
        }

        let block = hir::Block {
            functions: std::iter::zip(functions, function_bindings)
                .map(|(f, r#ref)| {
                    let function_id = self.function(f);
                    self.resolver
                        .late_finish(r#ref, hir::Ref::Function(function_id));
                    function_id
                })
                .collect(),
            stmts: stmts.into_iter().map(|stmt| self.stmt(stmt)).collect(),
            tail: tail.map(|expr| self.expr(*expr)),
        };

        self.resolver.values.unscope();
        self.create_node(block)
    }

    fn stmt(&mut self, stmt: ast::Stmt) -> hir::StmtId {
        let stmt = match stmt {
            ast::Stmt::Expr(expr) => hir::Stmt::Expr(self.expr(expr)),
            ast::Stmt::Let { name, ty, value } => {
                let binding = hir::Binding {
                    name,
                    ty: ty.map(r#type),
                    value: self.expr(value),
                };

                let id = self.create_node(binding);
                self.resolver.values.scope(Scope::subscope());
                self.resolver
                    .values
                    .bind(name, hir::Ref::Local(id))
                    .unwrap();
                hir::Stmt::Let(id)
            }
            ast::Stmt::Break => hir::Stmt::Break,
            ast::Stmt::Continue => hir::Stmt::Continue,
            ast::Stmt::Return(value) => hir::Stmt::Return(value.map(|value| self.expr(value))),
        };

        self.create_node(stmt)
    }

    fn expr(&mut self, expr: ast::Expr) -> hir::ExprId {
        let expr = self.expr_rec(expr);
        self.create_node(expr)
    }

    fn expr_rec(&mut self, expr: ast::Expr) -> hir::Expr {
        match expr {
            ast::Expr::Block(block) => hir::Expr::Block(self.block(block)),
            ast::Expr::If(r#if) => {
                let mut decision = hir::Expr::Decision {
                    conditions: Vec::with_capacity(1),
                    branches: Vec::with_capacity(1),
                    default: None,
                };

                self.r#if(r#if, &mut decision);
                decision
            }
            ast::Expr::Loop(body) => hir::Expr::Loop(self.block(body)),
            ast::Expr::While { condition, body } => {
                let body = self.block(body);

                if self.hir[body].tail.is_some() {
                    self.errors.push(
                        Report::build(ReportKind::Error, (), 0) // TODO: span tracking
                            .with_message("'while' loop body cannot return values")
                            .finish(),
                    )
                } else {
                    let branch = hir::Block {
                        stmts: vec![self.create_node(hir::Stmt::Break)],
                        ..Default::default()
                    };

                    let expr = hir::Expr::Decision {
                        conditions: vec![self.expr(*condition)],
                        branches: { vec![self.create_node(branch)] },
                        default: None,
                    };

                    self.hir[body].tail = Some(self.create_node(expr));
                }

                hir::Expr::Loop(body)
            }
            ast::Expr::Prefix { op, expr } => hir::Expr::Prefix {
                op,
                expr: self.expr(*expr),
            },
            ast::Expr::Infix { lhs, op, rhs } => hir::Expr::Infix {
                lhs: self.expr(*lhs),
                op,
                rhs: self.expr(*rhs),
            },
            ast::Expr::Paren(expr) => self.expr_rec(*expr),
            ast::Expr::Call { function, args } => hir::Expr::Call {
                function: self.expr(*function),
                args: args.into_iter().map(|arg| self.expr(arg)).collect(),
            },
            ast::Expr::Int(int) => hir::Expr::Int(int),
            ast::Expr::Bool(bool) => hir::Expr::Bool(bool),
            ast::Expr::Ident(name) => {
                let r#ref = self
                    .resolver
                    .values
                    .resolve(name)
                    .copied()
                    .unwrap_or_else(|| {
                        self.errors.push(
                                Report::build(ReportKind::Error, (), 0) // TODO: span tracking
                                    .with_message(format_args!(
                                        "could not find '{name}' in this scope"
                                    ))
                                    .finish(),
                            );

                        hir::Ref::Error
                    });

                if let hir::Ref::Late(_) = r#ref {
                    self.resolver.late_exprs.push(self.hir.exprs.next_id());
                }

                hir::Expr::Ref(r#ref)
            }
            ast::Expr::ParseError => {
                unreachable!("expression failed to parse but is being lowered into HIR")
            }
        }
    }

    fn r#if(
        &mut self,
        ast::ExprIf {
            condition,
            then,
            r#else,
        }: ast::ExprIf,
        decision: &mut hir::Expr,
    ) {
        let hir::Expr::Decision {
            conditions,
            branches,
            default,
        } = decision
        else {
            unreachable!()
        };

        conditions.push(self.expr(*condition));
        branches.push(self.block(then));
        if let Some(r#else) = r#else {
            match r#else {
                ast::ExprElse::If(r#if) => self.r#if(*r#if, decision),
                ast::ExprElse::Block(block) => *default = Some(self.block(block)),
            }
        }
    }
}

fn r#type(ty: ast::Type) -> hir::Type {
    match ty {
        ast::Type::Int => hir::Type::Int,
        ast::Type::Bool => hir::Type::Bool,
        ast::Type::Void => hir::Type::Void,
        ast::Type::Function {
            params,
            return_type,
        } => hir::Type::Function {
            params: params.into_iter().map(r#type).collect(),
            return_type: Box::new(return_type.map(|ty| r#type(*ty)).unwrap_or(hir::Type::Void)),
        },
        ast::Type::ParseError => unreachable!("type failed to parse but is being lowered into HIR"),
    }
}
