use ahash::{HashMap, HashMapExt};
use Value::*;

use crate::{
    ast::{InfixOp, PrefixOp},
    hir::*,
};

pub struct Interpreter<'a> {
    hir: &'a Hir,
    variables: HashMap<BindingId, Value>,
    current_function_args: Vec<Value>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Function(FunctionId),
    Void,
}

enum ControlFlow {
    None,
    Break,
    Continue,
    Return(Value),
}

impl<'a> Interpreter<'a> {
    pub fn new(hir: &'a Hir) -> Self {
        Interpreter {
            hir,
            variables: HashMap::new(),
            current_function_args: Vec::new(),
        }
    }

    pub fn eval(&mut self) {
        let main = self
            .hir
            .functions
            .iter()
            .find_map(|(id, function)| ("main" == &*function.name).then_some(id))
            .expect("no main() function");
        self.function(main, Vec::new());
    }

    fn function(&mut self, id: FunctionId, args: Vec<Value>) -> Value {
        let old_params = std::mem::take(&mut self.current_function_args);
        self.current_function_args = args;
        let function = &self.hir[id];

        let name: &str = function.name.as_ref();
        match name {
            "print" if self.current_function_args.len() == 1 => {
                print!("{:?}", self.current_function_args[0]);
                Void
            }
            "println" if self.current_function_args.len() == 1 => {
                println!("{:?}", self.current_function_args[0]);
                Void
            }
            _ => {
                let return_value = self.block(function.body).0;
                self.current_function_args = old_params;
                return_value
            }
        }
    }

    fn block(&mut self, id: BlockId) -> (Value, ControlFlow) {
        let Block { stmts, tail, .. } = &self.hir[id];

        for stmt in stmts {
            match self.stmt(*stmt) {
                ControlFlow::None => {}
                cf @ (ControlFlow::Break | ControlFlow::Continue) => return (Void, cf),
                cf @ ControlFlow::Return(value) => return (value, cf),
            }
        }

        if let Some(tail) = tail {
            (self.expr(*tail), ControlFlow::None)
        } else {
            (Void, ControlFlow::None)
        }
    }

    fn stmt(&mut self, id: StmtId) -> ControlFlow {
        match &self.hir[id] {
            Stmt::Expr(expr) => {
                self.expr(*expr);
                ControlFlow::None
            }
            Stmt::Let(id) => {
                let Binding { value, .. } = &self.hir[*id];
                let value = self.expr(*value);
                self.variables.insert(*id, value);
                ControlFlow::None
            }
            Stmt::Break => ControlFlow::Break,
            Stmt::Continue => ControlFlow::Continue,
            Stmt::Return(value) => ControlFlow::Return(value.map(|v| self.expr(v)).unwrap_or(Void)),
        }
    }

    fn expr(&mut self, id: ExprId) -> Value {
        match &self.hir[id] {
            Expr::Block(block) => self.block(*block).0,
            Expr::Decision {
                conditions,
                branches,
                default,
            } => {
                for (c, b) in std::iter::zip(conditions, branches) {
                    if let Bool(true) = self.expr(*c) {
                        return self.block(*b).0;
                    }
                }

                if let Some(default) = default {
                    self.block(*default).0
                } else {
                    unreachable!("if missing else clause");
                }
            }
            Expr::Loop(block) => loop {
                match self.block(*block).1 {
                    ControlFlow::None | ControlFlow::Continue => {}
                    ControlFlow::Break => break Void,
                    ControlFlow::Return(value) => break value,
                }
            },
            Expr::Prefix { op, expr } => match (op, self.expr(*expr)) {
                (PrefixOp::Not, Bool(b)) => Bool(!b),
                (PrefixOp::Negate, Int(i)) => Int(-i),
                _ => panic!("invalid operation: {op:?} {expr:?}"),
            },
            Expr::Infix { lhs, op, rhs } => {
                if let InfixOp::Assign = op {
                    let Expr::Ref(Ref::Local(id)) = self.hir[*lhs] else {
                        panic!("not assignable");
                    };
                    let value = self.expr(*rhs);
                    self.variables.insert(id, value);
                    value
                } else {
                    let (lhs, rhs) = (self.expr(*lhs), self.expr(*rhs));
                    match (op, lhs, rhs) {
                        (InfixOp::Add, Int(a), Int(b)) => Int(a + b),
                        (InfixOp::Sub, Int(a), Int(b)) => Int(a - b),
                        (InfixOp::Mul, Int(a), Int(b)) => Int(a * b),
                        (InfixOp::Div, Int(a), Int(b)) => Int(a / b),
                        (InfixOp::Rem, Int(a), Int(b)) => Int(a % b),
                        (InfixOp::LogicAnd, Bool(a), Bool(b)) => Bool(a && b),
                        (InfixOp::LogicOr, Bool(a), Bool(b)) => Bool(a || b),
                        (InfixOp::Eq, a, b) => Bool(a == b),
                        (InfixOp::Ne, a, b) => Bool(a != b),
                        (InfixOp::Lt, Int(a), Int(b)) => Bool(a < b),
                        (InfixOp::Le, Int(a), Int(b)) => Bool(a <= b),
                        (InfixOp::Gt, Int(a), Int(b)) => Bool(a > b),
                        (InfixOp::Ge, Int(a), Int(b)) => Bool(a >= b),
                        _ => panic!("invalid operation: {lhs:?} {op:?} {rhs:?}"),
                    }
                }
            }
            Expr::Call { function, args } => {
                let Function(id) = self.expr(*function) else {
                    panic!("not callable");
                };
                let args = args.iter().map(|arg| self.expr(*arg)).collect();
                self.function(id, args)
            }
            Expr::Int(i) => Int(*i as i64),
            Expr::Bool(b) => Bool(*b),
            Expr::Ref(r#ref) => match r#ref {
                Ref::Function(id) => Function(*id),
                Ref::Local(id) => self.variables[id],
                Ref::Param(idx) => self.current_function_args[*idx],
                _ => unreachable!("resolution error"),
            },
        }
    }
}
