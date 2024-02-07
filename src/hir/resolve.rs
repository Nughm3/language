use ahash::{HashMap, HashMapExt};
use internment::Intern;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env<T> {
    scopes: Vec<Scope<T>>,
}

impl<T> Default for Env<T> {
    fn default() -> Self {
        Env { scopes: Vec::new() }
    }
}

#[derive(Debug)]
pub struct AlreadyBound;

pub trait Resolvable {
    fn restricted(&self) -> bool;
}

impl<T: Resolvable> Env<T> {
    pub fn new() -> Self {
        Env::default()
    }

    pub fn scope(&mut self, scope: Scope<T>) {
        self.scopes.push(scope);
    }

    pub fn unscope(&mut self) {
        assert!(!self.scopes.is_empty(), "not in any scope");

        while let Some(scope) = self.scopes.pop() {
            if !scope.subscope {
                break;
            }
        }
    }

    pub fn bind(&mut self, name: Intern<str>, value: T) -> Result<(), AlreadyBound> {
        if self
            .scopes
            .last_mut()
            .expect("not in any scope")
            .map
            .insert(name, value)
            .is_none()
        {
            Ok(())
        } else {
            Err(AlreadyBound)
        }
    }

    pub fn resolve(&mut self, name: Intern<str>) -> Option<&T> {
        let mut transparent = true;

        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.map.get(&name) {
                if transparent || !value.restricted() {
                    return Some(value);
                }
            }

            transparent &= scope.transparent;
        }

        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<T> {
    map: HashMap<Intern<str>, T>,
    subscope: bool,
    transparent: bool,
}

impl<T> Default for Scope<T> {
    fn default() -> Self {
        Scope {
            map: HashMap::new(),
            subscope: false,
            transparent: true,
        }
    }
}

impl<T> Scope<T> {
    pub fn new() -> Self {
        Scope::default()
    }

    pub fn subscope() -> Self {
        Scope {
            subscope: true,
            ..Default::default()
        }
    }

    pub fn restrict(self) -> Self {
        Scope {
            transparent: false,
            ..self
        }
    }
}
