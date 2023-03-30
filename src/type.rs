use std::collections::HashMap;

use crate::ast::{ Node, BinaryOp, UnaryOp, vec_string };

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Literal Types    
    Integer,
    String,
    Float,
    Bool,
    
    Enum(
        String,              // Name 
        Vec<String>          // Constructurs
    ),
    
    FunctionType(
        Vec<Type>,           // Argument Types 
        Box<Type>            // Return Type
    ),

    Struct(
        String,              // Name 
        Vec<(String, Type)>  // Members TODO: Change members into hashmap
    ),
    
    GenericFunction(
        String,              // Name 
        Vec<String>,         // Type Variables
        Vec<(String, Type)>, // Typed-Arguments
        Box<Type>,           // Return Type
        Box<Node>            // Body of the function
    ),

    StructDef(
        usize                // Id of the Sturct 
    ),
    TypeVar(String),
    Type(Box<Type>),
    Unit, 
}

#[derive(Clone)]
struct StructDef(String, Option<Vec<String>>, Vec<(String, Type)>);

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Integer => write!(f, "int"),
            Type::String => write!(f, "str"),
            Type::Float => write!(f, "float"),
            Type::FunctionType(domain, codomain) => write!(f, "({} -> {})", vec_string(domain), codomain),
            // TODO: print type_vars // fix printing
            Type::GenericFunction(_name, _type_vars, domain, codomain, _body) => write!(f, "(G {:?} -> {})", domain, codomain),
            Type::TypeVar(name) => write!(f, "{name}'"),
            Type::Type(ty) => write!(f, "(type {})", ty),
            Type::Unit => write!(f, "()"),
            Type::Struct(name, members) => write!(f, "{name} {:?}", members),
            Type::StructDef(id) => write!(f, "s{id}"),
            Type::Enum(name, _syms) => write!(f, "{name}"),
            Type::Bool => write!(f, "bool"),
        }
    }
}


type TypeError = String;
type TypecheckResult = Result<Type, TypeError>;

impl UnaryOp {
    fn typecheck(&self, operand: Type) -> TypecheckResult {
        match self {
            UnaryOp::Plu => match operand {
                Type::Integer => Ok(Type::Integer),
                Type::Float => Ok(Type::Float),
                _ => Err(format!("Bad operand type for unary +: '{:?}'", operand))
            },
            UnaryOp::Neg => match operand {
                Type::Integer => Ok(Type::Integer),
                Type::Float => Ok(Type::Float),
                _ => Err(format!("Bad operand type for unary -: '{:?}'", operand))
            },
        }
    }
}

impl BinaryOp {
    fn typecheck(&self, right: Type, left: Type) -> TypecheckResult {
        match self {
            BinaryOp::Add => match (right.clone(), left.clone()) {
                (Type::Integer, Type::Integer) => Ok(Type::Integer),
                (Type::Float, Type::Float) => Ok(Type::Float),
                (Type::String, Type::String) => Ok(Type::String),
                _ => Err(format!("Bad argument types for +: '{:?}' '{:?}'", right, left))
            },
            BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => match (right.clone(), left.clone()) {
                (Type::Integer, Type::Integer) => Ok(Type::Integer),
                (Type::Float, Type::Float) => Ok(Type::Float),
                _ => Err(format!("Bad argument types for -: '{:?}' '{:?}'", right, left))
            },
            BinaryOp::Assign => 
                if right == left {
                    Ok(Type::Unit)
                } else {
                    Err(format!("Tried to assign '{}' to '{}'", right, left))
                }
        }
    }
}

#[derive(Default, Clone, Debug)]
struct Scope {
    env: HashMap<String, Type>,
    upper: Option<Box<Scope>>
}

impl Scope {
    fn resolve(&self, var: &String) -> Option<Type> {
        match self.env.get(var) {
            Some(value) => Some(value.clone()), 
            None => match &self.upper {
                Some(upper) => upper.resolve(var),
                None => None
            }
        }
    }

    // TODO: fix
    fn define(&mut self, var: String, typ: Type) -> Result<(), TypeError> {
        if self.env.contains_key(&var) {
            return Err(format!("Already defined variable '{var}'"));
        } 
        self.env.insert(var, typ);
        Ok(())
    }
}


pub struct Typechecker {
    globals: HashMap<String, Type>,
    scope: Scope,

    struct_defs: HashMap<usize, StructDef>,
    struct_id: usize,
}

impl Typechecker {
    pub fn new() -> Typechecker {
        let mut tc = Typechecker { 
            globals: HashMap::default(), 
            scope: Scope::default(), 
            struct_defs: HashMap::default(),
            struct_id: 0,
        };
        // TODO: Disallow the usage of type as values and variables 
        tc.globals.insert("sin".into(), Type::FunctionType(vec![Type::Integer], Box::new(Type::Integer)));
        tc.globals.insert("int".into(), Type::Type(Box::new(Type::Integer)));
        tc.globals.insert("float".into(), Type::Type(Box::new(Type::Float)));
        tc.globals.insert("str".into(), Type::Type(Box::new(Type::String)));
        tc.globals.insert("bool".into(), Type::Type(Box::new(Type::Bool)));
        tc
    }

    fn enter_scope(&mut self) {
        let upper = self.scope.clone();
        self.scope = Scope::default();
        self.scope.upper = Some(Box::new(upper));
    }

    fn exit_scope(&mut self) {
        if let Some(scope) = &self.scope.upper {
            self.scope = *scope.clone();
        }
    }

    fn get_struct_def(&mut self, id: &usize) -> StructDef {
        self.struct_defs.get(id).expect("Bad id for struct def").clone()
    }
    
    fn resolve_type(&mut self, node: Node) -> TypecheckResult {
        match self.typecheck(node)? {
            Type::Type(ty) => match *ty {
                Type::StructDef(id) => {
                    let StructDef(name, typ_args, members) = self.get_struct_def(&id);
                    if let Some(_) = typ_args {
                        Err(format!("Type '{name}' needs parameters"))
                    } else {
                        Ok(Type::Struct(name, members))
                    }
                }       
                _ => Ok(*ty)
            },
            Type::TypeVar(ty) => Ok(Type::TypeVar(ty)),
            _ => Err(format!("Bad Type"))
        }
    }

    // TODO: detect and report unreachable code
    fn typecheck_return(&mut self, node: Node, return_type: Type) -> Result<bool, String> {
        match node {
            Node::Return(expr) => {
                let ty = self.typecheck(*expr)?;
                if ty != return_type {
                    self.exit_scope();
                    return Err(format!("Function returns '{}' not '{}'", ty, return_type))
                }            
                Ok(true)
            },
            Node::If(expr, body, elseb) => {
                let ty = self.typecheck(*expr)?;
                let Type::Bool = ty else {
                    return Err(format!("Expected 'Bool' but got '{ty}' at if"))
                };
                let a = self.typecheck_return(*body, return_type.clone())?;
                let b = self.typecheck_return(*elseb, return_type)?;
                
                Ok(a && b)
            }
            Node::Block(states) => {    
                for state in states {
                    if self.typecheck_return(state, return_type.clone())? {
                        return Ok(true)
                    }
                }
                Ok(false)
            }
            _ => {
                self.typecheck(node)?;
                Ok(false)
            },
        }
    }

    fn sub_generic(&self, typ_table: &HashMap<String, Type>, typ: Type) -> Result<Type, TypeError> {
        match typ {
            Type::TypeVar(var) => match typ_table.get(&var) {
                Some(ty) => Ok(ty.clone()),
                None => unreachable!(),
            },
            Type::FunctionType(args, ret) => {
                let nret = Box::new(self.sub_generic(typ_table, *ret)?);
                let mut nargs = vec![];
                for arg in args {
                    nargs.push(self.sub_generic(typ_table, arg)?)
                }
                Ok(Type::FunctionType(nargs, nret))
            },
            Type::Struct(name, members) => {
                let mut nmembers = vec![]; 
                for (name, typ) in members {
                    nmembers.push((name, self.sub_generic(typ_table, typ)?))
                }
                Ok(Type::Struct(name, nmembers))
            },
            _ => Ok(typ)    
        }
    }

    pub fn typecheck(&mut self, node: Node) -> TypecheckResult {
        match node {
            Node::Program(states) => {
                for state in states {
                    self.typecheck(state)?;
                }
                Ok(Type::Unit)
            }
            Node::Block(states) => {
                self.enter_scope();
                // TODO: fix while returning exiting scope
                for state in states {
                    if let Err(err) = self.typecheck(state) {
                        self.exit_scope();
                        return Err(err)
                    }
                }
                self.exit_scope();
                Ok(Type::Unit)
            },
            Node::Binary(left, op, right) => op.typecheck(self.typecheck(*right)?, self.typecheck(*left)?),
            Node::Unary(op, operand) => op.typecheck(self.typecheck(*operand)?),
            Node::Uinteger(_) => Ok(Type::Integer),
            Node::Float(_) => Ok(Type::Float),
            Node::String(_) => Ok(Type::String),
            Node::Symbol(sym) => match self.scope.resolve(&sym) {
                Some(typ) => Ok(typ.clone()),
                None => match self.globals.get(&sym) {
                    Some(typ) => Ok(typ.clone()),
                    None => Err(format!("No variable named '{}'", sym)) 
                }
            },
            Node::True | Node::False => Ok(Type::Bool),
            Node::Fun(name, type_vars, args, ret, body) => {

                self.enter_scope();
                if let Some(ty_vars) = type_vars.clone() {
                    for var in ty_vars.clone() {
                        self.scope.define(var.clone(), Type::TypeVar(var))?;
                    }
                    let return_type = self.resolve_type(*ret)?;
                    
                    let mut types = vec![];
                    for arg in args {
                        if let Node::TypedSymbol(sym, typ) = arg {
                            types.push((sym, self.resolve_type(*typ)?));
                        }
                    }
                    
                    self.exit_scope();
                    let ftype = Type::GenericFunction(name.clone(), ty_vars.clone(), types.clone(), Box::new(return_type.clone()), body.clone());
                    self.scope.define(name, ftype)?;
                    return Ok(Type::Unit);
                }

                let mut types = vec![];
                for arg in args {
                    if let Node::TypedSymbol(sym, typ) = arg {
                        let t = self.resolve_type(*typ)?;
                        types.push(t.clone());
                        self.scope.define(sym, t)?;
                    }
                }
                let return_type = self.resolve_type(*ret)?;
                self.scope.define(
                    name.clone(), 
                    Type::FunctionType(types.clone(), Box::new(return_type.clone()))
                )?;
                
                let a = self.typecheck_return(*body, return_type.clone())?;
                self.exit_scope();
                
                if !a && return_type != Type::Unit {
                    return Err(format!("Function doesn't always return"))
                }
                
                let ftype = Type::FunctionType(types.clone(), Box::new(return_type));
                self.scope.define(name, ftype)?;
                Ok(Type::Unit)
            },
            Node::Call(fexpr, args) => {
                let ftype = self.typecheck(*fexpr)?;
                match ftype.clone() {
                    Type::FunctionType(types, ret) => {
                        if types.len() != args.len() {
                            return Err(format!("Function takes '{}' args, given '{}'", types.len(), args.len()))
                        }
                        for i in 0..args.len() {
                            let ty = self.typecheck(args[i].clone())?;
                            if ty != types[i] {
                                return Err(format!("Expected '{}' got '{}' argument '{i}'", types[i], ty))
                            }
                        }
                        Ok(*ret)
                    },
                    Type::Type(typ) => {
                        // TODO: Refactor
                        match *typ {
                            Type::StructDef(id) => {
                                let StructDef(name, _type_vars, members) = self.get_struct_def(&id);
                                
                                if members.len() != args.len() {
                                    return Err(format!("Struct has '{}' members, given '{}'", members.len(), args.len()))
                                }
        
                                for i in 0..args.len() {
                                    let ty = self.typecheck(args[i].clone())?;
                                    if ty != members[i].1 {
                                        return Err(format!("Expected '{}' got '{}' argument '{i}'", members[i].1, ty))
                                    }
                                }
                                Ok(Type::Struct(name, members))
                            },
                            Type::Struct(name, members) => {
                                if members.len() != args.len() {
                                    return Err(format!("Struct has '{}' members, given '{}'", members.len(), args.len()))
                                }

                                for i in 0..args.len() {
                                    let ty = self.typecheck(args[i].clone())?;
                                    if ty != members[i].1 {
                                        return Err(format!("Expected '{}' got '{}' argument '{i}'", members[i].1, ty))
                                    }
                                }
                                
                                Ok(Type::Struct(name, members))
                            }
                            _ => Err(format!("Not callable type '{}'", ftype))
                        }
                    },

                    _ => Err(format!("Not callable type '{}'", ftype))
                }
            },
            Node::TypeCall(expr, typ_args) => {
                let typee = self.typecheck(*expr)?;
                match typee.clone() {
                    Type::GenericFunction(name, type_vars, types, ret, body) => {
                        let mut type_table = HashMap::new();
                        if type_vars.len() != typ_args.len() {
                            return Err(format!("Function takes '{}' types, given '{}'", type_vars.len(), typ_args.len()))
                        }

                        for i in 0..typ_args.len() {
                            type_table.insert(type_vars[i].clone(), self.resolve_type(typ_args[i].clone())?);
                        }
                        
                        let mut new_args = vec![];
                        for (name, typ) in types {
                            new_args.push((name, self.sub_generic(&type_table, typ.clone())?));
                        } 
                        let new_return = Box::new(self.sub_generic(&type_table, *ret)?);
                        
                        self.enter_scope();
                        let mut types = vec![];
                        for (name, typ) in new_args {
                            types.push(typ.clone());
                            self.scope.define(name, typ)?;
                        }
                        self.scope.define(
                            name.clone(), Type::FunctionType(types.clone(), new_return.clone())
                        )?;
                        let a = self.typecheck_return(*body, *new_return.clone())?;
                        
                        self.exit_scope();
                        if !a && *new_return != Type::Unit {
                            return Err(format!("Function doesn't always return"))
                        }
                        Ok(Type::FunctionType(types, new_return))
                    }
                    Type::Type(typ) => {
                        // TODO: Refactor
                        let Type::StructDef(id) = *typ else {
                            return Err(format!("Not callable type '{}'", typee))
                        };
                        let mut type_table = HashMap::new();
                        let StructDef(name, type_vars, members) = self.get_struct_def(&id);
                        
                        if let Some(vars) = type_vars {
                            if vars.len() != typ_args.len() {
                                return Err(format!("Struct takes '{}' types, given '{}'", vars.len(), typ_args.len()))
                            }
                            for i in 0..typ_args.len() {
                                type_table.insert(vars[i].clone(), self.resolve_type(typ_args[i].clone())?);
                            }    
                        } else {
                            if typ_args.len() > 0 {
                                return Err(format!("Unneccesary Type vars"))
                            }
                        }
                        let mut new_members = vec![];
                        for (name, typ) in members {
                            new_members.push((name, self.sub_generic(&type_table, typ.clone())?));
                        }
                        Ok(Type::Type(Box::new(Type::Struct(name, new_members))))
                    }
                    _ => Err(format!("Not generic type '{}'", typee))
                }
            }
            Node::Projection(expr, member) => {
                let ty = self.typecheck(*expr)?;
                match ty.clone() {
                    Type::Struct(name, members) => {
                        // Ideally we want hashmap, O(1) , not linear search
                        for (sym, typ) in members {
                            if sym == member {
                                return Ok(typ)
                            }
                        }
                        Err(format!("Struct '{name}' has no field called '{member}'"))
                    },
                    Type::Type(typ) => {
                        let Type::Enum(name, syms) = *typ.clone() else {
                            return Err(format!("Not callable type '{ty}'"))
                        };

                        if syms.contains(&member) {
                            return Ok(*typ);
                        }
                        Err(format!("Enum '{name}' has no variant called '{member}'"))
                    }
                    _ => return Err(format!("Unprojectable type '{ty}'"))
                }
            },
            Node::FuncType(domain, codomain) => {
                // TODO: refactor
                let mut types = vec![];
                for ty in domain {
                    types.push(self.resolve_type(ty)?);
                }
                let ret = Box::new(self.resolve_type(*codomain)?);

                Ok(Type::Type(Box::new(Type::FunctionType(types, ret))))
            },
            Node::Let(name, typ, expr) => {
                let expr_typ = self.typecheck(*expr)?;
                match *typ.clone() {
                    Node::NoType => self.scope.define(name, expr_typ)?,
                    _ => {
                        let ty = self.resolve_type(*typ)?;
                        if expr_typ == ty {
                            self.scope.define(name, expr_typ)?;
                        } else {
                            return Err(format!("Expression has type '{}', not '{}'", expr_typ, ty))                        
                        }
                    }  
                }
                Ok(Type::Unit)
            },
            Node::Struct(name, typ_params, members) => {

                self.enter_scope();
                
                let id = self.struct_id;
                self.struct_id += 1;

                let typ_vars = if let Some(vars) = typ_params {
                    for var in vars.clone() {
                        self.scope.define(var.clone(), Type::TypeVar(var))?;
                    }
                    Some(vars)
                } else {
                    None
                };
                
                self.struct_defs.insert(id, StructDef(name.clone(), typ_vars, vec![]));
                self.scope.define(name.clone(), Type::Type(Box::new(Type::StructDef(id))))?;
                
                let mut tuples = vec![];
                for member in members {
                    let Node::TypedSymbol(name, typ) = member else {
                        unreachable!()
                    }; 
                    tuples.push((name, self.resolve_type(*typ)?));
                }
                let st = self.struct_defs.get_mut(&id).unwrap();
                st.2 = tuples; 
                self.exit_scope();
                // TODO: Fix this
                self.scope.define(name, Type::Type(Box::new(Type::StructDef(id))))?;
                Ok(Type::Unit)
            },
            Node::Enum(name, syms) => {
                self.scope.define(name.clone(), Type::Type(Box::new(Type::Enum(name, syms))))?;
                Ok(Type::Unit)
            },
            Node::Return(_) => return Err(format!("Return outside of function")),
            Node::If(expr, body, elseb) => {
                let ty = self.typecheck(*expr)?;
                let Type::Bool = ty else {
                    return Err(format!("Expected 'Bool' but got '{ty}' at if"))
                };
                self.typecheck(*body)?;
                self.typecheck(*elseb)?;
                Ok(Type::Unit)
            },
            Node::GenericType(typ, typ_args) => {
                match self.scope.resolve(&typ) {
                    Some(ty) => match ty {
                        Type::Type(typ) => {
                            let Type::StructDef(id) = *typ else {
                                return Err(format!("Unnecessary type args for '{typ}'"))
                            };
                            let StructDef(name, typ_vars, members) = self.get_struct_def(&id);
                            let mut typ_table = HashMap::new();
                            if let Some(vars) = typ_vars {
                                if vars.len() != typ_args.len() {
                                    return Err(format!("Struct takes '{}' types, given '{}'", vars.len(), typ_args.len()))
                                }
                                for i in 0..typ_args.len() {
                                    typ_table.insert(vars[i].clone(), self.resolve_type(typ_args[i].clone())?);
                                }    
                            } else {
                                if typ_args.len() > 0 {
                                    return Err(format!("Unneccesary Type vars"))
                                }
                            }
                            let mut new_members = vec![];
                            for (name, typ) in members {
                                new_members.push((name, self.sub_generic(&typ_table, typ)?));
                            } 
                            Ok(Type::Type(Box::new(Type::Struct(name, new_members))))
                        }
                        _ => Err(format!("Unnecessary type args for '{ty}'"))
                    },  
                    None => Err(format!("Undefined variable {typ}"))
                }                
            },
            Node::Typeof(expr) => {
                let ty = self.typecheck(*expr)?;
                println!("{ty}");
                Ok(Type::Type(Box::new(ty)))
            }
            Node::Empty => Ok(Type::Unit), // ??
            Node::NoType => Ok(Type::Type(Box::new(Type::Unit))),
            Node::NoExpr => Ok(Type::Unit),
            Node::TypedSymbol(..) => unreachable!(),
        }
    }

}
