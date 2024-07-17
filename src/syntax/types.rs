use std::fmt::Display;

use super::tokenizer::{ascii, int, uint};

pub(crate) trait TypeOf {
    fn typ(&self) -> Type;
}

pub(crate) trait SizeOf {
    fn size(&self) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BaseType {
    Int,
    Ascii,
    Bool,
    Str,
}

impl Display for BaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Int => write!(f, "int"),
            Self::Ascii => write!(f, "ascii"),
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "str"),
        };
    }
}

impl SizeOf for BaseType {
    fn size(&self) -> usize {
        return match self {
            Self::Int => std::mem::size_of::<int>(),
            Self::Ascii => std::mem::size_of::<ascii>(),
            Self::Bool => std::mem::size_of::<bool>(),
            Self::Str => std::mem::size_of::<*const ascii>() + std::mem::size_of::<uint>(),
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Base(BaseType),
    // TODO(breaking)(stefano): enforce a max length
    /*
    TODO(breaking)(stefano): enforce a min length of 2:
    - arrays of 0 elements are meaningless, they don't even occupy any memory
    - arrays of 1 element are literaly just that element with extra steps to get to it
    */
    Array { typ: BaseType, len: uint },
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Base(typ) => write!(f, "{typ}"),
            Self::Array { typ, len } => write!(f, "{typ}[{len}]"),
        };
    }
}

impl SizeOf for Type {
    fn size(&self) -> usize {
        return match self {
            Self::Base(typ) => typ.size(),
            Self::Array { typ, len } => typ.size() * len,
        };
    }
}
