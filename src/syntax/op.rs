use super::types::BaseType;
use std::fmt::Display;

/*
IDEA(stefano): introduce "unchecked" operators
skip safety checks, maybe using the '?' or the '!' suffix, i.e:
- <<?, <<<?, >>>?, or <<!, >>!, <<<!, >>>! -> skip the check for a positive 6bit shift amount
- **? -> skip the check for a neagtive index
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Len, // temporary way of getting the length of strings and arrays
    Equals,
    Not,

    Pow,
    WrappingPow,
    SaturatingPow,
    PowEquals,
    WrappingPowEquals,
    SaturatingPowEquals,

    Times,
    WrappingTimes,
    SaturatingTimes,
    TimesEquals,
    WrappingTimesEquals,
    SaturatingTimesEquals,

    Divide,
    WrappingDivide,
    SaturatingDivide,
    DivideEquals,
    WrappingDivideEquals,
    SaturatingDivideEquals,

    Remainder,
    RemainderEquals,

    Plus,
    WrappingPlus,
    SaturatingPlus,
    PlusEquals,
    WrappingPlusEquals,
    SaturatingPlusEquals,

    Minus,
    WrappingMinus,
    SaturatingMinus,
    MinusEquals,
    WrappingMinusEquals,
    SaturatingMinusEquals,

    LeftShift,
    WrappingLeftShift,
    SaturatingLeftShift,
    LeftShiftEquals,
    WrappingLeftShiftEquals,
    SaturatingLeftShiftEquals,

    RightShift,
    RightShiftEquals,

    LeftRotate,
    LeftRotateEquals,
    RightRotate,
    RightRotateEquals,

    BitAnd,
    BitAndEquals,

    BitXor,
    BitXorEquals,

    BitOr,
    BitOrEquals,

    And,
    AndEquals,

    Or,
    OrEquals,

    Compare,
    EqualsEquals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,
}

impl Display for Op {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Len       => write!(f, "len"),
            Self::Equals    => write!(f, "="),
            Self::Not       => write!(f, "!"),

            Self::Pow                   => write!(f,  "**"),
            Self::WrappingPow           => write!(f, r"**\"),
            Self::SaturatingPow         => write!(f,  "**|"),
            Self::PowEquals             => write!(f,  "**="),
            Self::WrappingPowEquals     => write!(f, r"**\="),
            Self::SaturatingPowEquals   => write!(f,  "**|="),

            Self::Times                 => write!(f,  "*"),
            Self::WrappingTimes         => write!(f, r"*\"),
            Self::SaturatingTimes       => write!(f,  "*|"),
            Self::TimesEquals           => write!(f,  "*="),
            Self::WrappingTimesEquals   => write!(f, r"*\="),
            Self::SaturatingTimesEquals => write!(f,  "*|="),

            Self::Divide                    => write!(f,  "/"),
            Self::WrappingDivide            => write!(f, r"/\"),
            Self::SaturatingDivide          => write!(f,  "/|"),
            Self::DivideEquals              => write!(f,  "/="),
            Self::WrappingDivideEquals      => write!(f, r"/\="),
            Self::SaturatingDivideEquals    => write!(f,  "/|="),

            Self::Remainder         => write!(f, "%"),
            Self::RemainderEquals   => write!(f, "%="),

            Self::Plus                  => write!(f,  "+"), // also unary safe absolute value
            Self::WrappingPlus          => write!(f, r"+\"), // also unary wrapping absolute value
            Self::SaturatingPlus        => write!(f,  "+|"), // also unary saturating absolute value
            Self::PlusEquals            => write!(f,  "+="),
            Self::WrappingPlusEquals    => write!(f, r"+\="),
            Self::SaturatingPlusEquals  => write!(f,  "+|="),

            Self::Minus                 => write!(f,  "-"), // also unary integer negation
            Self::WrappingMinus         => write!(f, r"-\"), // also unary wrapping integer negation
            Self::SaturatingMinus       => write!(f,  "-|"), // also unary saturating integer negation
            Self::MinusEquals           => write!(f,  "-="),
            Self::WrappingMinusEquals   => write!(f, r"-\="),
            Self::SaturatingMinusEquals => write!(f,  "-|="),

            Self::LeftShift                 => write!(f,  "<<"),
            Self::WrappingLeftShift         => write!(f, r"<<\"),
            Self::SaturatingLeftShift       => write!(f,  "<<|"),
            Self::LeftShiftEquals           => write!(f,  "<<="),
            Self::WrappingLeftShiftEquals   => write!(f, r"<<\="),
            Self::SaturatingLeftShiftEquals => write!(f,  "<<|="),

            Self::RightShift        => write!(f,  ">>"),
            Self::RightShiftEquals  => write!(f,  ">>="),

            Self::LeftRotate        => write!(f, "<<<"),
            Self::LeftRotateEquals  => write!(f, "<<<="),
            Self::RightRotate       => write!(f, ">>>"),
            Self::RightRotateEquals => write!(f, ">>>="),

            Self::BitAnd        => write!(f, "&"),
            Self::BitAndEquals  => write!(f, "&="),

            Self::BitOr         => write!(f, "|"),
            Self::BitOrEquals   => write!(f, "|="),

            Self::BitXor        => write!(f, "^"),
            Self::BitXorEquals  => write!(f, "^="),

            Self::And       => write!(f, "&&"),
            Self::AndEquals => write!(f, "&&="),

            Self::Or        => write!(f, "||"),
            Self::OrEquals  => write!(f, "||="),

            Self::Compare           => write!(f, "<=>"),
            Self::EqualsEquals      => write!(f, "=="),
            Self::NotEquals         => write!(f, "!="),
            Self::Greater           => write!(f, ">"),
            Self::GreaterOrEquals   => write!(f, ">="),
            Self::Less              => write!(f, "<"),
            Self::LessOrEquals      => write!(f, "<="),
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnaryOp {
    Len,
    Not,

    Plus,
    WrappingPlus,
    SaturatingPlus,

    Minus,
    WrappingMinus,
    SaturatingMinus,
}

impl Display for UnaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Len               => write!(f, "len"),
            Self::Not               => write!(f, "!"),

            Self::Plus              => write!(f,  "+"),
            Self::WrappingPlus      => write!(f, r"+\"),
            Self::SaturatingPlus    => write!(f,  "+|"),

            Self::Minus             => write!(f,  "-"),
            Self::WrappingMinus     => write!(f, r"-\"),
            Self::SaturatingMinus   => write!(f,  "-|"),
        };
    }
}

impl UnaryOp {
    #[allow(clippy::unused_self)]
    pub(crate) const fn typ(self) -> BaseType {
        return BaseType::Int;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BooleanUnaryOp {
    Not,
}

impl Display for BooleanUnaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Not => write!(f, "!"),
        };
    }
}

impl BooleanUnaryOp {
    #[allow(clippy::unused_self)]
    pub(crate) const fn typ(self) -> BaseType {
        return BaseType::Bool;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BinaryOp {
    Pow,
    WrappingPow,
    SaturatingPow,

    Times,
    WrappingTimes,
    SaturatingTimes,

    Divide,
    WrappingDivide,
    SaturatingDivide,

    Remainder,

    Plus,
    WrappingPlus,
    SaturatingPlus,

    Minus,
    WrappingMinus,
    SaturatingMinus,

    LeftShift,
    WrappingLeftShift,
    SaturatingLeftShift,

    RightShift,

    LeftRotate,
    RightRotate,

    BitAnd,
    BitXor,
    BitOr,
}

impl Display for BinaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Pow           => write!(f,  "**"),
            Self::WrappingPow   => write!(f, r"**\"),
            Self::SaturatingPow => write!(f,  "**|"),

            Self::Times             => write!(f,  "*"),
            Self::WrappingTimes     => write!(f, r"*\"),
            Self::SaturatingTimes   => write!(f,  "*|"),

            Self::Divide            => write!(f,  "/"),
            Self::WrappingDivide    => write!(f, r"/\"),
            Self::SaturatingDivide  => write!(f,  "/|"),

            Self::Remainder => write!(f, "%"),

            Self::Plus              => write!(f,  "+"),
            Self::WrappingPlus      => write!(f, r"+\"),
            Self::SaturatingPlus    => write!(f,  "+|"),

            Self::Minus             => write!(f,  "-"),
            Self::WrappingMinus     => write!(f, r"-\"),
            Self::SaturatingMinus   => write!(f,  "-|"),

            Self::LeftShift             => write!(f,  "<<"),
            Self::WrappingLeftShift     => write!(f, r"<<\"),
            Self::SaturatingLeftShift   => write!(f,  "<<|"),

            Self::RightShift    => write!(f,  ">>"),
            Self::LeftRotate    => write!(f, "<<<"),
            Self::RightRotate   => write!(f, ">>>"),

            Self::BitAnd    => write!(f, "&"),
            Self::BitOr     => write!(f, "|"),
            Self::BitXor    => write!(f, "^"),
        };
    }
}

impl BinaryOp {
    #[allow(clippy::unused_self)]
    pub(crate) const fn typ(self) -> BaseType {
        return BaseType::Int;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BooleanBinaryOp {
    And,
    Or,
}

impl Display for BooleanBinaryOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::And   => write!(f, "&&"),
            Self::Or    => write!(f, "||"),
        };
    }
}

impl BooleanBinaryOp {
    #[allow(clippy::unused_self)]
    pub(crate) const fn typ(self) -> BaseType {
        return BaseType::Bool;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ComparisonOp {
    Compare,
    EqualsEquals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,
}

impl Display for ComparisonOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Compare           => write!(f, "<=>"),
            Self::EqualsEquals      => write!(f, "=="),
            Self::NotEquals         => write!(f, "!="),
            Self::Greater           => write!(f, ">"),
            Self::GreaterOrEquals   => write!(f, ">="),
            Self::Less              => write!(f, "<"),
            Self::LessOrEquals      => write!(f, "<="),
        }
    }
}

impl ComparisonOp {
    pub(crate) const fn typ(self) -> BaseType {
        return match self {
            Self::Compare => BaseType::Int,
            Self::EqualsEquals
            | Self::NotEquals
            | Self::Greater
            | Self::GreaterOrEquals
            | Self::Less
            | Self::LessOrEquals => BaseType::Bool,
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AssignmentOp {
    Equals,

    Pow,
    WrappingPow,
    SaturatingPow,

    Times,
    WrappingTimes,
    SaturatingTimes,

    Divide,
    WrappingDivide,
    SaturatingDivide,

    Remainder,

    Plus,
    WrappingPlus,
    SaturatingPlus,

    Minus,
    WrappingMinus,
    SaturatingMinus,

    LeftShift,
    WrappingLeftShift,
    SaturatingLeftShift,

    RightShift,

    LeftRotate,
    RightRotate,

    And,
    BitAnd,
    BitXor,
    Or,
    BitOr,
}

impl Display for AssignmentOp {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Equals => write!(f, "="),

            Self::Pow           => write!(f,  "**="),
            Self::WrappingPow   => write!(f, r"**\="),
            Self::SaturatingPow => write!(f,  "**|="),

            Self::Times             => write!(f,  "*="),
            Self::WrappingTimes     => write!(f, r"*\="),
            Self::SaturatingTimes   => write!(f,  "*|="),

            Self::Divide            => write!(f,  "/="),
            Self::WrappingDivide    => write!(f, r"/\="),
            Self::SaturatingDivide  => write!(f,  "/|="),

            Self::Remainder => write!(f, "%="),

            Self::Plus              => write!(f,  "+="),
            Self::WrappingPlus      => write!(f, r"+\="),
            Self::SaturatingPlus    => write!(f,  "+|="),

            Self::Minus             => write!(f,  "-="),
            Self::WrappingMinus     => write!(f, r"-\="),
            Self::SaturatingMinus   => write!(f,  "-|="),

            Self::And       => write!(f, "&&="),
            Self::BitAnd    => write!(f, "&="),
            Self::Or        => write!(f, "||="),
            Self::BitOr     => write!(f, "|="),
            Self::BitXor    => write!(f, "^="),

            Self::LeftShift             => write!(f,  "<<="),
            Self::WrappingLeftShift     => write!(f, r"<<\="),
            Self::SaturatingLeftShift   => write!(f,  "<<|="),

            Self::RightShift    => write!(f,  ">>="),
            Self::LeftRotate    => write!(f, "<<<="),
            Self::RightRotate   => write!(f, ">>>="),
        };
    }
}
