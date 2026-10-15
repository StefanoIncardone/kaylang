use back_to_front::ascii;

pub(crate) trait SizeOf {
    #[must_use]
    fn size(&self) -> usize;
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[must_use] #[repr(u8)]
pub enum BaseType {
    I64,
    Ascii,
    Bool,
    Str,
}

impl BaseType {
    pub(crate) const I64_STR:   &str = "i64";
    pub(crate) const ASCII_STR: &str = "ascii";
    pub(crate) const BOOL_STR:  &str = "bool";
    pub(crate) const STR_STR:   &str = "str";

    #[must_use] #[inline]
    pub(crate) const fn to_str(self) -> &'static str {
        #[rustfmt::skip]
        return match self {
            Self::I64   => Self::I64_STR,
            Self::Ascii => Self::ASCII_STR,
            Self::Bool  => Self::BOOL_STR,
            Self::Str   => Self::STR_STR,
        };
    }

    #[expect(clippy::single_call_fn)]
    #[must_use] #[inline]
    pub(crate) fn from_str(name: &str) -> Option<Self> {
        #[rustfmt::skip]
        return match name {
            Self::I64_STR   => Some(Self::I64),
            Self::ASCII_STR => Some(Self::Ascii),
            Self::BOOL_STR  => Some(Self::Bool),
            Self::STR_STR   => Some(Self::Str),
            _               => None,
        };
    }

    #[must_use] #[inline]
    pub(crate) const fn size_of(&self) -> usize {
        #[rustfmt::skip]
        return match self {
            Self::I64   => size_of::<i64>(),
            Self::Ascii => size_of::<ascii>(),
            Self::Bool  => size_of::<bool>(),
            Self::Str   => size_of::<u64>() + size_of::<*const ascii>(),
        };
    }
}

impl SizeOf for BaseType {
    #[inline(always)]
    fn size(&self) -> usize {
        return self.size_of();
    }
}

impl core::fmt::Display for BaseType {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.write_str(self.to_str());
    }
}
