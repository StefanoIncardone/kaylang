use core::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub(crate) enum Reg64 {
    Rax = 0,
    Rbx = 1,
    Rcx = 2,
    Rdx = 3,

    Rsi = 4,
    Rdi = 5,
    Rbp = 6,
    Rsp = 7,

    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}

impl Display for Reg64 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Rax => write!(f, "rax"),
            Self::Rbx => write!(f, "rbx"),
            Self::Rcx => write!(f, "rcx"),
            Self::Rdx => write!(f, "rdx"),

            Self::Rsi => write!(f, "rsi"),
            Self::Rdi => write!(f, "rdi"),
            Self::Rbp => write!(f, "rbp"),
            Self::Rsp => write!(f, "rsp"),

            Self::R8  => write!(f, "r8"),
            Self::R9  => write!(f, "r9"),
            Self::R10 => write!(f, "r10"),
            Self::R11 => write!(f, "r11"),
            Self::R12 => write!(f, "r12"),
            Self::R13 => write!(f, "r13"),
            Self::R14 => write!(f, "r14"),
            Self::R15 => write!(f, "r15"),
        };
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting a `Reg64`")]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub(crate) enum Reg32 {
    Eax = Reg64::Rax as u8,
    Ebx = Reg64::Rbx as u8,
    Ecx = Reg64::Rcx as u8,
    Edx = Reg64::Rdx as u8,

    Esi = Reg64::Rsi as u8,
    Edi = Reg64::Rdi as u8,
    Ebp = Reg64::Rbp as u8,
    Esp = Reg64::Rsp as u8,

    R8d = Reg64::R8 as u8,
    R9d = Reg64::R9 as u8,
    R10d = Reg64::R10 as u8,
    R11d = Reg64::R11 as u8,
    R12d = Reg64::R12 as u8,
    R13d = Reg64::R13 as u8,
    R14d = Reg64::R14 as u8,
    R15d = Reg64::R15 as u8,
}

impl Display for Reg32 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Eax  => write!(f, "eax"),
            Self::Ebx  => write!(f, "ebx"),
            Self::Ecx  => write!(f, "ecx"),
            Self::Edx  => write!(f, "edx"),

            Self::Esi  => write!(f, "esi"),
            Self::Edi  => write!(f, "edi"),
            Self::Ebp  => write!(f, "ebp"),
            Self::Esp  => write!(f, "esp"),

            Self::R8d  => write!(f, "r8d"),
            Self::R9d  => write!(f, "r9d"),
            Self::R10d => write!(f, "r10d"),
            Self::R11d => write!(f, "r11d"),
            Self::R12d => write!(f, "r12d"),
            Self::R13d => write!(f, "r13d"),
            Self::R14d => write!(f, "r14d"),
            Self::R15d => write!(f, "r15d"),
        };
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting a `Reg64`")]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub(crate) enum Reg16 {
    Ax = Reg64::Rax as u8,
    Bx = Reg64::Rbx as u8,
    Cx = Reg64::Rcx as u8,
    Dx = Reg64::Rdx as u8,

    Si = Reg64::Rsi as u8,
    Di = Reg64::Rdi as u8,
    Bp = Reg64::Rbp as u8,
    Sp = Reg64::Rsp as u8,

    R8w = Reg64::R8 as u8,
    R9w = Reg64::R9 as u8,
    R10w = Reg64::R10 as u8,
    R11w = Reg64::R11 as u8,
    R12w = Reg64::R12 as u8,
    R13w = Reg64::R13 as u8,
    R14w = Reg64::R14 as u8,
    R15w = Reg64::R15 as u8,
}

impl Display for Reg16 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Ax   => write!(f, "ax"),
            Self::Bx   => write!(f, "bx"),
            Self::Cx   => write!(f, "cx"),
            Self::Dx   => write!(f, "dx"),

            Self::Si   => write!(f, "si"),
            Self::Di   => write!(f, "di"),
            Self::Bp   => write!(f, "bp"),
            Self::Sp   => write!(f, "sp"),

            Self::R8w  => write!(f, "r8w"),
            Self::R9w  => write!(f, "r9w"),
            Self::R10w => write!(f, "r10w"),
            Self::R11w => write!(f, "r11w"),
            Self::R12w => write!(f, "r12w"),
            Self::R13w => write!(f, "r13w"),
            Self::R14w => write!(f, "r14w"),
            Self::R15w => write!(f, "r15w"),
        };
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting a `Reg64`")]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub(crate) enum Reg8l {
    Al = Reg64::Rax as u8,
    Bl = Reg64::Rbx as u8,
    Cl = Reg64::Rcx as u8,
    Dl = Reg64::Rdx as u8,

    Sil = Reg64::Rsi as u8,
    Dil = Reg64::Rdi as u8,
    Bpl = Reg64::Rbp as u8,
    Spl = Reg64::Rsp as u8,

    R8b = Reg64::R8 as u8,
    R9b = Reg64::R9 as u8,
    R10b = Reg64::R10 as u8,
    R11b = Reg64::R11 as u8,
    R12b = Reg64::R12 as u8,
    R13b = Reg64::R13 as u8,
    R14b = Reg64::R14 as u8,
    R15b = Reg64::R15 as u8,
}

impl Display for Reg8l {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Al   => write!(f, "al"),
            Self::Bl   => write!(f, "bl"),
            Self::Cl   => write!(f, "cl"),
            Self::Dl   => write!(f, "dl"),

            Self::Sil  => write!(f, "sil"),
            Self::Dil  => write!(f, "dil"),
            Self::Bpl  => write!(f, "bpl"),
            Self::Spl  => write!(f, "spl"),

            Self::R8b  => write!(f, "r8b"),
            Self::R9b  => write!(f, "r9b"),
            Self::R10b => write!(f, "r10b"),
            Self::R11b => write!(f, "r11b"),
            Self::R12b => write!(f, "r12b"),
            Self::R13b => write!(f, "r13b"),
            Self::R14b => write!(f, "r14b"),
            Self::R15b => write!(f, "r15b"),
        };
    }
}

#[expect(dead_code, reason = "it's in reality created by trasmuting a `Reg64`")]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub(crate) enum Reg8h {
    Ah = Reg64::Rax as u8,
    Bh = Reg64::Rbx as u8,
    Ch = Reg64::Rcx as u8,
    Dh = Reg64::Rdx as u8,
}

impl Display for Reg8h {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Ah => write!(f, "ah"),
            Self::Bh => write!(f, "bh"),
            Self::Ch => write!(f, "ch"),
            Self::Dh => write!(f, "dh"),
        };
    }
}

impl Into<Reg32> for Reg64 {
    #[inline(always)]
    fn into(self) -> Reg32 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg16> for Reg64 {
    #[inline(always)]
    fn into(self) -> Reg16 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg8l> for Reg64 {
    #[inline(always)]
    fn into(self) -> Reg8l {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Option<Reg8h>> for Reg64 {
    #[inline(always)]
    fn into(self) -> Option<Reg8h> {
        return match self {
            Self::Rax | Self::Rbx | Self::Rcx | Self::Rdx => unsafe {
                Some(core::mem::transmute(self))
            },
            Self::Rsi
            | Self::Rdi
            | Self::Rbp
            | Self::Rsp
            | Self::R8
            | Self::R9
            | Self::R10
            | Self::R11
            | Self::R12
            | Self::R13
            | Self::R14
            | Self::R15 => None,
        };
    }
}

impl Into<Reg64> for Reg32 {
    #[inline(always)]
    fn into(self) -> Reg64 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg16> for Reg32 {
    #[inline(always)]
    fn into(self) -> Reg16 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg8l> for Reg32 {
    #[inline(always)]
    fn into(self) -> Reg8l {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Option<Reg8h>> for Reg32 {
    #[inline(always)]
    fn into(self) -> Option<Reg8h> {
        let reg64: Reg64 = self.into();
        return reg64.into();
    }
}

impl Into<Reg64> for Reg16 {
    #[inline(always)]
    fn into(self) -> Reg64 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg32> for Reg16 {
    #[inline(always)]
    fn into(self) -> Reg32 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg8l> for Reg16 {
    #[inline(always)]
    fn into(self) -> Reg8l {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Option<Reg8h>> for Reg16 {
    #[inline(always)]
    fn into(self) -> Option<Reg8h> {
        let reg64: Reg64 = self.into();
        return reg64.into();
    }
}

impl Into<Reg64> for Reg8l {
    #[inline(always)]
    fn into(self) -> Reg64 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg32> for Reg8l {
    #[inline(always)]
    fn into(self) -> Reg32 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg16> for Reg8l {
    #[inline(always)]
    fn into(self) -> Reg16 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Option<Reg8h>> for Reg8l {
    #[inline(always)]
    fn into(self) -> Option<Reg8h> {
        let reg64: Reg64 = self.into();
        return reg64.into();
    }
}

impl Into<Reg64> for Reg8h {
    #[inline(always)]
    fn into(self) -> Reg64 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg32> for Reg8h {
    #[inline(always)]
    fn into(self) -> Reg32 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg16> for Reg8h {
    #[inline(always)]
    fn into(self) -> Reg16 {
        return unsafe { core::mem::transmute(self) };
    }
}

impl Into<Reg8l> for Reg8h {
    #[inline(always)]
    fn into(self) -> Reg8l {
        return unsafe { core::mem::transmute(self) };
    }
}
