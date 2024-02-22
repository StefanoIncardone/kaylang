use crate::cli::logging::{CAUSE, ERROR};
use std::{
    borrow::Cow,
    fmt::{Debug, Display},
};

pub mod assembler;
pub mod compiler;
pub mod linker;
pub mod run;

#[derive(Debug)]
pub struct BackEndErrorInfo {
    pub msg: Cow<'static, str>,
    pub cause: Cow<'static, str>,
}

pub trait BackEndErrorKindInfo {
    fn info(&self) -> BackEndErrorInfo;
}

#[derive(Debug)]
pub struct BackEndError<Kind: BackEndErrorKindInfo> {
    pub kind: Kind,
}

impl<Kind: BackEndErrorKindInfo> Display for BackEndError<Kind> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let BackEndErrorInfo { msg, cause } = self.kind.info();

        write!(
            f,
            "{ERROR}: {msg}\
            \n{CAUSE}: {cause}"
        )
    }
}

impl<Kind: Debug + BackEndErrorKindInfo> std::error::Error for BackEndError<Kind> {}
