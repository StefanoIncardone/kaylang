/* TODO(stefano):
fix allocation of arrays of 0 items when thet will be allowed by either
- allow for zero size values
- place padding between elements (i.e. disallow zero size values by making them at least 1 byte long)
*/
// IDEA(stefano): represent false as 0 and true as non-0 instead of 1

pub mod artifacts;
pub(crate) mod asm;
pub mod compiler;

#[expect(clippy::allow_attributes)]
#[allow(clippy::useless_attribute)]
#[expect(clippy::pub_use)]
pub use compiler::Compiler;
