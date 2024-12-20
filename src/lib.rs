extern crate renv_macro;

pub use renv_macro::FromEnv;
use thiserror::Error;

pub trait FromEnv: Sized {
    fn from_env() -> Result<Self, FromEnvironmentError>;
}

#[derive(Debug, Error)]
pub enum FromEnvironmentError {
    #[error("{0} must be set")]
    VarNotPresent(String),
    #[error("{0} type cannot be parseable into {1}")]
    InvalidType(String, String),
}
