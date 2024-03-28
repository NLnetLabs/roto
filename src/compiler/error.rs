#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompileError {
    /// An error that we - the Roto compiler - think is caused by the Roto
    /// end-user, through a Roto script.
    User(String),
    /// A logical error caused by internal inconsistency in the Roto compiler.
    Internal(String),
    /// An error with no discernable cause, this is very worrying, btw.
    Unspecified
}

impl CompileError {
    pub fn new(message: String) -> Self {
        CompileError::User(message)
    }

    pub fn message(&self) -> Option<&str> {
        match self {
            CompileError::User(msg) => Some(msg),
            CompileError::Internal(msg) => Some(msg),
            _ => None
        }
    }
}

impl From<String> for CompileError {
    fn from(message: String) -> Self {
        CompileError::User(message)
    }
}

impl From<&str> for CompileError {
    fn from(message: &str) -> Self {
        CompileError::User(message.to_string())
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::User(msg) => write!(f, "{msg}"),
            CompileError::Internal(msg) => write!(f, "[INTERNAL] {msg}"),
            CompileError::Unspecified => write!(f, "[UNSPECIFIED]"),
        }
    }
}

impl From<CompileError> for Box<dyn std::error::Error> {
    fn from(value: CompileError) -> Self {
        let msg = if let CompileError::User(msg) | CompileError::Internal(msg) = value {
            msg
        } else {
            "None".to_string()
        };

        msg.into()
    }
}
