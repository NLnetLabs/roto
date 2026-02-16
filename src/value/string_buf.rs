use std::sync::{Arc, Mutex};

#[derive(Clone, Default)]
pub struct StringBuf(Arc<Mutex<String>>);

impl StringBuf {
    pub fn new() -> Self {
        Self(Default::default())
    }

    pub fn from(s: Arc<str>) -> Self {
        Self(Arc::new(Mutex::new(s.as_ref().to_owned())))
    }

    pub fn push_char(self, c: char) {
        self.0.lock().unwrap().push(c)
    }

    pub fn push_string(self, s: Arc<str>) {
        self.0.lock().unwrap().push_str(&s);
    }

    #[expect(clippy::wrong_self_convention)]
    pub fn as_string(self) -> Arc<str> {
        let s = self.0.lock().unwrap();
        (&**s).into()
    }
}

impl PartialEq for StringBuf {
    fn eq(&self, other: &Self) -> bool {
        if Arc::ptr_eq(&self.0, &other.0) {
            return true;
        }
        *self.0.lock().unwrap() == *other.0.lock().unwrap()
    }
}
