type T = ();

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct DynVal(pub *mut T);
