type T = ();

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct DynVal(pub *mut T);

unsafe impl Send for DynVal {}
unsafe impl Sync for DynVal {}
