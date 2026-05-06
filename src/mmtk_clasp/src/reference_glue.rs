use crate::ClaspVM;
use mmtk::util::opaque_pointer::VMWorkerThread;
use mmtk::util::ObjectReference;
use mmtk::vm::ReferenceGlue;

pub struct VMReferenceGlue;

impl ReferenceGlue<ClaspVM> for VMReferenceGlue {
    type FinalizableType = ObjectReference;

    fn set_referent(_reference: ObjectReference, _referent: ObjectReference) {
        unimplemented!()
    }

    fn get_referent(_object: ObjectReference) -> Option<ObjectReference> {
        unimplemented!()
    }

    fn clear_referent(_object: ObjectReference) {
        unimplemented!()
    }

    fn enqueue_references(_references: &[ObjectReference], _tls: VMWorkerThread) {
        unimplemented!()
    }
}
