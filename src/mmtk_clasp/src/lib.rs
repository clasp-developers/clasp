use std::sync::OnceLock;

use mmtk::vm::VMBinding;
use mmtk::MMTK;

pub mod active_plan;
pub mod api;
pub mod collection;
pub mod object_model;
pub mod reference_glue;
pub mod scanning;
pub mod slot;

pub use slot::ClaspVMSlot;

#[derive(Default)]
pub struct ClaspVM;

impl VMBinding for ClaspVM {
    type VMObjectModel = object_model::VMObjectModel;
    type VMScanning = scanning::VMScanning;
    type VMCollection = collection::VMCollection;
    type VMActivePlan = active_plan::VMActivePlan;
    type VMReferenceGlue = reference_glue::VMReferenceGlue;
    type VMSlot = ClaspVMSlot;
    type VMMemorySlice = mmtk::vm::slot::UnimplementedMemorySlice<ClaspVMSlot>;

    const MAX_ALIGNMENT: usize = 1 << 6;
}

pub static SINGLETON: OnceLock<Box<MMTK<ClaspVM>>> = OnceLock::new();

pub(crate) fn mmtk() -> &'static MMTK<ClaspVM> {
    SINGLETON.get().expect("MMTk not initialized")
}
