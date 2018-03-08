extern crate serde_cbor as cbor;

use std::mem;
use std::slice;

fn triple_impl(x: i32) -> i32 {
    x * 3
}

fn triple_wrapper(input: &[u8]) -> Box<[u8]> {
    let param = cbor::from_slice(input).unwrap();
    cbor::to_vec(&triple_impl(param)).unwrap().into_boxed_slice()
}

#[no_mangle]
pub unsafe extern "C" fn triple(
    input: *const u8,
    input_len: usize,
    output: *mut *mut u8,
    output_len: *mut usize,
) {
    let mut result = triple_wrapper(slice::from_raw_parts(input, input_len));
    *output = result.as_mut_ptr();
    *output_len = result.len();
    mem::forget(result)
}

#[no_mangle]
pub unsafe extern "C" fn tenkei_free(buffer: *mut u8, buffer_len: usize) {
    mem::drop(Box::from_raw(slice::from_raw_parts_mut(buffer, buffer_len) as *mut [u8]));
}
