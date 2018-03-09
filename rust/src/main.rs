extern crate serde_cbor as cbor;
#[macro_use]
extern crate structopt;

use std::ops;
use std::ptr;
use std::slice;
use structopt::StructOpt;

mod sys {
    #[link(name = "tenkei_haskell")]
    extern "C" {
        pub fn quadruple(
            input: *const u8,
            input_len: usize,
            output: *mut *mut u8,
            output_len: *mut usize,
        );
        pub fn tenkei_free(buffer: *mut u8, buffer_len: usize);
        pub fn hs_init(argc: *mut i32, argv: *mut *mut *mut u8);
        pub fn hs_exit();
    }
}

struct Buffer {
    ptr: *mut u8,
    len: usize,
}

impl Buffer {
    fn new() -> Buffer {
        Buffer {
            ptr: ptr::null_mut(),
            len: 0,
        }
    }
}

impl ops::Deref for Buffer {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        unsafe {
            slice::from_raw_parts(self.ptr, self.len)
        }
    }
}

impl Drop for Buffer {
    fn drop(&mut self) {
        unsafe {
            sys::tenkei_free(self.ptr, self.len);
        }
    }
}

fn quadruple_wrapper(input: &[u8]) -> Buffer {
    let mut buffer = Buffer::new();
    unsafe {
        sys::quadruple(input.as_ptr(), input.len(), &mut buffer.ptr, &mut buffer.len);
    }
    buffer
}

fn quadruple(x: i32) -> i32 {
    cbor::from_slice(&quadruple_wrapper(&cbor::to_vec(&x).unwrap())).unwrap()
}

#[derive(StructOpt)]
struct Options {
    integer: i32,
}

fn main() {
    unsafe {
        sys::hs_init(ptr::null_mut(), ptr::null_mut());
    }
    let options = Options::from_args();
    println!("{}", quadruple(options.integer));
    unsafe {
        sys::hs_exit();
    }
}
