pub fn triple(x: i32) -> i32 {
    x * 3
}

pub fn quadruple(x: i32) -> i32 {
    x * 4
}

pub fn quintuple(x: i32) -> i32 {
    x * 5
}

pub struct TestType {
    test: i32,
    other_test: i32,
}

pub enum TestType2 {
    Test(i32),
    OtherTest(i32),
}
