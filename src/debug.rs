pub static mut DEBUG: bool = false;

#[macro_export]
macro_rules! debug {
    ($name:expr) => {
        if unsafe { crate::debug::DEBUG } {
            println!("{}", $name);
        }
    };
}

#[macro_export]
macro_rules! debugln {
    () => {
        if unsafe { crate::debug::DEBUG } {
            println!("{}", line!());
        }
    };
}
