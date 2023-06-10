pub mod codegen;
pub mod driver;
pub mod semantic;
pub mod syntax;
pub mod util;

fn main() {
    driver::entry();
}
