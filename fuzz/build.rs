fn main() {
    #[cfg(target_os = "macos")]
    for func in [
        "_libafl_main",
        "_LLVMFuzzerCustomMutator",
        "_LLVMFuzzerCustomCrossOver",
    ] {
        println!("cargo:rustc-link-arg=-Wl,-U,{func}");
    }

    #[cfg(target_os = "linux")]
    println!("cargo:rustc-link-arg=-Wl,--allow-multiple-definition");
}
