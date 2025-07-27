macro_rules! error_codes {
    ($($code:ident),* $(,)?) => {
        $(
        #[doc = include_str!(concat!("error_codes/", stringify!($code), ".md"))]
        pub const $code: &'static str = stringify!($code);
        )*
    };
}

error_codes!(E001, E002);
