use proc_macro::TokenStream;

mod display;
mod error_data;
mod utils;

#[proc_macro_derive(Display, attributes(display))]
pub fn derive_display(input: TokenStream) -> TokenStream {
    display::derive_display(input)
}

#[proc_macro_derive(
    ErrorData,
    attributes(
        error,
        note,
        error_code,
        primary_label,
        secondary_label,
        secondary_labels
    )
)]
pub fn derive_error_data(input: TokenStream) -> TokenStream {
    error_data::derive_error_data(input)
}
