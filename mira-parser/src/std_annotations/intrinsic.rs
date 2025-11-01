use super::*;
use std::str::FromStr;

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct IntrinsicAnnotation(Intrinsic);

impl Display for IntrinsicAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("@intrinsic(")?;
        Display::fmt(&self.0, f)?;
        f.write_char(')')
    }
}

impl Annotation for IntrinsicAnnotation {
    fn get_name(&self) -> &'static str {
        "intrinsic"
    }

    fn is_valid_for(&self, thing: AnnotationReceiver, annotations: &Annotations<'_>) -> bool {
        thing == AnnotationReceiver::Function
            && annotations.get_annotations::<Self>().nth(1).is_none()
    }
}

impl IntrinsicAnnotation {
    pub fn get(&self) -> Intrinsic {
        self.0
    }
}

pub fn parse<'arena>(
    mut tokens: TokenStream<'arena>,
    _: SharedCtx<'arena>,
) -> Result<IntrinsicAnnotation, ParsingError<'arena>> {
    let (name, span) = tokens.expect_string()?;
    let v = Intrinsic::from_str(&name)
        .map(IntrinsicAnnotation)
        .map_err(|_| ParsingError::InvalidIntrinsic { span, name })?;
    tokens.finish()?;
    Ok(v)
}

macro_rules! intrinsics {
    ($($name:ident => $value:ident),* $(,)? ) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        pub enum Intrinsic {
            $($name),*
        }

        impl Display for Intrinsic {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$name => f.write_str(stringify!($value)),)*
                }
            }
        }

        impl FromStr for Intrinsic {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $(stringify!($value) => Ok(Self::$name),)*
                    _ => Err(()),
                }
            }
        }
    };
}

intrinsics! {
    CallMain => call_main, // () -> void, calls the main function
    SizeOf => size_of, // <T>() -> usize, returns the size of T in bytes
    SizeOfVal => size_of_val, // <unsized T>(v: &T) -> usize, returns the size of v in bytes
    Offset => offset, // <unsized T>(v: &T, off: usize) -> &T, offsets a pointer
    GetMetadata => get_metadata, // <unsized T>(v: &T) -> usize, returns the metadata of a fat
    // pointer or 0 for a thin pointer
    WithMetadata => with_metadata, // <unsized T>(ptr: &void, data: usize) -> &T, attaches the
    // data to the ptr, assuming T is unsized. Errors if T is sized.
    TypeName => type_name, // <unsized T>() -> &str, returns the name of the type T
    Unreachable => unreachable, // marks a location as unreachable
    // that was previously there
    VolatileRead => volatile_read, // <T>(ptr: &T) -> T
    VolatileWrite => volatile_write, // <T>(ptr: &T, val: T);
}
