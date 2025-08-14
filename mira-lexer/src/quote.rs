#[macro_export]
macro_rules! quote {
    (@ with_ctx $tokens:ident ( $($tok:tt)* )) => {
        $crate::dummy_token(&mut $tokens, $crate::TokenType::ParenLeft);
        $(quote!(@ with_ctx $tokens $tok);)*
        $crate::dummy_token(&mut $tokens, $crate::TokenType::ParenRight);
    };

    (@ with_ctx $tokens:ident [ $($tok:tt)* ]) => {
        $crate::dummy_token(&mut $tokens, $crate::TokenType::BracketLeft);
        $(quote!(@ with_ctx $tokens $tok);)*
        $crate::dummy_token(&mut $tokens, $crate::TokenType::BracketRight);
    };

    (@ with_ctx $tokens:ident { $($tok:tt)* }) => {
        $crate::dummy_token(&mut $tokens, $crate::TokenType::CurlyLeft);
        $(quote!(@ with_ctx $tokens $tok);)*
        $crate::dummy_token(&mut $tokens, $crate::TokenType::CurlyRight);
    };

    (@ with_ctx $tokens:ident let) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Let) };

    (@ with_ctx $tokens:ident ==) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::EqualEqual) };
    (@ with_ctx $tokens:ident !=) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::NotEquals) };
    (@ with_ctx $tokens:ident <=) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::LessThan) };
    (@ with_ctx $tokens:ident >) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::GreaterThan) };
    (@ with_ctx $tokens:ident !) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::LogicalNot) };
    (@ with_ctx $tokens:ident &&) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::LogicalAnd) };
    (@ with_ctx $tokens:ident ||) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::LogicalOr) };
    (@ with_ctx $tokens:ident void) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::VoidLiteral) };
    (@ with_ctx $tokens:ident =) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Equal) };
    (@ with_ctx $tokens:ident :) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Colon) };
    (@ with_ctx $tokens:ident ;) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Semicolon) };
    (@ with_ctx $tokens:ident +) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Plus) };
    (@ with_ctx $tokens:ident -) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Minus) };
    (@ with_ctx $tokens:ident *) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Asterix) };
    (@ with_ctx $tokens:ident /) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Divide) };
    (@ with_ctx $tokens:ident %) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Modulo) };
    (@ with_ctx $tokens:ident ~) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::BitwiseNot) };
    (@ with_ctx $tokens:ident &) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Ampersand) };
    (@ with_ctx $tokens:ident |) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::BitwiseOr) };
    (@ with_ctx $tokens:ident ^) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::BitwiseXor) };
    (@ with_ctx $tokens:ident PipeOp) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::PipeOperator) };
    (@ with_ctx $tokens:ident return) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Return) };
    (@ with_ctx $tokens:ident fn) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Fn) };
    (@ with_ctx $tokens:ident extern) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Extern) };
    (@ with_ctx $tokens:ident use) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Use) };
    (@ with_ctx $tokens:ident mod) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Mod) };
    (@ with_ctx $tokens:ident export) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Export) };
    (@ with_ctx $tokens:ident if) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::If) };
    (@ with_ctx $tokens:ident else) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Else) };
    (@ with_ctx $tokens:ident asm) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Asm) };
    (@ with_ctx $tokens:ident volatile) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Volatile) };
    (@ with_ctx $tokens:ident while) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::While) };
    (@ with_ctx $tokens:ident for) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::For) };
    (@ with_ctx $tokens:ident pub) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Pub) };
    (@ with_ctx $tokens:ident in) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::In) };
    (@ with_ctx $tokens:ident unsized) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Unsized) };
    (@ with_ctx $tokens:ident ..) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Range) };
    (@ with_ctx $tokens:ident ..=) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::RangeInclusive) };
    (@ with_ctx $tokens:ident ->) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::ReturnType) };
    (@ with_ctx $tokens:ident struct) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Struct) };
    (@ with_ctx $tokens:ident trait) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Trait) };
    (@ with_ctx $tokens:ident impl) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Impl) };
    (@ with_ctx $tokens:ident ,) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Comma) };
    (@ with_ctx $tokens:ident +=) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::PlusAssign) };
    (@ with_ctx $tokens:ident -=) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::MinusAssign) };
    (@ with_ctx $tokens:ident .) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Dot) };
    (@ with_ctx $tokens:ident $) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::Dollar) };
    (@ with_ctx $tokens:ident ?) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::QuestionMark) };
    (@ with_ctx $tokens:ident as) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::As) };
    (@ with_ctx $tokens:ident @) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::AnnotationIntroducer) };
    (@ with_ctx $tokens:ident ::) => { $crate::dummy_token(&mut $tokens, $crate::TokenType::NamespaceAccess) };

    ($($tok:tt)+) => {{
        let mut tokens = Vec::new();
        $(quote!(@ with_ctx tokens $tok);)+
        tokens
    }};

}
