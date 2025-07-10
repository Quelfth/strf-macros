use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::{Parse, ParseStream}, token::Brace, Expr, ExprBlock, Ident, LitChar, LitInt, LitStr, Result, Token};
use tap::Pipe;

struct FormattedString {
    elements: Vec<FormattedElement>,
}

enum FormattedElement {
    Str(LitStr),
    Expr(FormattedExpr),
}

struct FormattedExpr {
    expr: Expr,
    format: Format,
}

#[derive(Default)]
struct Format {
    align: Option<FillAlign>,
    sign: Option<Sign>,
    alt: bool,
    zeros: bool,
    width: Option<Count>,
    precision: Option<Count>,
    r#type: FormatType,
}

struct FillAlign {
    fill: Option<LitChar>,
    align: Align
}

enum Align {
    Left,
    Center,
    Right,
}

enum Sign {
    Plus,
    Minus,
}

enum Count {
    Static(LitInt),
    Dynamic(ExprBlock),
}

#[derive(Default)]
enum FormatType {
    #[default]
    Display,
    Debug,
    DebugLowerHex,
    DebugUpperHex,
    Octal,
    LowerHex,
    UpperHex,
    Pointer,
    Binary,
    LowerExp,
    UpperExp,
}

impl Parse for FormatType {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        
        if input.is_empty() || input.peek(LitStr) || input.peek(Brace) {
            Self::Display
        } else if lookahead.peek(Token![?]) {
            _= input.parse::<Token![?]>();
            Self::Debug
        } else if lookahead.peek(Ident) {
            let ident = input.parse::<Ident>()?;
            
            match &*ident.to_string() {
                "o" => Self::Octal,
                "x" => {
                    if input.peek(Token![?]) {
                        _= input.parse::<Token![?]>();
                        Self::DebugLowerHex
                    } else {
                        Self::LowerHex
                    }
                }
                "X" => {
                    if input.peek(Token![?]) {
                        _= input.parse::<Token![?]>();
                        Self::DebugUpperHex
                    } else {
                        Self::UpperHex
                    }
                }
                "p" => Self::Pointer,
                "b" => Self::Binary,
                "e" => Self::LowerExp,
                "E" => Self::UpperExp,
                _ => return Err(syn::Error::new_spanned(ident, "Expected format type, valid values are: o, x, X, p, b, e, E"))
            }
            
        } else {
            return Err(lookahead.error());
        }.pipe(Ok)
    }
}

impl Parse for Count {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(LitInt) {
            Self::Static(input.parse()?)
        } else {
            Self::Dynamic(input.parse()?)
        }.pipe(Ok)
    }
}

impl Parse for Sign {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        
        if lookahead.peek(Token![+]) {
            _= input.parse::<Token![+]>();
            Self::Plus
        } else if lookahead.peek(Token![-]) {
            _= input.parse::<Token![-]>();
            Self::Minus
        } else {
            return Err(lookahead.error());
        }.pipe(Ok)
    }
}

impl Parse for Align {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        
        if lookahead.peek(Token![<]) {
            _= input.parse::<Token![<]>();
            Self::Left
        } else if lookahead.peek(Token![^]) {
            _= input.parse::<Token![^]>();
            Self::Center
        } else if lookahead.peek(Token![>]) {
            _= input.parse::<Token![>]>();
            Self::Right
        } else {
            return Err(lookahead.error());
        }.pipe(Ok)
    }
}

impl Parse for FillAlign {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            fill: input.peek(LitChar).then(|| input.parse()).transpose()?,
            align: input.parse()?,
        })
    }
}

impl Parse for Format {
    fn parse(input: ParseStream) -> Result<Self> {
        if !input.peek(Token![:]) { return Ok(Self::default()) };
        _= input.parse::<Token![:]>();
        
        let align = (input.peek(LitChar) || input.peek(Token![<]) || input.peek(Token![^]) || input.peek(Token![>])).then(|| input.parse()).transpose()?;
        
        let sign = (input.peek(Token![+]) || input.peek(Token![-])).then(|| input.parse()).transpose()?;
        
        let alt = input.peek(Token![#]);
        if alt {
            _= input.parse::<Token![#]>();
        }
        
        let zeros = input.peek(LitInt) && input.fork().parse::<LitInt>()?.base10_digits() == "0";
        if zeros { input.parse::<LitInt>()?; };
        
        let width = (input.peek(LitInt) || input.peek(Brace)).then(|| input.parse()).transpose()?;
        
        let precision = (input.peek(Token![.])).then(|| {_= input.parse::<Token![.]>(); input.parse()}).transpose()?;
        
        let r#type = input.parse()?;
        
        Ok(Self {
            align,
            sign,
            alt,
            zeros,
            width,
            precision,
            r#type,
        })
    }
}

impl Parse for FormattedExpr {
    fn parse(input: ParseStream) -> Result<Self> {
        Self {
            expr: input.parse()?,
            format: input.parse()?,
        }.pipe(Ok)
    }
}

impl Parse for FormattedElement {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(LitStr) {
            Self::Str(input.parse()?)
        } else {
            Self::Expr(input.parse()?)
        }.pipe(Ok)
    }
}

impl Parse for FormattedString {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut elts = Vec::new();
        
        while !input.is_empty() {
            let elt: FormattedElement = input.parse()?;
            
            elts.push(elt);
        }
        
        Ok(Self { elements: elts })
    }
}

impl FormattedString {
    pub fn format_string(&self) -> String {
        let mut string = String::new();
        
        let mut a = 0;
        
        for elt in &self.elements {
            match elt {
                FormattedElement::Str(lit_str) => {
                    for char in lit_str.value().chars() {
                        match char {
                            '{' => string += "{{",
                            '}' => string += "}}",
                            c => string.push(c)
                        }
                    }
                }
                FormattedElement::Expr(FormattedExpr { format: Format { align, sign, alt, zeros, width, precision, r#type }, .. }) => {
                    let mut next_arg = || {
                        let result = a.to_string();
                        a += 1;
                        result
                    };
                    
                    string.push('{');
                    string += &next_arg();
                    string.push(':');
                    if let Some(FillAlign { fill, align }) = align {
                        if let Some(fill) = fill {
                            string.push(fill.value());
                        }
                        match align {
                            Align::Left => string.push('<'),
                            Align::Center => string.push('^'),
                            Align::Right => string.push('>'),
                        }
                    }
                    
                    if let Some(sign) = sign {
                        match sign {
                            Sign::Plus => string.push('+'),
                            Sign::Minus => string.push('-'),
                        }
                    }
                    
                    if *alt {
                        string.push('#')
                    }
                    if *zeros {
                        string.push('0')
                    }
                    
                    let mut write_count = |string: &mut String, count: &Count| {
                        match count {
                            Count::Static(int) => *string += &int.to_string(),
                            Count::Dynamic(_) => {
                                *string += &next_arg();
                                *string += "$";
                            }
                        }
                    };
                    
                    if let Some(width) = width {
                        write_count(&mut string, width);
                    }
                    
                    if let Some(precision) = precision {
                        string.push('.');
                        write_count(&mut string, precision);
                    }
                    
                    match r#type {
                        FormatType::Display => (),
                        FormatType::Debug => string.push('?'),
                        FormatType::DebugLowerHex => string += "x?",
                        FormatType::DebugUpperHex => string += "X?",
                        FormatType::Octal => string.push('o'),
                        FormatType::LowerHex => string.push('x'),
                        FormatType::UpperHex => string.push('X'),
                        FormatType::Pointer => string.push('p'),
                        FormatType::Binary => string.push('b'),
                        FormatType::LowerExp => string.push('e'),
                        FormatType::UpperExp => string.push('E'),
                    }
                    
                    string.push('}')
                }
            }
        }
        
        string
    }
    
    pub fn args(&self) -> Vec<proc_macro2::TokenStream> {
        let mut args = Vec::new();
        
        for elt in &self.elements {
            let FormattedElement::Expr(FormattedExpr { expr, format: Format { width, precision, .. } }) = elt else {continue};
            args.push(quote!{ #expr });
            
            #[allow(clippy::manual_flatten)]
            for count in [width, precision] {
                if let Some(Count::Dynamic(expr)) = count {
                    args.push(quote!{ #expr });
                }
            }
        }
        
        args
    }
}

#[proc_macro]
pub fn f(tokens: TokenStream) -> TokenStream {
    let formatted = syn::parse_macro_input!(tokens as FormattedString);
    
    let format_string: String = formatted.format_string();
    let args = formatted.args();
    
    quote!{
        std::format_args!(#format_string, #(#args,)*)
    }.into()
}
