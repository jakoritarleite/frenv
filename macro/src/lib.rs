use std::cell::RefCell;
use std::fmt::Display;

use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use syn::meta::ParseNestedMeta;
use syn::parse_macro_input;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::DeriveInput;
use syn::Token;
use syn::Type;
use syn_path::path;

// All types which implements FromStr
// https://doc.rust-lang.org/std/str/trait.FromStr.html
const SUPPORTED_TYPES: [&str; 36] = [
    "IpAddr",
    "SocketAddr",
    "bool",
    "char",
    "f32",
    "f64",
    "i16",
    "i32",
    "i64",
    "i128",
    "isize",
    "u8",
    "u16",
    "u32",
    "u64",
    "u128",
    "usize",
    "OsString",
    "Ipv4Addr",
    "Ipv6Addr",
    "SocketAddrV4",
    "SocketAddrV6",
    "NonZero<i8>",
    "NonZero<i16>",
    "NonZero<i32>",
    "NonZero<i64>",
    "NonZero<i128>",
    "NonZero<isize>",
    "NonZero<u8>",
    "NonZero<u16>",
    "NonZero<u32>",
    "NonZero<u64>",
    "NonZero<u128>",
    "NonZero<usize>",
    "PathBuf",
    "String",
];
#[proc_macro_derive(FromEnv, attributes(frenv))]
pub fn derive_from_env(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    expand_derive_from_env(&mut input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn expand_derive_from_env(input: &mut DeriveInput) -> syn::Result<TokenStream> {
    let errors = Errors::new();

    let container = match Container::from_ast(&errors, input) {
        None => return Err(errors.check().unwrap_err()),
        Some(cont) => cont,
    };

    precondition_sized(&errors, &container);
    errors.check()?;

    let ident = &container.ident;
    let frenv = path!(frenv);

    let mut fields_impl = vec![];

    for field in container.data.fields {
        let syn::Type::Path(ty_path) = field.ty else {
            return Err(syn::Error::new_spanned(
                ident,
                "field type must be parseable from String, or implement FromEnv",
            ));
        };

        let field_ident = field.original.ident.clone().unwrap();
        let field_ty_str = field.ty.span().source_text();
        let field_ty = field.ty;

        let prefixed_ident = match field.attrs.prefix_rules {
            PrefixRules::None => field_ident.to_string().to_uppercase(),

            PrefixRules::Literal(prefix) => {
                format!("{}_{}", prefix, field_ident.to_string().to_uppercase())
            }

            _ => unreachable!(),
        };

        let prefixed_ident = match &container.attrs.prefix_all_rules {
            PrefixRules::None => prefixed_ident,

            PrefixRules::Literal(prefix) => {
                format!("{}_{}", prefix, prefixed_ident)
            }

            PrefixRules::LowerCase => {
                format!("{}_{}", ident.to_string().to_lowercase(), prefixed_ident)
            }

            PrefixRules::UpperCase => {
                format!("{}_{}", ident.to_string().to_uppercase(), prefixed_ident)
            }

            PrefixRules::ScreamingSnakeCase => {
                unimplemented!("implement screaming snake case for container prefix")
            }
        };

        if !SUPPORTED_TYPES
            .into_iter()
            .any(|sty| ty_path.path.is_ident(&sty))
        {
            fields_impl.push(quote! {
                #field_ident: <#field_ty as #frenv::FromEnv>::from_env()?
            });

            continue;
        }

        let field_impl = match field.attrs.default {
            Default::None => quote! {
                #field_ident: std::env::var(#prefixed_ident)
                    .map_err(|_| #frenv::FromEnvironmentError::VarNotPresent(#prefixed_ident.to_string()))?
                    .parse()
                    .map_err(|_| #frenv::FromEnvironmentError::InvalidType(#prefixed_ident.to_string(), #field_ty_str.to_string()))?
            },

            Default::Impl => quote! {
                #field_ident: std::env::var(#prefixed_ident)
                    .map_err(|_| #frenv::FromEnvironmentError::VarNotPresent(#prefixed_ident.to_string()))
                    .and_then(|value| value.parse().map_err(|_| #frenv::FromEnvironmentError::InvalidType(#prefixed_ident.to_string(), #field_ty_str.to_string())))
                    .unwrap_or_default()
            },

            Default::Path(expr_path) => quote! {
                #field_ident: std::env::var(#prefixed_ident)
                    .map_err(|_| #frenv::FromEnvironmentError::VarNotPresent(#prefixed_ident.to_string()))
                    .and_then(|value| value.parse().map_err(|_| #frenv::FromEnvironmentError::InvalidType(#prefixed_ident.to_string(), #field_ty_str.to_string())))
                    .unwrap_or_else(|_| #expr_path())
            },
        };

        fields_impl.push(field_impl);
    }

    let impl_block = quote! {
        #[automatically_derived]
        impl #frenv::FromEnv for #ident {
            fn from_env() -> Result<#ident, #frenv::FromEnvironmentError> {
                Ok(#ident {
                    #(#fields_impl,)*
                })
            }
        }
    };

    Ok(impl_block)
}

/// A type to collect errors together and format them.
struct Errors(RefCell<Vec<syn::Error>>);

impl Errors {
    fn new() -> Self {
        Self(RefCell::new(Vec::new()))
    }

    fn error_spanned_by<A: ToTokens, T: Display>(&self, obj: A, message: T) {
        self.0
            .borrow_mut()
            .push(syn::Error::new_spanned(obj.into_token_stream(), message));
    }

    fn syn_error(&self, err: syn::Error) {
        self.0.borrow_mut().push(err);
    }

    fn check(self) -> syn::Result<()> {
        let mut errors = self.0.take().into_iter();

        let mut combined = match errors.next() {
            Some(first) => first,
            None => return Ok(()),
        };

        for rest in errors {
            combined.combine(rest);
        }

        Err(combined)
    }
}

/// A source data structure annotated with `#[derive(FromEnv)]` parsed into an internal
/// representation.
struct Container<'a> {
    /// The struct name (without generics).
    ident: syn::Ident,
    /// Attributes on the structure, parsed for frenv.
    attrs: ContainerAttr,
    /// The contents of the struct.
    data: Data<'a>,
    /// Original input.
    original: &'a syn::DeriveInput,
}

impl<'a> Container<'a> {
    pub fn from_ast(errors: &Errors, item: &'a syn::DeriveInput) -> Option<Self> {
        let attrs = ContainerAttr::from_ast(errors, item);

        let data = match &item.data {
            syn::Data::Struct(data) => match &data.fields {
                syn::Fields::Named(fields) => Data {
                    fields: fields_from_ast(errors, &fields.named),
                },

                syn::Fields::Unnamed(_) | syn::Fields::Unit => {
                    errors.error_spanned_by(item, "only flat structs are supported");
                    return None;
                }
            },

            syn::Data::Enum(_) => {
                errors.error_spanned_by(item, "enums are not supported");
                return None;
            }

            syn::Data::Union(_) => {
                errors.error_spanned_by(item, "unions are not supported");
                return None;
            }
        };

        Some(Container {
            ident: item.ident.clone(),
            attrs,
            data,
            original: item,
        })
    }
}

fn fields_from_ast<'a>(
    errors: &Errors,
    fields: &'a Punctuated<syn::Field, Token![,]>,
) -> Vec<Field<'a>> {
    fields
        .iter()
        .map(|field| Field {
            attrs: FieldAttr::from_ast(errors, field),
            ty: &field.ty,
            original: field,
        })
        .collect()
}

/// The fields of a struct.
///
/// Analogus to `syn::Data`.
struct Data<'a> {
    fields: Vec<Field<'a>>,
}

/// A field of a struct.
struct Field<'a> {
    attrs: FieldAttr,
    ty: &'a syn::Type,
    original: &'a syn::Field,
}

/// Represents field attribute information.
struct FieldAttr {
    default: Default,
    prefix_rules: PrefixRules,
}

impl FieldAttr {
    /// Extract out the `#[frenv(...)]` attributes from a struct field.
    fn from_ast(errors: &Errors, field: &syn::Field) -> Self {
        let mut default = Attr::none(errors, "default");
        let mut prefix_rules = Attr::none(errors, "prefix");

        for attr in &field.attrs {
            if !attr.path().is_ident("frenv") {
                continue;
            }

            if let syn::Meta::List(meta) = &attr.meta {
                if meta.tokens.is_empty() {
                    continue;
                }
            }

            let parsed = attr.parse_nested_meta(|meta| {
                // #[frenv(prefix = "foo")] or #[frenv(prefix = "SOMETHING")]
                if meta.path.is_ident("prefix") {
                    let prefix = get_lit_str(errors, "prefix", "prefix", &meta)?
                        .map(|lit| PrefixRules::Literal(lit.value()));

                    prefix_rules.set_opt(&meta.path, prefix);
                } else if meta.path.is_ident("default") {
                    // #[frenv(default = "...")]
                    if meta.input.peek(Token![=]) {
                        if let Some(path) = parse_lit_into_expr_path(errors, "default", &meta)? {
                            default.set(&meta.path, Default::Path(path));
                        }
                    } else {
                        // #[frenv(default)]
                        default.set(&meta.path, Default::Impl);
                    }
                }

                Ok(())
            });

            if let Err(err) = parsed {
                errors.syn_error(err);
            }
        }

        Self {
            default: default.get().unwrap_or(Default::None),
            prefix_rules: prefix_rules.get().unwrap_or(PrefixRules::None),
        }
    }
}

struct Attr<'e, T> {
    errors: &'e Errors,
    name: &'static str,
    tokens: TokenStream,
    value: Option<T>,
}

impl<'e, T> Attr<'e, T> {
    fn none(errors: &'e Errors, name: &'static str) -> Self {
        Attr {
            errors,
            name,
            tokens: TokenStream::new(),
            value: None,
        }
    }

    fn get(self) -> Option<T> {
        self.value
    }

    fn set<A: ToTokens>(&mut self, obj: A, value: T) {
        let tokens = obj.into_token_stream();

        if self.value.is_some() {
            let msg = format!("duplicate serde attribute `{}`", self.name);
            self.errors.error_spanned_by(tokens, msg);
        } else {
            self.tokens = tokens;
            self.value = Some(value);
        }
    }

    fn set_opt<A: ToTokens>(&mut self, obj: A, value: Option<T>) {
        if let Some(value) = value {
            self.set(obj, value);
        }
    }
}

/// Represents struct attribute information.
struct ContainerAttr {
    prefix_all_rules: PrefixRules,
}

impl ContainerAttr {
    /// Extract out the `#[frenv(...)]` attributes from an item.
    fn from_ast(errors: &Errors, input: &syn::DeriveInput) -> Self {
        let mut prefix_all = Attr::none(errors, "prefix");

        for attr in &input.attrs {
            if !attr.path().is_ident("frenv") {
                continue;
            }

            if let syn::Meta::List(meta) = &attr.meta {
                if meta.tokens.is_empty() {
                    continue;
                }
            }

            let parsed = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("prefix") {
                    if meta.input.peek(Token![=]) {
                        // #[frenv(prefix = "foo")] or #[frenv(prefix = "SCREAMING_SNAKE_CASE")]
                        let prefix = get_lit_str(errors, "prefix", "prefix", &meta)?
                            .map(|lit| PrefixRules::from_str(&lit.value()));
                        prefix_all.set_opt(&meta.path, prefix);
                    } else {
                        // #[frenv(prefix)]
                        prefix_all.set(&meta.path, PrefixRules::UpperCase);
                    }
                }

                Ok(())
            });

            if let Err(err) = parsed {
                errors.syn_error(err);
            }
        }

        ContainerAttr {
            prefix_all_rules: prefix_all.get().unwrap_or(PrefixRules::None),
        }
    }
}

fn parse_lit_into_expr_path(
    errors: &Errors,
    attr_name: &'static str,
    meta: &ParseNestedMeta,
) -> syn::Result<Option<syn::ExprPath>> {
    let string = match get_lit_str(errors, attr_name, attr_name, meta)? {
        Some(string) => string,
        None => return Ok(None),
    };

    Ok(match string.parse() {
        Ok(expr) => Some(expr),
        Err(_) => {
            errors.error_spanned_by(
                &string,
                format!("failed to parse path: {:?}", string.value()),
            );
            None
        }
    })
}

fn get_lit_str(
    errors: &Errors,
    attr_name: &'static str,
    meta_name: &'static str,
    meta: &ParseNestedMeta,
) -> syn::Result<Option<syn::LitStr>> {
    let expr: syn::Expr = meta.value()?.parse()?;
    let mut value = &expr;

    while let syn::Expr::Group(e) = value {
        value = &e.expr;
    }

    if let syn::Expr::Lit(syn::ExprLit {
        lit: syn::Lit::Str(lit),
        ..
    }) = value
    {
        let suffix = lit.suffix();
        if !suffix.is_empty() {
            errors.error_spanned_by(
                lit,
                format!("unexpected suffix `{}` on string literal", suffix),
            );
        }
        Ok(Some(lit.clone()))
    } else {
        errors.error_spanned_by(
            expr,
            format!(
                "expected frenv {} attribute to be a string: `{} = \"...\"`",
                attr_name, meta_name
            ),
        );
        Ok(None)
    }
}

/// The different possible ways to add a prefix in a struct.
#[derive(Clone, Default, PartialEq, Eq)]
enum PrefixRules {
    /// Don't add a prefix.
    #[default]
    None,
    /// Add a prefix from the container ident in "lowercase" style.
    LowerCase,
    /// Add a prefix from the container ident in "UPPERCASE" style.
    UpperCase,
    /// Add a prefix from the container ident in "SCREAMING_SNAKE_CASE" style.
    ScreamingSnakeCase,
    /// Add a prefix from using the given literal value.
    Literal(String),
}

const PREFIX_RULES: &[(&str, PrefixRules)] = &[
    ("none", PrefixRules::None),
    ("lowercase", PrefixRules::LowerCase),
    ("UPPERCASE", PrefixRules::UpperCase),
    ("SCREAMING_SNAKE_CASE", PrefixRules::ScreamingSnakeCase),
];

impl PrefixRules {
    pub fn from_str(prefix_str: &str) -> Self {
        for (name, rule) in PREFIX_RULES {
            if prefix_str == *name {
                return rule.clone();
            }
        }

        PrefixRules::Literal(prefix_str.to_string())
    }
}

/// Represents the default to use for a field.
#[derive(Clone, Default)]
enum Default {
    /// Field must always be specified because it does not have a default.
    #[default]
    None,
    /// The default is given by `std::default::Default::default()`.
    Impl,
    /// The default is given by this function.
    Path(syn::ExprPath),
}

fn ungroup(mut ty: &Type) -> &Type {
    while let Type::Group(group) = ty {
        ty = &group.elem;
    }

    ty
}

fn precondition_sized(errors: &Errors, container: &Container) {
    if let Some(last) = container.data.fields.last() {
        if let syn::Type::Slice(_) = ungroup(last.ty) {
            errors.error_spanned_by(
                container.original,
                "cannot load dynamically sized struct from env",
            )
        }
    }
}
