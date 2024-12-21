use proc_macro2::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Meta;
use syn::Token;
use syn_path::path;

#[proc_macro_derive(FromEnv, attributes(frenv))]
pub fn from_env_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let prefix_attr = match parse_prefix_attr(&input) {
        Err(err) => {
            return err.to_compile_error().into();
        }
        Ok(attr) => attr,
    };

    let token_stream = match &input.data {
        syn::Data::Struct(data) => parse_struct(&input, prefix_attr, data),
        syn::Data::Enum(_) => {
            syn::Error::new(input.span(), "enums are not supported!").to_compile_error()
        }
        syn::Data::Union(_) => {
            syn::Error::new(input.span(), "union types are not supported!").to_compile_error()
        }
    };

    token_stream.into()
}

fn parse_prefix_attr(input: &DeriveInput) -> syn::Result<Option<String>> {
    let mut prefix = None::<String>;

    for attr in &input.attrs {
        if matches!(attr.style, syn::AttrStyle::Inner(_)) {
            return Err(syn::Error::new_spanned(
                attr,
                "cannot use inner attributes!",
            ));
        }

        if !attr.path().is_ident("frenv") {
            continue;
        }

        let nested = attr.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?;
        for meta in nested {
            match &meta {
                // #[frenv(prefix)]
                Meta::Path(path) if path.is_ident("prefix") => {
                    prefix = Some(input.ident.to_string().to_uppercase());
                }

                // #[frenv(prefix = "SOMETHING")]
                Meta::NameValue(name_value)
                    if name_value.path.is_ident("prefix")
                        && matches!(name_value.value, syn::Expr::Lit(_)) =>
                {
                    let syn::Expr::Lit(expr_lit) = &name_value.value else {
                        unreachable!()
                    };

                    let syn::Lit::Str(value) = &expr_lit.lit else {
                        return Err(syn::Error::new_spanned(name_value, "invalid value type"));
                    };

                    prefix = Some(value.value())
                }

                _ => return Err(syn::Error::new_spanned(meta, "unrecognized frenv")),
            }
        }
    }

    Ok(prefix)
}

fn parse_struct(ast: &DeriveInput, prefix: Option<String>, data: &DataStruct) -> TokenStream {
    let name = &ast.ident;

    let path = path!(::frenv);

    match &data.fields {
        syn::Fields::Named(named_fields) => {
            let env_vars = named_fields
                .named
                .iter()
                .filter_map(|field| field.ident.as_ref().map(|ident| (ident, &field.ty)))
                .map(|(ident, ty)| {
                    let ident_env_var = match prefix.as_ref() {
                        None => ident.to_string().to_uppercase(),
                        Some(prefix) => format!("{}_{}", prefix, ident.to_string().to_uppercase()),
                    };

                    let field_ty = ty.span().source_text();

                    let syn::Type::Path(ty_path) = ty else {
                        return Err(syn::Error::new_spanned(ident, "field type must be parseable from String, or implement FromEnv"));
                    };

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
                        "String"
                    ];

                    for sty in SUPPORTED_TYPES {
                        if ty_path.path.is_ident(sty) {
                            return Ok(quote! {
                                #ident: std::env::var(#ident_env_var)
                                    .map_err(|_| #path::FromEnvironmentError::VarNotPresent(#ident_env_var.to_string()))?
                                    .parse()
                                    .map_err(|_| #path::FromEnvironmentError::InvalidType(#ident_env_var.to_string(), #field_ty.to_string()))?
                            });
                        }
                    }

                    Ok(quote! {
                        #ident: <#ty as #path::FromEnv>::from_env()?
                    })
                })
                .collect::<syn::Result<Vec<_>>>();

            let env_vars = match env_vars {
                Err(err) => return err.to_compile_error(),
                Ok(value) => value,
            };

            quote! {
                impl #path::FromEnv for #name {
                    fn from_env() -> Result<#name, #path::FromEnvironmentError> {
                        Ok(#name {
                            #(#env_vars,)*
                        })
                    }
                }
            }
        }

        _ => syn::Error::new_spanned(name, "only flat structs are supported").to_compile_error(),
    }
}
