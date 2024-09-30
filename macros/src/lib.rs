use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Token};

struct Intermediate {
    function: proc_macro2::TokenStream,
    name: syn::Ident,
    identifier: proc_macro2::TokenStream,
}

struct FunctionArgs {
    runtime_ident: syn::Ident,
    name: Option<syn::Ident>,
}

impl syn::parse::Parse for FunctionArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let runtime_ident = input.parse()?;

        let mut name = None;
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            if input.peek(syn::Ident) {
                name = input.parse()?;
            }
        }

        Ok(Self {
            runtime_ident,
            name,
        })
    }
}

#[proc_macro_attribute]
pub fn roto_function(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as syn::ItemFn);
    let Intermediate {
        function,
        identifier,
        name: function_ident,
    } = generate_function(item);

    let FunctionArgs {
        runtime_ident,
        name,
    } = syn::parse(attr).unwrap();

    let name = name.unwrap_or(function_ident);

    let expanded = quote! {
        #function

        #runtime_ident.register_function(stringify!(#name), #identifier).unwrap();
    };

    TokenStream::from(expanded)
}

struct MethodArgs {
    runtime_ident: syn::Ident,
    ty: syn::Type,
    name: Option<syn::Ident>,
}

impl syn::parse::Parse for MethodArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let runtime_ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let ty = input.parse()?;

        let mut name = None;
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            if input.peek(syn::Ident) {
                name = input.parse()?;
            }
        }
        Ok(Self {
            runtime_ident,
            ty,
            name,
        })
    }
}

#[proc_macro_attribute]
pub fn roto_method(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as syn::ItemFn);
    let Intermediate {
        function,
        identifier,
        name: function_name,
    } = generate_function(item);

    let MethodArgs {
        runtime_ident,
        ty,
        name,
    } = parse_macro_input!(attr as MethodArgs);

    let name = name.unwrap_or(function_name);

    let expanded = quote! {
        #function

        #runtime_ident.register_method::<#ty, _, _>(stringify!(#name), #identifier).unwrap();
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn roto_static_method(
    attr: TokenStream,
    item: TokenStream,
) -> TokenStream {
    let item = parse_macro_input!(item as syn::ItemFn);
    let Intermediate {
        function,
        identifier,
        name: function_name,
    } = generate_function(item);

    let MethodArgs {
        runtime_ident,
        ty,
        name,
    } = parse_macro_input!(attr as MethodArgs);

    let name = name.unwrap_or(function_name);

    let expanded = quote! {
        #function

        #runtime_ident.register_static_method::<#ty, _, _>(stringify!(#name), #identifier).unwrap();
    };

    TokenStream::from(expanded)
}

fn generate_function(item: syn::ItemFn) -> Intermediate {
    let syn::ItemFn {
        attrs,
        vis,
        sig,
        block: _,
    } = item.clone();

    assert!(sig.unsafety.is_none());
    assert!(sig.variadic.is_none());

    let ident = sig.ident;
    let args = sig.inputs.iter().map(|i| {
        let syn::FnArg::Typed(syn::PatType { pat, .. }) = i else {
            panic!()
        };
        pat
    });

    let generics = sig.generics;
    let inputs = sig.inputs.clone().into_iter();
    let ret = match sig.output {
        syn::ReturnType::Default => quote!(()),
        syn::ReturnType::Type(_, t) => quote!(#t),
    };

    let input_types: Vec<_> = sig
        .inputs
        .clone()
        .into_iter()
        .map(|i| {
            let syn::FnArg::Typed(syn::PatType { ty, .. }) = i else {
                panic!()
            };
            ty
        })
        .collect();

    let underscored_types = input_types.iter().map(|_| quote!(_));
    let arg_types = quote!(_, #(#underscored_types,)*);

    let function = quote! {
        #(#attrs)*
        #vis extern "C" fn #ident #generics ( out: *mut #ret, #(#inputs,)* ) {
            #item

            unsafe { *out = #ident(#(#args),*) };
        }
    };

    let identifier = quote! {
        #ident as extern "C" fn(#arg_types)
    };

    Intermediate {
        function,
        name: ident,
        identifier,
    }
}
