use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

///
///
/// ```rust,no_run
/// fn foo(a1: A1, a2: A2) -> Ret {
///    /* ... */
/// }
/// ```
///
///
#[proc_macro_attribute]
pub fn roto_function(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);

    let ItemFn {
        attrs,
        vis,
        sig,
        block: _,
    } = input.clone();

    assert!(sig.unsafety.is_none());
    assert!(sig.generics.params.is_empty());
    assert!(sig.generics.where_clause.is_none());
    assert!(sig.variadic.is_none());

    let ident = sig.ident;
    let args = sig.inputs.iter().map(|i| {
        let syn::FnArg::Typed(syn::PatType { pat, .. }) = i else {
            panic!()
        };
        pat
    });

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

    let arg_types = quote!(*mut #ret, #(#input_types,)*);

    let expanded = quote! {
        #[allow(non_upper_case_globals)]
        #vis const #ident: extern "C" fn(#arg_types) = {
            #(#attrs)*
            extern "C" fn #ident ( out: *mut #ret, #(#inputs,)* ) {
                #input

                unsafe { *out = #ident(#(#args),*) };
            }

            #ident as extern "C" fn(#arg_types)
        };
    };

    TokenStream::from(expanded)
}
