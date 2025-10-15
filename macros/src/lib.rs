use proc_macro::TokenStream;
use quote::quote;
use syn::{Error, Token, parse::Parse, parse_macro_input, spanned::Spanned};

#[proc_macro_derive(Context)]
pub fn roto_context(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as syn::DeriveInput);

    let struct_name = &item.ident;

    let syn::Data::Struct(s) = &item.data else {
        panic!("Only structs can be used as context");
    };

    let syn::Fields::Named(fields) = &s.fields else {
        panic!("Fields must be named");
    };

    let fields: Vec<_> = fields
        .named
        .iter()
        .map(|f| {
            if !matches!(f.vis, syn::Visibility::Public(_)) {
                panic!("All fields must be marked pub")
            }

            let field_name = f.ident.as_ref().unwrap();
            let field_ty = &f.ty;
            let offset = quote!(std::mem::offset_of!(Self, #field_name));
            let type_name = quote!(std::any::type_name::<#field_ty>());
            let type_id = quote!(std::any::TypeId::of::<#field_ty>());
            let docstring = gather_docstring(&f.attrs);

            quote!(
                roto::__internal::ContextField {
                    name: stringify!(#field_name),
                    offset: #offset,
                    type_name: #type_name,
                    type_id: #type_id,
                    docstring: #docstring,
                }
            )
        })
        .collect();

    let expanded = quote!(
        unsafe impl Context for #struct_name {
            fn fields() -> Vec<roto::__internal::ContextField> {
                vec![
                    #(#fields),*
                ]
            }
        }
    );

    TokenStream::from(expanded)
}

struct ItemList {
    items: Vec<ItemWithDocs>,
}

struct ItemWithDocs {
    doc: proc_macro2::TokenStream,
    item: Item,
}

enum Item {
    Type(syn::ItemType),
    Let(syn::ExprLet),
    Fn(syn::ItemFn),
    Mod(syn::Ident, ItemList),
    Impl(proc_macro2::Span, syn::Type, ItemList),
    Const(syn::ItemConst),
    Include(proc_macro2::TokenStream),
    Use(syn::ItemUse),
}

mod kw {
    syn::custom_keyword!(include);
}

impl Parse for ItemList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut items = Vec::new();

        while !input.is_empty() {
            // We need to parse the docstring and other attributes first, so
            // that we can then branch on the next keyword. Afterwards, we can
            // then assign the attributes to the item we parsed.
            let attributes = syn::Attribute::parse_outer(input)?;
            let doc = gather_docstring(&attributes);

            let look = input.lookahead1();
            let item = match () {
                // Case 1: A normal function
                _ if look.peek(Token![fn]) => {
                    let mut item: syn::ItemFn = input.parse()?;
                    item.attrs = attributes;
                    Item::Fn(item)
                }
                // Case 2: A module
                _ if look.peek(Token![mod]) => {
                    input.parse::<Token![mod]>()?;
                    let ident: syn::Ident = input.parse()?;
                    let content;
                    syn::braced!(content in input);
                    let item_list: ItemList = content.parse()?;
                    Item::Mod(ident, item_list)
                }
                // Case 3: A let binding with a closure
                _ if look.peek(Token![let]) => {
                    let mut item: syn::ExprLet = input.parse()?;
                    input.parse::<Token![;]>()?;
                    item.attrs = attributes;
                    Item::Let(item)
                }
                // Case 4: An impl block
                _ if look.peek(Token![impl]) => {
                    let tok = input.parse::<Token![impl]>()?;
                    let ty: syn::Type = input.parse()?;

                    let content;
                    syn::braced!(content in input);
                    let item_list: ItemList = content.parse()?;
                    Item::Impl(tok.span, ty, item_list)
                }
                // Case 5: A constant
                _ if look.peek(Token![const]) => {
                    let mut item: syn::ItemConst = input.parse()?;
                    item.attrs = attributes;
                    Item::Const(item)
                }
                // Case 6: A clone type
                _ if look.peek(Token![type]) => {
                    let mut item: syn::ItemType = input.parse()?;
                    item.attrs = attributes;
                    Item::Type(item)
                }
                _ if look.peek(Token![use]) => {
                    let item = input.parse()?;
                    Item::Use(item)
                }
                _ if look.peek(kw::include) => {
                    let m: syn::Macro = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Item::Include(m.tokens)
                }
                _ => return Err(look.error()),
            };

            items.push(ItemWithDocs { doc, item });
        }

        Ok(ItemList { items })
    }
}

#[proc_macro]
pub fn library(input: TokenStream) -> TokenStream {
    let parsed_items: ItemList = syn::parse_macro_input!(input);

    let expanded = to_tokens(parsed_items, None)
        .unwrap_or_else(Error::into_compile_error);

    TokenStream::from(expanded)
}

fn location(s: proc_macro2::Span) -> proc_macro2::TokenStream {
    let start = s.end();
    let line = start.line as u32;
    let column = start.column as u32;
    quote! {
        roto::Location {
            file: file!(),
            line: #line,
            column: #column,
        }
    }
}

fn to_tokens(
    item_list: ItemList,
    ty: Option<&syn::Type>,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut items = Vec::new();
    for ItemWithDocs { doc, item } in item_list.items {
        let new = match item {
            Item::Type(item) => {
                let ident = &item.ident;
                let location = location(ident.span());
                let ident_str = ident.to_string();
                let ty = &item.ty;
                let movability = get_movability(item.span(), &item.attrs)?;
                quote! {
                    roto::Type::#movability::<#ty>(
                        #ident_str,
                        #doc,
                        #location,
                    ).unwrap()
                }
            }
            Item::Let(item) => {
                let pat = item.pat;

                let syn::Pat::Ident(ident) = &*pat else {
                    todo!("good error message");
                };
                let location = location(ident.ident.span());
                let ident_str = ident.ident.to_string();

                let expr = item.expr;
                let syn::Expr::Closure(closure) = &*expr else {
                    todo!("good error message");
                };

                let params: Vec<_> = closure
                    .inputs
                    .iter()
                    .map(|p| param_name(p).unwrap())
                    .collect();

                quote! {
                    roto::Function::new(
                        #ident_str,
                        #doc,
                        { let x: Vec<&'static str> = vec![#(#params),*]; x },
                        #expr,
                        #location,
                    ).unwrap()
                }
            }
            Item::Fn(item) => {
                let sig = &item.sig;
                let ident = &sig.ident;
                let location = location(ident.span());
                let ident_str = ident.to_string();
                let params: Vec<_> = item
                    .sig
                    .inputs
                    .iter()
                    .map(|arg| match arg {
                        syn::FnArg::Receiver(_) => "self".into(),
                        syn::FnArg::Typed(pat) => {
                            param_name(&pat.pat).unwrap()
                        }
                    })
                    .collect();

                let expr = if let Some(ty) = ty {
                    // This is a trick to allow method syntax:
                    //  - We define a private extension trait in a const block.
                    //  - Then we implement that trait, which won't conflict
                    //    with anything.
                    //  - Then we export that method as a free function.
                    //
                    // We do need to map each pattern to `_` because patterns
                    // are not allowed to appear in trait definitions.
                    let new_inputs = sig
                        .inputs
                        .iter()
                        .map(|arg| match arg {
                            syn::FnArg::Receiver(rec) => {
                                syn::FnArg::Receiver(syn::Receiver {
                                    mutability: None,
                                    ..rec.clone()
                                })
                            }
                            syn::FnArg::Typed(pat_type) => {
                                syn::FnArg::Typed(syn::PatType {
                                    pat: Box::new(syn::Pat::Wild(
                                        syn::PatWild {
                                            attrs: Vec::new(),
                                            underscore_token:
                                                syn::token::Underscore {
                                                    spans: [pat_type.span()],
                                                },
                                        },
                                    )),
                                    ..pat_type.clone()
                                })
                            }
                        })
                        .collect();

                    let new_sig = syn::Signature {
                        inputs: new_inputs,
                        ident: syn::Ident::new("__ext__", sig.ident.span()),
                        ..sig.clone()
                    };

                    let mut new_item = item.clone();
                    new_item.sig.ident =
                        syn::Ident::new("__ext__", sig.ident.span());

                    quote!(const {
                        trait Ext {
                            #new_sig;
                        }

                        impl Ext for #ty {
                            #new_item
                        }

                        <#ty as Ext>::__ext__
                    })
                } else {
                    quote!({ #item #ident })
                };

                quote! {
                    roto::Function::new(
                        #ident_str,
                        #doc,
                        { let x: Vec<&'static str> = vec![#(#params),*]; x },
                        #expr,
                        #location,
                    ).unwrap()
                }
            }
            Item::Mod(ident, items) => {
                let ident_str = ident.to_string();
                let location = location(ident.span());
                let items = to_tokens(items, None)?;
                quote! {{
                    let mut module = roto::Module::new(
                        #ident_str,
                        #doc,
                        #location,
                    ).unwrap();
                    module.add(#items);
                    module
                }}
            }
            Item::Impl(span, ty, items) => {
                let items = to_tokens(items, Some(&ty))?;
                let location = location(span);
                quote! {{
                    let mut impl_block = roto::Impl::new::<#ty>(#location);
                    impl_block.add(#items);
                    impl_block
                }}
            }
            Item::Const(item) => {
                let ident = item.ident;
                let location = location(ident.span());
                let ident_str = ident.to_string();
                let ty = item.ty;
                let expr = item.expr;
                quote! {
                    roto::Constant::new::<#ty>(
                        #ident_str,
                        #doc,
                        #expr,
                        #location,
                    ).unwrap()
                }
            }
            Item::Include(item) => {
                quote! { #item }
            }
            Item::Use(item) => {
                let imports = flatten_use_tree(&item.tree);
                quote! {
                    roto::Use::new(
                        vec![#(vec![#(#imports.to_string()),*]),*],
                        roto::location!(),
                    )
                }
            }
        };

        items.push(quote! { #new });
    }

    Ok(quote! {{
        let mut lib = roto::Library::new();
        #(roto::Registerable::add_to_lib(#items, &mut lib);)*
        lib
    }})
}

fn get_movability(
    span: proc_macro2::Span,
    attrs: &[syn::Attribute],
) -> syn::Result<syn::Ident> {
    let mut clone = 0;
    let mut copy = 0;
    let mut value = 0;
    let mut ident_span = None;

    for attr in attrs {
        if let syn::Meta::Path(p) = &attr.meta {
            if p.is_ident("clone") {
                clone += 1;
                ident_span = Some(p.span());
            } else if p.is_ident("copy") {
                copy += 1;
                ident_span = Some(p.span());
            } else if p.is_ident("value") {
                value += 1;
                ident_span = Some(p.span());
            }
        }
    }

    let s = match (clone, copy, value) {
        (1, 0, 0) => "clone",
        (0, 1, 0) => "copy",
        (0, 0, 1) => "value",
        _ => {
            return Err(syn::Error::new(
                span,
                "specify exactly 1 of `#[clone]`, `#[copy]` or `#[value]`",
            ));
        }
    };

    Ok(syn::Ident::new(s, ident_span.unwrap()))
}

fn flatten_use_tree(tree: &syn::UseTree) -> Vec<Vec<String>> {
    match tree {
        syn::UseTree::Path(p) => {
            let recursive = flatten_use_tree(&p.tree);
            recursive
                .into_iter()
                .map(|v| {
                    let mut new_v = vec![p.ident.to_string()];
                    new_v.extend(v);
                    new_v
                })
                .collect()
        }
        syn::UseTree::Name(name) => {
            vec![vec![name.ident.to_string()]]
        }
        syn::UseTree::Group(g) => {
            g.items.iter().flat_map(flatten_use_tree).collect()
        }
        syn::UseTree::Rename(_) => panic!(),
        syn::UseTree::Glob(_) => panic!(),
    }
}

fn param_name(pat: &syn::Pat) -> Option<String> {
    match pat {
        syn::Pat::Ident(ident) => Some(ident.ident.to_string()),
        syn::Pat::Paren(paren) => param_name(&paren.pat),
        syn::Pat::Reference(reference) => param_name(&reference.pat),
        syn::Pat::TupleStruct(pat) => {
            let elems: Vec<_> = pat.elems.iter().collect();
            let [elem] = &*elems else { return None };
            param_name(elem)
        }
        syn::Pat::Type(p) => param_name(&p.pat),
        syn::Pat::Wild(_) => Some("_".to_string()),

        // Rust will ensure that any name bound in any or the or pattern cases
        // will also appear in the other cases and error out otherwise.
        // Therefore, we can just look at the first case to extract the name.
        syn::Pat::Or(p) => param_name(p.cases.first()?),

        // ---
        syn::Pat::Verbatim(_) => None,
        syn::Pat::Tuple(_) => None,
        syn::Pat::Struct(_) => None,
        syn::Pat::Slice(_) => None,
        syn::Pat::Rest(_) => None,
        syn::Pat::Range(_) => None,
        syn::Pat::Path(_) => None,
        syn::Pat::Const(_) => None,
        syn::Pat::Lit(_) => None,
        syn::Pat::Macro(_) => None,
        _ => None,
    }
}

struct Intermediate {
    function: proc_macro2::TokenStream,
    ident: syn::Ident,
    docstring: proc_macro2::TokenStream,
    parameter_names: proc_macro2::TokenStream,
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
        ident,
        docstring,
        parameter_names,
    } = generate_function(item);

    let FunctionArgs {
        runtime_ident,
        name,
    } = syn::parse(attr).unwrap();

    let name = name.unwrap_or(ident.clone());

    let expanded = quote! {
        #function

        #runtime_ident.register_fn(
            stringify!(#name),
            #docstring,
            #parameter_names,
            #ident,
        ).unwrap();
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
        ident,
        docstring,
        parameter_names,
    } = generate_function(item);

    let MethodArgs {
        runtime_ident,
        ty,
        name,
    } = parse_macro_input!(attr as MethodArgs);

    let name = name.unwrap_or(ident.clone());

    let expanded = quote! {
        #function

        #runtime_ident.register_method::<#ty, _, _>(
            stringify!(#name),
            #docstring,
            #parameter_names,
            #ident
        ).unwrap();
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
        ident,
        docstring,
        parameter_names,
    } = generate_function(item);

    let MethodArgs {
        runtime_ident,
        ty,
        name,
    } = parse_macro_input!(attr as MethodArgs);

    let name = name.unwrap_or(ident.clone());

    let expanded = quote! {
        #function

        #runtime_ident.register_static_method::<#ty, _, _>(
            stringify!(#name),
            #docstring.to_string(),
            #parameter_names,
            #ident
        ).unwrap();
    };

    TokenStream::from(expanded)
}

fn gather_docstring(attrs: &[syn::Attribute]) -> proc_macro2::TokenStream {
    let mut docstring = Vec::new();

    for attr in attrs {
        if attr.path().is_ident("doc") {
            let value = match &attr.meta {
                syn::Meta::NameValue(name_value) => &name_value.value,
                _ => panic!("doc attribute must be a name and a value"),
            };
            docstring.push(value);
        }
    }

    quote! {{
        let x: Vec<String> = vec![#({
            let s: String = #docstring.to_string();
            s.strip_prefix(" ").unwrap_or(&s).to_string()
        }),*];
        x.join("\n")
    }}
}

fn generate_function(item: syn::ItemFn) -> Intermediate {
    let syn::ItemFn {
        attrs,
        vis: _,
        sig,
        block: _,
    } = item.clone();

    let docstring = gather_docstring(&attrs);

    assert!(sig.unsafety.is_none());
    assert!(sig.variadic.is_none());

    let ident = sig.ident;
    let args: Vec<_> = sig
        .inputs
        .iter()
        .map(|i| {
            let syn::FnArg::Typed(syn::PatType { pat, .. }) = i else {
                panic!()
            };
            pat
        })
        .collect();

    let parameter_names = quote!( [#(stringify!(#args)),*] );

    Intermediate {
        function: quote!(#item),
        ident,
        docstring,
        parameter_names,
    }
}
