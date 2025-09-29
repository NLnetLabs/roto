use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, spanned::Spanned, Token};

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
    CopyType(syn::ItemType),
    CloneType(syn::ItemType),
    ValueType(syn::ItemType),
    Let(syn::ExprLet),
    Fn(syn::ItemFn),
    Mod(syn::Ident, ListOrAssign),
    Impl(proc_macro2::Span, syn::Type, ListOrAssign),
    Const(syn::ItemConst),
    Include(syn::Expr),
    Use(syn::ItemUse),
}

#[allow(clippy::large_enum_variant)]
enum ListOrAssign {
    Assign(syn::Expr),
    List(ItemList),
}

mod kw {
    syn::custom_keyword!(clone);
    syn::custom_keyword!(copy);
    syn::custom_keyword!(value);
    syn::custom_keyword!(item);
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

            let item = match () {
                // Case 1: A normal function
                _ if input.peek(Token![fn]) => {
                    let mut item: syn::ItemFn = input.parse()?;
                    item.attrs = attributes;
                    Item::Fn(item)
                }
                // Case 2: A module
                _ if input.peek(Token![mod]) => {
                    input.parse::<Token![mod]>()?;
                    let ident: syn::Ident = input.parse()?;
                    if input.peek(Token![=]) {
                        input.parse::<Token![=]>()?;
                        let expr = input.parse()?;
                        input.parse::<Token![;]>()?;
                        Item::Mod(ident, ListOrAssign::Assign(expr))
                    } else {
                        let content;
                        syn::braced!(content in input);
                        let item_list: ItemList = content.parse()?;
                        Item::Mod(ident, ListOrAssign::List(item_list))
                    }
                }
                // Case 3: A let binding with a closure
                _ if input.peek(Token![let]) => {
                    let mut item: syn::ExprLet = input.parse()?;
                    input.parse::<Token![;]>()?;
                    item.attrs = attributes;
                    Item::Let(item)
                }
                // Case 4: An impl block
                _ if input.peek(Token![impl]) => {
                    let tok = input.parse::<Token![impl]>()?;
                    let ty: syn::Type = input.parse()?;

                    if input.peek(Token![=]) {
                        input.parse::<Token![=]>()?;
                        let expr = input.parse()?;
                        input.parse::<Token![;]>()?;
                        Item::Impl(tok.span, ty, ListOrAssign::Assign(expr))
                    } else {
                        let content;
                        syn::braced!(content in input);
                        let item_list: ItemList = content.parse()?;
                        Item::Impl(
                            tok.span,
                            ty,
                            ListOrAssign::List(item_list),
                        )
                    }
                }
                // Case 5: A constant
                _ if input.peek(Token![const]) => {
                    let mut item: syn::ItemConst = input.parse()?;
                    item.attrs = attributes;
                    Item::Const(item)
                }
                // Case 6: A clone type
                _ if input.peek(kw::clone) => {
                    input.parse::<kw::clone>()?;
                    let mut item: syn::ItemType = input.parse()?;
                    item.attrs = attributes;
                    Item::CloneType(item)
                }
                // Case 7: A copy type
                _ if input.peek(kw::copy) => {
                    input.parse::<kw::copy>()?;
                    let mut item: syn::ItemType = input.parse()?;
                    item.attrs = attributes;
                    Item::CopyType(item)
                }
                // Case 8: A value type
                _ if input.peek(kw::value) => {
                    input.parse::<kw::value>()?;
                    let mut item: syn::ItemType = input.parse()?;
                    item.attrs = attributes;
                    Item::ValueType(item)
                }
                _ if input.peek(kw::item) => {
                    input.parse::<kw::item>()?;
                    let expr: syn::Expr = input.parse()?;
                    Item::Include(expr)
                }
                _ if input.peek(Token![use]) => {
                    let item = input.parse()?;
                    Item::Use(item)
                }
                _ => {
                    todo!("good error message");
                }
            };

            items.push(ItemWithDocs { doc, item });
        }

        Ok(ItemList { items })
    }
}

#[proc_macro]
pub fn library(input: TokenStream) -> TokenStream {
    let parsed_items: ItemList = syn::parse_macro_input!(input);

    let expanded = to_tokens(parsed_items);

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

fn to_tokens(item_list: ItemList) -> proc_macro2::TokenStream {
    let mut items = Vec::new();
    for ItemWithDocs { doc, item } in item_list.items {
        let new = match item {
            Item::CopyType(item) => {
                let ident = item.ident;
                let location = location(ident.span());
                let ident_str = ident.to_string();
                let ty = item.ty;
                quote! {
                    roto::Type::copy::<#ty>(
                        #ident_str,
                        #doc,
                        #location,
                    ).unwrap()
                }
            }
            Item::CloneType(item) => {
                let location = location(item.span());
                let ident = item.ident;
                let ident_str = ident.to_string();
                let ty = item.ty;
                quote! {
                    roto::Type::clone::<#ty>(
                        #ident_str,
                        #doc,
                        #location,
                    ).unwrap()
                }
            }
            Item::ValueType(item) => {
                let ident = item.ident;
                let location = location(ident.span());
                let ident_str = ident.to_string();
                let ty = item.ty;
                quote! {
                    roto::Type::value::<#ty>(
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
                let ident = &item.sig.ident;
                let location = location(ident.span());
                let ident_str = ident.to_string();
                let params: Vec<_> = item
                    .sig
                    .inputs
                    .iter()
                    .map(|arg| {
                        let syn::FnArg::Typed(pat) = arg else {
                            panic!();
                        };
                        param_name(&pat.pat).unwrap()
                    })
                    .collect();

                quote! {
                    roto::Function::new(
                        #ident_str,
                        #doc,
                        { let x: Vec<&'static str> = vec![#(#params),*]; x },
                        { #item #ident },
                        #location,
                    ).unwrap()
                }
            }
            Item::Mod(ident, items) => {
                let ident_str = ident.to_string();
                let location = location(ident.span());
                let items = match items {
                    ListOrAssign::List(item_list) => to_tokens(item_list),
                    ListOrAssign::Assign(expr) => quote! { #expr },
                };
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
                let items = match items {
                    ListOrAssign::List(item_list) => to_tokens(item_list),
                    ListOrAssign::Assign(expr) => quote! { #expr },
                };

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

    quote! {
        roto::Library::from(vec![ #(roto::Item::from(#items)),* ])
    }
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
        let x: Vec<String> = vec![#(#docstring.to_string()),*];
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
