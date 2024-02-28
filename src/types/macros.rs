#[macro_export]
macro_rules! typedefconversion {
    (
        $( $type_no_data:ident (
            $( $into_type:ident ),* ; $( $into_type_nest:ident ),*
        ) ),* ;
        $( $type_data:ident (
            $( $into_type_data:ident ),* ; $( $into_type_nest_data:ident ),*
        ) ),* ;
        $( $no_conversion_no_data:ident ),* ;
        $( $no_conversion_data:ident ),*
    ) => {
        #[allow(unreachable_patterns)]
        pub(crate) fn test_type_conversion(
            self,
            into_ty: TypeDef,
        ) -> bool {
            if self == into_ty { return true; }
            match self {
                $(
                    TypeDef::$type_no_data => {
                        return match into_ty {
                            // This arm is unreachable for some type conversion
                            // created by this macro, clippy warns about this,
                            // hence we're disabling `unreachable_patterns`.
                            $(
                                TypeDef::$into_type => true,
                            )*
                            $(
                                TypeDef::$into_type_nest(_) => true,
                            )*
                            _ => false
                        };
                    }
                )*
                $(
                    TypeDef::$type_data(_rec_def) => {
                        return match into_ty {
                            $(
                                TypeDef::$into_type_data => true,
                            )*
                            $(
                                TypeDef::$into_type_nest_data(rec_def_into) => _rec_def == rec_def_into,
                            )*
                            _ => false
                        };
                    }
                )*
                $(
                    TypeDef::$no_conversion_no_data => false,
                )*
                $(
                    TypeDef::$no_conversion_data(_) => false,
                )*
            }
        }
    }
}

#[macro_export]
macro_rules! createtoken {
    (
        $token_enum: ident;
        $( $token: ident = $value: literal )*
    ) => {
        paste! {
            #[derive(Debug)]
            enum [<$token_enum:camel Token>] {
                $( [<$token:camel>] ),*
            }

            impl TryFrom<usize> for [<$token_enum:camel Token>] {
                type Error = VmError;

                fn try_from(val: usize) -> Result<Self, VmError> {
                    match val {
                        $( $value =>
                            Ok([<$token_enum:camel Token>]::[<$token:camel>]), )*
                        t => {
                            error!("Cannot find method for token: {}",  t);
                            Err(VmError::InvalidMethodCall)
                        }
                    }
                }
            }

            impl From<[<$token_enum:camel Token>]> for usize {
                fn from(val: [<$token_enum:camel Token>]) -> Self {
                    match val {
                        $( [<$token_enum:camel Token>]::[<$token:camel>] =>
                            $value, )*
                    }
                }
            }
        }
    }
}


// into_type() method that only features a `set` method, for use in a RotoType
// implementation.
//
// takes the TypeDef variant as its input. Can be lowercase, will be
// transformed into CamelCase.
#[macro_export]
macro_rules! noconversioninto {
    (
        $type_def: ident
    ) => {
        fn into_type(
            self,
            type_def: &TypeDef,
        ) -> Result<TypeValue, CompileError> {
            paste! {
                match type_def {
                    TypeDef::[<$type_def:camel>] => {
                        Ok(TypeValue::Builtin(
                            BuiltinTypeValue::[<$type_def:camel>](self)
                        ))
                    }
                    _ => Err(format!(
                        concat!("Cannot convert type ",
                            stringify!($type_def)," to type {:?}"),
                        type_def
                    )
                    .into()),
                }
            }
        }
    };
}

// into_type() method for a TypeDef that only has 'standard' conversions,
// using the From implementation of the TypeDef variant.
//
// Takes the TypeDef variant as its input, as well as a list of TypeDef
// variants it can convert into.
#[macro_export]
macro_rules! intotype {
    (
        $my_type: ident;
        $( $into_type_def: ident ),*
    ) => {
        fn into_type(
            self,
            type_def: &TypeDef,
        ) -> Result<TypeValue, CompileError> {
            paste! {
                match type_def {
                    TypeDef::[<$my_type:camel>] => Ok(TypeValue::Builtin(
                        BuiltinTypeValue::[<$my_type:camel>](self)
                    )),
                    $( 
                        TypeDef::[<$into_type_def:camel>] => Ok(TypeValue::Builtin(
                            BuiltinTypeValue::[<$into_type_def:camel>](self.into()))),
                    )*
                    _ => {
                        Err(format!(
                            concat!(
                                "Cannot convert type ", 
                                stringify!($my_type), 
                                " to type {:?}"),
                                type_def
                            ).into()
                        )
                    }
                }
            }
        }
    };
}


// This is the same as intotype!, but it takes two lists, that come out of the
// scalartype macro. You probably don't want to use this.
#[macro_export]
macro_rules! _intotype {
    (
        $my_type: ident;
        $( $into_type_def: ident ),*;
        $( $wrapped_into_type: ident ),*
    ) => {
        fn into_type(
            self,
            type_def: &TypeDef,
        ) -> Result<TypeValue, CompileError> {
            paste! {
                match type_def {
                    TypeDef::[<$my_type:camel>] => Ok(TypeValue::Builtin(
                        BuiltinTypeValue::[<$my_type:camel>](self)
                    )),
                    $( 
                        TypeDef::[<$into_type_def:camel>] => Ok(TypeValue::Builtin(
                            BuiltinTypeValue::[<$into_type_def:camel>](self.into()))),
                    )*
                    $( 
                        TypeDef::[<$wrapped_into_type:camel>] => Ok(TypeValue::Builtin(
                            BuiltinTypeValue::[<$wrapped_into_type:camel>](self.into()))), 
                    )*
                    _ => {
                        Err(format!(
                            concat!(
                                "Cannot convert type ", 
                                stringify!($my_type), 
                                " to type {:?}"),
                                type_def
                            ).into()
                        )
                    }
                }
            }
        }
    };
}

// Methods for the RotoType implementation of a TypeDef variant that only
// features a `set` method.
#[macro_export]
macro_rules! setmethodonly {
    (
        $type_def: ident
    ) => {
        fn get_props_for_method(
            _ty: TypeDef,
            method_name: &$crate::ast::Identifier,
        ) -> Result<MethodProps, CompileError>
        where
            Self: std::marker::Sized,
        {
            paste! {
                match method_name.ident.as_str() {
                    "set" => Ok(MethodProps::new(
                        TypeDef::Unknown,
                        [<$type_def:camel Token>]::Set.into(),
                        vec![TypeDef::[<$type_def:camel>]],
                    )
                    .consume_value()),
                    _ => Err(format!(
                        concat!("Unknown method: '{}' for type ",
                            stringify!([<$type_def:camel>])),
                        method_name.ident
                    )
                    .into()),
                }
            }
        }

        fn exec_value_method<'a>(
            &'a self,
            method_token: usize,
            args: &'a [StackValue],
            _res_type: TypeDef,
        ) -> Result<TypeValue, VmError> {
            paste! {
                match method_token.try_into()? {
                    [<$type_def:camel Token>]::Set => {
                        if let TypeValue::Builtin(
                                BuiltinTypeValue::[<$type_def:camel>](value)
                            ) = args
                                    .get(0)
                                    .ok_or(VmError::InvalidMethodCall)?
                                    .as_ref() {
                            Ok(TypeValue::from(*value))
                        } else {
                            Err(VmError::AnonymousArgumentNotFound)
                        }
                    }
                }
            }
        }

        fn exec_consume_value_method(
            self,
            method_token: usize,
            mut args: Vec<TypeValue>,
            _res_type: TypeDef,
        ) -> Result<TypeValue, VmError> {
            paste! {
                match method_token.try_into()? {
                    [<$type_def:camel Token>]::Set => {
                        if let Ok(TypeValue::Builtin(BuiltinTypeValue::[<$type_def:camel>](
                            value,
                        ))) = args.remove(0).into_type(&TypeDef::[<$type_def:camel>])
                        {
                            Ok(TypeValue::Builtin(BuiltinTypeValue::[<$type_def:camel>](value)))
                        } else {
                            Err(VmError::InvalidValueType)
                        }
                    }
                }
            }
        }
    
        fn exec_type_method(
            _method_token: usize,
            _args: &[$crate::vm::StackValue],
            _res_type: $crate::types::typedef::TypeDef,
        ) -> Result<TypeValue, $crate::vm::VmError> {
            Err(VmError::InvalidMethodCall)
        }
    }
}

#[macro_export]
macro_rules! typevaluefromimpls {
    (
        $type_def: ident
    ) => {
        paste! {
            impl From<$type_def> for TypeValue {
                fn from(value: $type_def) -> Self {
                    TypeValue::Builtin(BuiltinTypeValue::[<$type_def:camel>](value))
                }
            }
        }

        paste! {
            impl From<$type_def> for BuiltinTypeValue {
                fn from(value: $type_def) -> Self {
                    BuiltinTypeValue::[<$type_def:camel>](value)
                }
            }
        }

        // impl ScalarValue for $type_def {}
    }
}

#[macro_export]
macro_rules! primitivefromimpls {
    (
        $type_def: ident;
        $( $into_wrapped_type: ident = $into_inner_type: ident),*
    ) => {
        typevaluefromimpls!($type_def);

        $(
            impl From<$type_def> for $into_wrapped_type {
                fn from(value: $type_def) -> Self {
                    $into_wrapped_type(value as $into_inner_type)
                }
            }
        )*
    }
}

#[macro_export]
macro_rules! wrappedfromimpls {
    (
        $type_def: ident;
        $( $into_type_def: ident ),*;
        $( $into_wrapped_type: ident = $into_inner_type: ident),*
    ) => {
        typevaluefromimpls!($type_def);

        $(
            impl From<$type_def> for $into_type_def {
                fn from(value: $type_def) -> Self {
                    value.0 as $into_type_def
                }
            }
        )*

        $(
            impl From<$type_def> for $into_wrapped_type {
                fn from(value: $type_def) -> Self {
                    $into_wrapped_type(value.0 as $into_inner_type)
                }
            }
        )*
    }
}

#[macro_export]
macro_rules! scalartype {
    (
        $type_def: ident;
        $( $into_type_def: ident ),*;
        $( $wrapped_into_type: ident = $into_inner_type: ident),*
    ) => {
        createtoken!($type_def; Set = 0);

        impl RotoType for $type_def {
            setmethodonly!($type_def);

            _intotype!($type_def;
                $( $into_type_def ),*;
                $( $wrapped_into_type ),*
            );
        }
        
        typevaluefromimpls!($type_def);

        $(
            impl From<$type_def> for $wrapped_into_type {
                fn from(value: $type_def) -> Self {
                    $wrapped_into_type(value.0 as $into_inner_type)
                }
            }
        )*
    }
}

// An implementation for a TypeDef variant, that has/is:
// - a ScalarValue
// - only has a `set` method
// - has no type conversions.
#[macro_export]
macro_rules! minimalscalartype {
    (
        $type_def: ident
    ) => {
  
        createtoken!($type_def; Set = 0);

        impl RotoType for $type_def {
            setmethodonly!($type_def);
            intotype!($type_def; StringLiteral);
        }

        typevaluefromimpls!($type_def);
    };
}


#[macro_export]
macro_rules! lazyelmtypevalue {
    (
        $raw_bytes: ident;
        $raw_ty: path;
        $lazy_fn_body: expr
    ) => {
        LazyElementTypeValue::Lazy(Box::new(
            move |$raw_bytes: &BytesRecord<$raw_ty>| $lazy_fn_body,
        ))
    };
}

#[macro_export]
macro_rules! lazyrecord {
    (
        $lazy_record_vec: expr
    ) => {
        LazyRecord::new($lazy_record_vec)
    };
}

// this macro produces a LazyField on a LazyRecord, i.e. a method that will be
// invoked to retrieve the value for that field. For LazyRecords that are
// backed by a BytesRecord these methods are parsers and these can fail. If
// they do so we are NOT erroring out, but instead we return a
// TypeValue::Unknown. Other LazyFields in the LazyRecord may parse just fine,
// so we want to be able to keep going here.
#[macro_export]
macro_rules! lazyfield {
    (
        $field_name: literal,
        $ty: ty,
        $raw_ty: path,
        $base_call: ident
        $( .$method_call: ident )*
    ) => {(
        ShortString::from($field_name),
        lazyelmtypevalue!(
            raw_bytes;
            $raw_ty;
            // TypeValue::Builtin(
                TypeValue::from(
                    // $ty::new(
                        raw_bytes
                            .bytes_parser()
                            .$base_call()$(.$method_call())*
                    // )
                // )
            ).try_into().unwrap_or(
                ElementTypeValue::Primitive(TypeValue::Unknown)
            )
        )
        // LazyElementTypeValue::Lazy(
        //     Box::new(move |$raw_bytes: &BytesRecord<routecore::bmp::message::RouteMonitoring<bytes::Bytes>>| {
        //         TypeValue::Builtin(
        //             $ty::new(
        //                 $method_call
        //             ).into()
        //     ).into()
        //      })
        // )
    )}
}

#[macro_export]
macro_rules! lazy_list_field {
    (
        $field_name: literal,
        $ty: ty,
        $raw_ty: path,
        $base_call: ident
        $( .$method_call: ident )*
    ) => {(
        ShortString::from($field_name),
        lazyelmtypevalue!(
            raw_bytes;
            $raw_ty;
            ElementTypeValue::Primitive(
                TypeValue::List(
                List::from(
                    // Vec<ElementTypeValue> here.
                    (    raw_bytes
                            .bytes_parser()
                            .$base_call()$(.$method_call())*    
                    )
                )
            )
        ))
    )}
}

#[macro_export]
macro_rules! lazymethodfield {
    (
        $field_name: literal,
        $ty: ty,
        $raw_ty: path,
        $base_call: expr
        // $( .$method_call: ident )*
    ) => {(
        ShortString::from($field_name),
        lazyelmtypevalue!(
            raw_bytes;
            $raw_ty;
            TypeValue::Builtin(
                BuiltinTypeValue::from(    
                    raw_bytes
                        .bytes_parser()
                        .expr
                        // .$base_call()$(.$method_call())*
                )
            ).try_into().unwrap_or(
                ElementTypeValue::Primitive(TypeValue::Unknown)
            )
        )
    )}
}

// this macro produces a LazyEnum on a LazyRecord, i.e. a method that will be
// invoked to retrieve the value for that field. For LazyRecords that are
// backed by a BytesRecord these methods are parsers and these can fail. If
// they do so we are NOT erroring out, but instead we return a
// TypeValue::Unknown. Other LazyFields in the LazyRecord may parse just fine,
// so we want to be able to keep going here.
#[macro_export]
macro_rules! lazyenum {
    (
        $field_name: literal,
        $enum_ty: path = $enum_name: literal,
        $raw_ty: path,
        $base_call: ident
        $( .$method_call: ident )*
    ) => {(
        ShortString::from($field_name),
        LazyElementTypeValue::Lazy(Box::new(
            |raw_bytes: &$raw_ty| {
            trace!("Evaluate lazy fn {} result {:?}",
                $field_name,
                raw_bytes.bytes_parser().$base_call()$(.$method_call())*
            );
            TypeValue::Builtin(
                EnumVariant::<u8>::new((
                    $enum_name.into(),
                    raw_bytes
                        .bytes_parser()
                        .$base_call()$(.$method_call())*.into(),
                )).into(),
        ).try_into().unwrap_or(
            ElementTypeValue::Primitive(TypeValue::Unknown)
        )}))
        // (
        //     "peer_type".into(),
        //     LazyElementTypeValue::Lazy(Box::new(
        //         |raw_message: &BytesRecord<routecore::bmp::message::RouteMonitoring<bytes::Bytes>>| {
        //         TypeValue::Builtin(
        //             EnumVariant::<u8>::new(
        //                 "BMP_PEER_TYPE".into(),
        //                 raw_message.bytes()
        //                     .per_peer_header()
        //                     .peer_type()
        //                     .into(),
        //         ).into(),
        //     ).into() }))
        // ),
    )}
}

#[macro_export]
macro_rules! lazy_data_enum {
    (
        $field_name: literal,
        $variant_identifier: literal,
        $enum_ty: path,
        $raw_ty: path,
        $variant: path,
        $target_ty_value: ident


        // $next_enum_data_field_name: literal;
        //             $next_enum_data_variant_identifier: literal,
        //             $next_enum_data_ty: path, // = $next_enum_data_name: literal,
        //             $next_enum_data_raw_ty: path,
        //             $next_enum_data_variant: path,
        //             $target_ty_value: path
    ) => {(
        ShortString::from($field_name),
        LazyElementTypeValue::Lazy(Box::new(
            |raw_bytes: &$raw_ty| {
            trace!("Evaluate lazy fn {}",
                $field_name
            );
            if let $variant(data) = raw_bytes.bytes_parser() {
                ElementTypeValue::Primitive(
                    TypeValue::Builtin(
                        BuiltinTypeValue::$target_ty_value(
                            Arc::new(BytesRecord(data.clone()))
                        )
                    )
                )
            }
            // TypeValue::Builtin(
            //     EnumVariant::<u8>::new((
            //         $enum_name.into(),
            //         raw_bytes
            //             .bytes_parser()
            //             .$base_call()$(.$method_call())*.into(),
            //     )).into(),
            // ).into() 
        }))
        // LazyElementTypeValue::LazyRecord(
        //     LazyRecord::new($raw_ty())
        // )
    )}
}

#[macro_export]
macro_rules! bytes_record_impl {
    (
        $bytes_record_type: ident,
        $builtin_type_variant: ident,
        #[type_def($(
            $(
                record_field(
                    $sub_record_name: literal; $self_variant_identifier: literal,
                    $(
                        $( field(
                            $field_name: literal;
                            $variant_identifier: literal,
                            $ty: ident,
                            $base_call: ident
                            $( .$method_call: ident )*
                        ) )?
                        $( enum_field(
                            $enum_field_name: literal;
                            $enum_variant_identifier: literal,
                            $enum_ty: path = $enum_name: literal,
                            $enum_raw_ty: path,
                            $enum_base_call: ident
                            $( .$enum_method_call: ident )*
                        ) )?,
                    )*
                )
            )?
            $(
                list_field(
                    $list_field_name: literal; $list_field_variant_identifier: literal,
                    $list_field_ty: ident,
                    $list_field_vec_ty: ty,
                    $list_field_base_call: ident
                    $( .$list_field_method_call: ident )*
                )
            )?
            $(
                field(
                    $next_field_name: literal; $next_field_variant_identifier: literal,
                    $next_field_ty: ident,
                    $next_field_base_call: ident
                    $( .$next_field_method_call: ident )*
                )
            )?
            $(
                method_field(
                    $m_field_name: literal; $m_field_variant_identifier: literal,
                    $m_field_ty: ident,
                    $m_field_base_call: expr
                    // $( .$next_field_method_call: ident )*
                )
            )?
            $(
                enum_data_field(
                    $next_enum_data_field_name: literal;
                    $next_enum_data_variant_identifier: literal,
                    $next_enum_data_ty: path, // = $next_enum_data_name: literal,
                    $next_enum_data_raw_ty: path,
                    $next_enum_data_variant: path,
                    $target_ty_value: ident
                )
            )?
            $(
                enum_field(
                    $next_enum_field_name: literal;
                    $next_enum_variant_identifier: literal,
                    $next_enum_ty: path = $next_enum_name: literal,
                    $next_enum_raw_ty: path,
                    $next_enum_base_call: ident
                    $( .$next_enum_method_call: ident )*
                )
            )?,
        )*)],
        $field_num: expr
    ) => {
        impl RecordType for $bytes_record_type {
            fn get_field_num() -> usize {
                $field_num
            }
            fn into_typevalue(self) -> TypeValue {
                TypeValue::Builtin(BuiltinTypeValue::$builtin_type_variant(self.into()))
            }
            fn get_name() -> & 'static str {
                stringify!($bytes_record_type)
            }
        }

        impl BytesRecord<$bytes_record_type> {
            pub(crate) fn lazy_type_def<'a>() -> LazyNamedTypeDef<
                'a,
                $bytes_record_type,
            > {
                vec![
                    $(
                        $(
                            (
                                $sub_record_name.into(),
                                LazyElementTypeValue::LazyRecord(lazyrecord!(
                                    vec![
                                        $(
                                            $( lazyfield!(
                                                $field_name,
                                                $ty,
                                                $bytes_record_type,
                                                $base_call$(.$method_call )*),
                                            )?
                                            $( lazyenum!(
                                                $enum_field_name,
                                                $enum_ty=$enum_name,
                                                $enum_raw_ty,
                                                $enum_base_call$(.$enum_method_call )*),
                                            )?
                                        )+
                                    ]
                                ))
                            ),
                        )?
                            $(
                                lazyfield!(
                                    $next_field_name,
                                    $next_field_ty,
                                    $bytes_record_type,
                                    $next_field_base_call$(.$next_field_method_call)*
                                ),
                            )?
                            $(
                                lazy_list_field!(
                                    $list_field_name,
                                    $list_field_ty,
                                    $bytes_record_type,
                                    $list_field_base_call$(.$list_field_method_call)*
                                ),
                            )?
                            $( 
                                lazy_data_enum!(
                                    $next_enum_data_field_name,
                                    $next_enum_data_variant_identifier,
                                    $next_enum_data_ty, // = $next_enum_data_name: literal,
                                    $next_enum_data_raw_ty,
                                    $next_enum_data_variant,
                                    $target_ty_value
                                ),
                            )?
                            $( 
                                lazyenum!(
                                    $next_enum_field_name,
                                    $next_enum_ty=$next_enum_name,
                                    $next_enum_raw_ty,
                                    $next_enum_base_call$(.$next_enum_method_call )*
                                ),
                            )?
                    )+
                ]
            }

            pub(crate) fn type_def() -> RecordTypeDef {
                RecordTypeDef::new(vec![
                    $(
                        $(

                                (
                                    $sub_record_name.into(),
                                    TypeDef::Record(
                                        RecordTypeDef::new(vec![
                                        $(
                                            $( (
                                                $field_name.into(),
                                                TypeDef::$ty.into()
                                            ), )?
                                            $( (
                                                    $enum_field_name.into(),
                                                    TypeDef::ConstEnumVariant(
                                                        $enum_name.into()
                                                    ).into()
                                            ), )?
                                        )+
                                        ])
                                    ).into()
                                ),

                        )?
                        $(
                            (
                                $next_field_name.into(),
                                TypeDef::$next_field_ty.into()
                            ),
                        )?
                        $(
                            (
                                $next_enum_name.into(),
                                TypeDef::ConstEnumVariant($next_enum_name.into()).into()
                            ),
                        )?
                    )+
                ])
            }

            pub(crate) fn get_props_for_field(
                field_name: &$crate::ast::Identifier,
            ) -> Result<(TypeDef, $crate::traits::Token), CompileError>
            where
                Self: std::marker::Sized,
            {
                match field_name.ident.as_str() {
                    $(
                        $(
                            $sub_record_name => Ok((
                                TypeDef::LazyRecord(LazyRecordTypeDef::$bytes_record_type),
                                Token::FieldAccess(vec![$self_variant_identifier]),)),

                            // LazyRecords are laid out in in a flat enum space,
                            // meaning all fields and sub-fields live in the same
                            // enum with different variant discriminators. We'll
                            // have to calculate the offset in the variant
                            // discriminator to align the value of the
                            // discriminator with the index of the Record Type
                            // Value. The Record Type is counting from the start
                            // of the subfield! E.g.:
                            // A TypeDef {
                            //       my_rec: LazyRecord,
                            //       my_value: U32
                            //  } would have an Enum that goes something like this:
                            //
                            // MyTypeToken {
                            //     MyRec = 0,
                            //     MyRecField1 = 1,
                            //     MyRecField2 = 2,
                            //     MyValue = 3
                            // }
                            // But its corresponding BuiltinTypeValue::Record for
                            // MyRec looks like:
                            //
                            // vec![("MyRecField1", ..), ("MyRecField2", ...)]
                            //
                            // So, the trick is to align the variant discriminator
                            // with the index.
                            $(
                                $( $field_name => Ok((
                                    TypeDef::$ty,
                                    Token::FieldAccess(vec![
                                        $variant_identifier -
                                        $self_variant_identifier - 1
                                    ]))),
                                )?
                                $( $enum_field_name => Ok((
                                    TypeDef::ConstEnumVariant($enum_name.into()),
                                    Token::FieldAccess(vec![
                                        $enum_variant_identifier -
                                        $self_variant_identifier - 1
                                    ]))),
                                )?
                            )+
                        )?
                        $(
                            $next_field_name => Ok((
                                TypeDef::$next_field_ty,
                                Token::FieldAccess(vec![
                                    $next_field_variant_identifier
                                ])
                            )),
                        )?
                        $(
                            $list_field_name => Ok((
                                TypeDef::List(Box::new(TypeDef::$list_field_ty)),
                                Token::FieldAccess(vec![
                                    $list_field_variant_identifier
                                ])
                            )),
                        )?
                        $(
                            $next_enum_field_name => Ok((
                                TypeDef::ConstEnumVariant($next_enum_name.into()),
                                Token::FieldAccess(vec![
                                    $next_enum_variant_identifier
                                ])
                            )),
                        )?
                    )+
                    _ => {
                        trace!(
                            "Unknown field '{}' for type {}",
                            field_name.ident,
                            stringify!($builtin_type_variant)
                        );
                        Err(format!(
                            "Unknown field '{}' for type {}",
                            field_name.ident,
                            stringify!($builtin_type_variant)
                        )
                        .into())
                    }
                }
            }
        }

        paste!(
            createtoken!(
                $bytes_record_type;
                $(
                    $(
                        [<$sub_record_name>] = $self_variant_identifier
                        $(
                            $( [<$sub_record_name _$field_name>] = $variant_identifier )?
                            $( [<$sub_record_name _$enum_field_name>] = $enum_variant_identifier )?
                        )+
                    )?
                    $(
                        [< $next_field_name >] = $next_field_variant_identifier
                    )?
                    $(
                        [< $next_enum_name >] = $next_enum_variant_identifier
                    )?
                )+
            );
        );
    }
}

// These are two small macros turn the Option<..> returned from first into an
// appropriate error.
#[macro_export]
macro_rules! first_into_compile_err {
    ( 
        $method_call: expr
    ) => {
        $method_call.first().ok_or(CompileError::Internal(
            format!("Cannot find: '{:?}'", $method_call)
        ))
    }
}

#[macro_export]
macro_rules! first_into_vm_err {
    ( 
        $method_call: expr,
        $err: ident
    ) => {
        $method_call.first().ok_or(VmError::$err)
    }
}

// #[macro_export]
// macro_rules! subrecord_impl {
//     (
//         $bytes_record_type: ident,
//         $sub_record_name: literal,
//         { $( (
//                 $field_name: literal;
//                 $variant_identifier: literal,
//                 $ty: ident
//                 // $base_call: ident
//                 // $( .$method_call: ident )*
//             ),
//         )+ },
//         { $(
//             (
//                 $enum_field_name: literal;
//                 $enum_variant_identifier: literal,
//                 $enum_ty: path = $enum_name: literal
//                 // $enum_raw_ty: path,
//                 // $enum_base_call: ident
//                 // $( .$enum_method_call: ident )*
//             ),
//         )+ }

//     ) => {
//         impl BytesRecord<$bytes_record_type> {
//             pub(crate) fn type_def() -> Vec<NamedTypeDef> {
//                 vec![
//                     $( ( $field_name.into(), TypeDef::$ty.into() ), )+
//                     $( ( $enum_field_name.into(), TypeDef::ConstEnumVariant($enum_name.into()).into() ), )+
//                 ]
//             }

//             pub(crate) fn get_props_for_field(
//                 field_name: &$crate::ast::Identifier,
//             ) -> Result<(TypeDef, $crate::traits::Token), CompileError> {
//                 match field_name.ident.as_str() {
//                     $( $field_name => Ok((
//                         TypeDef::$ty,
//                         Token::FieldAccess(vec![$variant_identifier])
//                     )), )+
//                     // "is_ipv4" => Ok((
//                     //     TypeDef::Boolean,
//                     //     Token::FieldAccess(vec![usize::from(
//                     //         PerPeerHeaderToken::IsIpv4,
//                     //     ) as u8]),
//                     // )),
//                     _ => Err(format!(
//                         "Unknown field '{}' for type PerPeerHEader",
//                         field_name.ident
//                     )
//                     .into()),
//                 }
//             }
//         }

//         createtoken!(
//             $bytes_record_type;
//             $( $field_name = $variant_identifier ),+,
//             $( $enum_field_name = $enum_variant_identifier ),+
//         );
//     }
// }
