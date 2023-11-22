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
        pub(crate) fn test_type_conversion(
            self,
            into_ty: TypeDef,
        ) -> bool {
            if self == into_ty { return true; }
            match self {
                $(
                    TypeDef::$type_no_data => {
                        return match into_ty {
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
            enum [<$token_enum Token>] {
                $( [<$token:camel>] ),*
            }

            impl TryFrom<usize> for [<$token_enum Token>] {
                type Error = VmError;

                fn try_from(val: usize) -> Result<Self, VmError> {
                    match val {
                        $( $value =>
                            Ok([<$token_enum Token>]::[<$token:camel>]), )*
                        t => {
                            error!("Cannot find method for token: {}",  t);
                            Err(VmError::InvalidMethodCall)
                        }
                    }
                }
            }

            impl From<[<$token_enum Token>]> for usize {
                fn from(val: [<$token_enum Token>]) -> Self {
                    match val {
                        $( [<$token_enum Token>]::[<$token:camel>] =>
                            $value, )*
                    }
                }
            }
        }
    }
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
        $ty: ident,
        $raw_ty: path,
        $base_call: ident
        $( .$method_call: ident )*
    ) => {(
        ShortString::from($field_name),
        lazyelmtypevalue!(
            raw_bytes;
            $raw_ty;
            TypeValue::Builtin(
                BuiltinTypeValue::from(
                    $ty::new(
                        raw_bytes
                            .bytes_parser()
                            .$base_call()$(.$method_call())*
                    )
                )
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
                field(
                    $next_field_name: literal; $next_field_variant_identifier: literal,
                    $next_field_ty: ident,
                    $next_field_base_call: ident
                    $( .$next_field_method_call: ident )*
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
                        // $(
                            $(
                                lazyfield!(
                                    $next_field_name,
                                    $next_field_ty,
                                    $bytes_record_type,
                                    $next_field_base_call$(.$next_field_method_call)*
                                ),
                            )?
                        // )?
                        // $(
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
                        // )?
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
                            "Unknown field '{}' for type BmpMessage",
                            field_name.ident
                        );
                        Err(format!(
                            "Unknown field '{}' for type BmpMessage",
                            field_name.ident
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
