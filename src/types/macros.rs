#[macro_export]
macro_rules! typedefconversion {
    ( 
        $( $type_no_data:ident ( $( $into_type:ident ),* ; $( $into_type_nest:ident ),* ) ),* ;
        $( $type_data:ident ( $( $into_type_data:ident ),* ; $( $into_type_nest_data:ident ),* ) ),* ;
        $( $no_conversion_no_data:ident ),* ;
        $( $no_conversion_data:ident ),*
    ) => {
        pub fn test_type_conversion(
            self,
            into_ty: TypeDef,
        ) -> bool {
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
                    TypeDef::$type_data(_) => {
                        return match into_ty {
                            $( 
                                TypeDef::$into_type_data => true,
                            )*
                            $(
                                TypeDef::$into_type_nest_data(_) => true,
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
        $( $token: ident = $value: literal ),*
    ) => {
        #[derive(Debug)]
        enum $token_enum {
            $( $token = $value, )*
        }

        impl From<usize> for $token_enum {
            fn from(val: usize) -> Self {
                match val {
                    $( $value => $token_enum::$token, )*
                    _ => panic!("Unknown token value: {}", val),
                }
            }
        }

        impl From<$token_enum> for usize {
            fn from(val: $token_enum) -> Self {
                match val {
                    $( $token_enum::$token => $value, )*
                }
            }
        }
    }
}

#[macro_export]
macro_rules! lazyelmtypevalue {
    (
        $raw_bytes: ident;
        $lazy_fn_body: expr
    ) => {
        LazyElementTypeValue::Lazy(
            Box::new(move |$raw_bytes| { $lazy_fn_body })
        )
    }
}

#[macro_export]
macro_rules! lazyrecord {
    (
        $lazy_record_vec: expr
    ) => {    
        LazyRecord::new(
            $lazy_record_vec
        ).unwrap()
    }
}

#[macro_export]
macro_rules! lazyfield {
    (
        $field_name: literal,
        $ty: ident,
        $base_call: ident
        $( .$method_call: ident )+
    ) => {(
        ShortString::from($field_name),
        lazyelmtypevalue!(
            raw_bytes;
            TypeValue::Builtin(
                BuiltinTypeValue::from(
                    $ty::new(
                        raw_bytes.bytes_parser().$base_call()$(.$method_call())+
                    )
                )
            ).into()
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
macro_rules! lazyenum {
    (
        $field_name: literal,
        $enum_ty: path = $enum_name: literal,
        $raw_ty: path,
        $base_call: ident
        $( .$method_call: ident )+
    ) => {(
        ShortString::from($field_name),
        LazyElementTypeValue::Lazy(Box::new( 
            |raw_bytes: &$raw_ty| {
            TypeValue::Builtin(
                EnumVariant::<u8>::new((
                    $enum_name.into(),
                    raw_bytes.bytes_parser().$base_call()$(.$method_call())+.into(),
                )).into(),
        ).into() }))
        // (
        //     "peer_type".into(),
        //     LazyElementTypeValue::Lazy(Box::new( 
        //         |raw_message: &BytesRecord<routecore::bmp::message::RouteMonitoring<bytes::Bytes>>| {
        //         TypeValue::Builtin(
        //         // BuiltinTypeValue::ConstU8EnumVariant(
        //             EnumVariant::<u8>::new(
        //                 "BMP_PEER_TYPE".into(),
        //                 raw_message.bytes()
        //                     .per_peer_header()
        //                     .peer_type()
        //                     .into(),
        //         ).into(),
        //         // )
        //     ).into() }))
        // ),
    )}
}