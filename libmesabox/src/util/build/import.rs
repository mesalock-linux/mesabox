macro_rules! import_utils {
    ($($group:ident { $(($util:tt, $feature:expr)),+ }),*) => {
        $(
            mod $group {
                $(
                    import_utils!(@import_single $util, $feature);
                )+
            }
        )*
    };

    (@import_single true, $feature:expr) => {
        #[cfg(feature = $feature)]
        #[path = "true.rs"]
        pub mod true_mod;
    };

    (@import_single false, $feature:expr) => {
        #[cfg(feature = $feature)]
        #[path = "false.rs"]
        pub mod false_mod;
    };

    (@import_single $util:ident, $feature:expr) => {
        #[cfg(feature = $feature)]
        pub mod $util;
    }
}
