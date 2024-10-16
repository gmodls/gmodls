macro_rules! lower {
    ($e:expr) => {
        $e | 0x20
    };
}

pub(crate) use lower;

macro_rules! is_whitespace {
    ($e:expr) => {
        match $e {
            b' ' | b'\n' | b'\t' => true,
            _ => false,
        }
    };
}

pub(crate) use is_whitespace;

macro_rules! is_alphanumeric {
    ($e:expr) => {
        match lower!($e) {
            b'a'..=b'z' => true,
            b'0'..=b'9' => true,
            _ => false,
        }
    };
}

pub(crate) use is_alphanumeric;

macro_rules! is_reserved {
    ($e:expr) => {
        match $e {
            "break" | "do" | "else" | "elseif" | "end" | "for" | "function" | "if" | "in"
            | "local" | "repeat" | "return" | "then" | "until" | "while" | "goto" | "and"
            | "or" | "nil" | "true" | "false" | "continue" => true,
            _ => false,
        }
    };
}

pub(crate) use is_reserved;

#[macro_export]
macro_rules! src {
    ($s:expr) => {
        $crate::source::Source::from(&$s[..])
    };
}

#[macro_export]
macro_rules! span {
    ($start:expr, $end:expr) => {
        $crate::span::Span::new($start, $end)
    };
}

#[macro_export]
macro_rules! pos {
    ($a: expr, $b: expr, $c: expr) => {
        $crate::span::Pos::new($a, $b, $c)
    };
}
