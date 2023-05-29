use crate::analysis::static_expression::BitStringConversionError::EmptySignedExpansion;
use crate::ast::{BaseSpecifier, BitString};
use crate::Latin1String;
use itertools::Itertools;
use std::cmp::Ordering;
use std::iter;

/// returns whether `byte` is  an odd number when interpreted as decimal.
/// byte must be between '0' and '9', but it is up to the caller to enforce this.
fn byte_is_odd_decimal(byte: u8) -> bool {
    (byte - b'0') % 2 == 1
}

/// Converts a decimal string (i.e. "123") to a binary string (i.e. "1111011").
///
/// When there are illegal characters in the string (i.e. non decimal characters),
/// returns an `Err` with the position of the first character.
///
/// # Special cases
/// - For an empty string, return a single string containing '0'
/// - For a string with zeros, return a single string containing '0'
/// - For a string that is padded with zeros, return a string without the padding. If the
///   String without the padding is empty, rule 1 applies.
pub(crate) fn decimal_str_to_binary_str(
    value: &Latin1String,
) -> Result<Latin1String, BitStringConversionError> {
    /// Divides `value` by two where `value` is a vector of u8's representing decimals.
    /// Returns an empty string when `value` is zero
    fn str_divide_by_2(value: Vec<u8>) -> Vec<u8> {
        let mut new_s: Vec<u8> = Vec::new();
        let mut add_next = 0;

        for ch in value {
            let new_digit = (ch + b'0') / 2 + add_next;
            new_s.push(new_digit);
            add_next = if byte_is_odd_decimal(ch) { 5 } else { 0 };
        }

        // remove the first element if it's '0'
        if new_s.first() == Some(&b'0') {
            new_s.drain(..1);
        }

        new_s
    }

    if let Some(idx) = value.bytes.iter().position(|b| *b < b'0' || *b > b'9') {
        return Err(BitStringConversionError::IllegalDecimalCharacter(idx));
    }

    let mut num: Vec<u8> = value
        .bytes
        .clone()
        .into_iter()
        .skip_while(|el| *el == b'0')
        .collect();

    if num.is_empty() {
        return Ok(Latin1String::new(b"0"));
    }

    let mut stack: Vec<u8> = Vec::new();

    while !num.is_empty() {
        if byte_is_odd_decimal(*num.last().unwrap()) {
            stack.push(b'1');
        } else {
            stack.push(b'0');
        }
        num = str_divide_by_2(num);
    }
    stack.reverse();

    Ok(Latin1String::from_vec(stack))
}

#[test]
fn test_decimal_to_binary() {
    let test_cases = [
        ("", "0"),
        ("0", "0"),
        ("000", "0"),
        ("001", "1"),
        ("1", "1"),
        ("12345", "11000000111001"),
        (
            "123456781234567812345678",
            "11010001001001001101100000011011011101100011101100101101101011110111101001110",
        ),
    ];

    for (dec, bin) in test_cases {
        assert_eq!(
            decimal_str_to_binary_str(&Latin1String::from_utf8_unchecked(dec)),
            Ok(Latin1String::from_utf8_unchecked(bin))
        );
    }
}

impl BaseSpecifier {
    /// Returns whether this base specifier represents a signed value
    /// (i.e. `SX` for signed hexadecimal) or an unsigned value
    /// (i.e. `UX` or `X` for unsigned hexadecimal)
    pub fn is_signed(&self) -> bool {
        match self {
            BaseSpecifier::SX | BaseSpecifier::SO | BaseSpecifier::SB => true,
            BaseSpecifier::B
            | BaseSpecifier::UB
            | BaseSpecifier::O
            | BaseSpecifier::UO
            | BaseSpecifier::X
            | BaseSpecifier::UX
            | BaseSpecifier::D => false,
        }
    }

    /// Get the digits that are obtained by replacing `byte` with the
    /// appropriate sequence of characters as defined in the standard (section 15.8).
    ///
    /// # Special Cases
    /// If the base specifier is `D`, i.e. decimal, return the byte itself (wrapped as array)
    ///
    /// # Example
    /// ```
    /// use vhdl_lang::ast::BaseSpecifier;
    ///
    /// let digits: Vec<u8> = BaseSpecifier::UX.get_extended_digits(b'C');
    /// assert_eq!(digits, Vec::from("1100"));
    ///
    /// let digits: Vec<u8> = BaseSpecifier::O.get_extended_digits(b'F');
    /// assert_eq!(digits, Vec::from("FFF"))
    /// ```
    pub fn get_extended_digits(&self, byte: u8) -> Vec<u8> {
        match self {
            // For O, UO and SO, the values 1-7 are replaced.
            // All other values are left as-is.
            BaseSpecifier::O | BaseSpecifier::UO | BaseSpecifier::SO => match byte {
                b'0' => Vec::from("000"),
                b'1' => Vec::from("001"),
                b'2' => Vec::from("010"),
                b'3' => Vec::from("011"),
                b'4' => Vec::from("100"),
                b'5' => Vec::from("101"),
                b'6' => Vec::from("110"),
                b'7' => Vec::from("111"),
                _ => vec![byte; 3],
            },
            // For U, UX and SX, the values 1-9 and A-F are replaced.
            // All other values are left as-is.
            BaseSpecifier::X | BaseSpecifier::UX | BaseSpecifier::SX => match byte {
                b'0' => Vec::from("0000"),
                b'1' => Vec::from("0001"),
                b'2' => Vec::from("0010"),
                b'3' => Vec::from("0011"),
                b'4' => Vec::from("0100"),
                b'5' => Vec::from("0101"),
                b'6' => Vec::from("0110"),
                b'7' => Vec::from("0111"),
                b'8' => Vec::from("1000"),
                b'9' => Vec::from("1001"),
                b'A' | b'a' => Vec::from("1010"),
                b'B' | b'b' => Vec::from("1011"),
                b'C' | b'c' => Vec::from("1100"),
                b'D' | b'd' => Vec::from("1101"),
                b'E' | b'e' => Vec::from("1110"),
                b'F' | b'f' => Vec::from("1111"),
                _ => vec![byte; 4],
            },
            // Binary values are simply the values left as they are.
            BaseSpecifier::B | BaseSpecifier::UB | BaseSpecifier::SB | BaseSpecifier::D => {
                vec![byte]
            }
        }
    }
}

/// Represents errors that occur when converting a bit string to a regular string
#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) enum BitStringConversionError {
    /// Illegal decimal character encountered while converting a decimal bit string (i.e. D"12AFFE")
    /// The `usize` argument represent the position for the first illegal character in the
    /// bit_string's `value` string, (i.e. 2 for the example above)
    IllegalDecimalCharacter(usize),
    /// Signals that when converting a value and truncating, information would be lost.
    /// # Example
    /// 5B"111111" => The first '0' would be lost
    /// 8SX"0FF"   => The bit-string is positive but would be converted to a negative value
    /// The `usize` argument is the index of the first character that cannot be truncated.
    /// The `Latin1String` argument is the expanded (erroneous) String
    IllegalTruncate(usize, Latin1String),
    /// Trying to expand an empty signed expression, i.e.
    /// SX""
    EmptySignedExpansion,
}

/// Converts a `BitString` to a `Latin1String` respecting the replacement values defined in LRM
/// Returns the string as Latin1String when successful and a `BitStringConversionError` else
/// 15.8 Bit string literals
pub(crate) fn bit_string_to_string(
    bit_string: &BitString,
) -> Result<Latin1String, BitStringConversionError> {
    // Simplifies the bit string by removing all occurrences of the underscore
    // character
    let simplified_value: Vec<u8> = bit_string
        .value
        .bytes
        .clone()
        .into_iter()
        .filter(|&b| b != b'_')
        .collect();

    // For empty signed bit-strings it is unclear what the expanded value should be,
    // according to the reference:
    // For example, 2SB"" could be
    // 1) A string containing '0's, i.e. "00"
    // 2) An error
    // According to the standard, the padding value should be the leftmost character in the string
    // but an empty string does not have a leftmost character.
    if simplified_value.is_empty() {
        return match bit_string.length {
            None => Ok(Latin1String::empty()),
            Some(value) => {
                if bit_string.base.is_signed() {
                    Err(EmptySignedExpansion)
                } else {
                    Ok(Latin1String::from_vec(
                        iter::repeat(b'0').take(value as usize).collect_vec(),
                    ))
                }
            }
        };
    }

    let mut extended_value = Vec::new();

    if bit_string.base == BaseSpecifier::D {
        match decimal_str_to_binary_str(&bit_string.value) {
            Err(e) => return Err(e),
            Ok(binary_string) => extended_value = binary_string.bytes,
        }
    } else {
        for ch in simplified_value {
            extended_value.append(&mut bit_string.base.get_extended_digits(ch));
        }
    }

    // append, truncate or leave the bit-string dependent on the user-specified length
    match bit_string.length {
        None => Ok(Latin1String::from_vec(extended_value)),
        Some(_length) => {
            let length = _length as usize;
            match length.cmp(&extended_value.len()) {
                Ordering::Equal => Ok(Latin1String::from_vec(extended_value)),
                Ordering::Less => {
                    let pivot = extended_value.len() - length;
                    let first_elements = &extended_value[..pivot];
                    let last_elements = &extended_value[pivot..];
                    // This char is allowed and may be truncated from the vector
                    let allowed_char = if bit_string.base.is_signed() {
                        last_elements[0]
                    } else {
                        b'0'
                    };

                    let idx = first_elements
                        .iter()
                        .rev()
                        .position(|el| *el != allowed_char);
                    match idx {
                        Some(value) => {
                            let real_idx = last_elements.len() + value - 1;
                            let erroneous_string = Latin1String::from_vec(extended_value);
                            Err(BitStringConversionError::IllegalTruncate(
                                real_idx,
                                erroneous_string,
                            ))
                        }
                        None => Ok(Latin1String::new(last_elements)),
                    }
                }
                Ordering::Greater => {
                    let pad_char = if bit_string.base.is_signed() {
                        extended_value[0]
                    } else {
                        b'0'
                    };
                    let pad_vector = iter::repeat(pad_char)
                        .take(length - extended_value.len())
                        .chain(extended_value)
                        .collect_vec();
                    Ok(Latin1String::from_vec(pad_vector))
                }
            }
        }
    }
}

#[cfg(test)]
mod test_mod {
    use crate::analysis::static_expression::{bit_string_to_string, BitStringConversionError};
    use crate::ast::{BaseSpecifier, BitString};
    use crate::Latin1String;

    impl BitString {
        fn new(length: Option<u32>, base: BaseSpecifier, value: &str) -> BitString {
            BitString {
                length,
                base,
                value: Latin1String::from_utf8_unchecked(value),
            }
        }
    }

    #[test]
    fn an_empty_bit_string_converts_to_an_empty_string() {
        let all_base_specifiers = [
            BaseSpecifier::O,
            BaseSpecifier::UO,
            BaseSpecifier::SO,
            BaseSpecifier::X,
            BaseSpecifier::UX,
            BaseSpecifier::SX,
            BaseSpecifier::B,
            BaseSpecifier::UB,
            BaseSpecifier::SB,
            BaseSpecifier::D,
        ];
        for base_specifier in all_base_specifiers {
            assert_eq!(
                bit_string_to_string(&BitString::new(None, base_specifier, "")).unwrap(),
                Latin1String::empty()
            )
        }
    }

    #[test]
    fn test_illegal_decimal_character() {
        assert_eq!(
            bit_string_to_string(&BitString::new(None, BaseSpecifier::D, "12AFFE")),
            Err(BitStringConversionError::IllegalDecimalCharacter(2))
        );

        assert_eq!(
            bit_string_to_string(&BitString::new(None, BaseSpecifier::D, "?")),
            Err(BitStringConversionError::IllegalDecimalCharacter(0))
        );

        assert_eq!(
            bit_string_to_string(&BitString::new(None, BaseSpecifier::D, "78234+")),
            Err(BitStringConversionError::IllegalDecimalCharacter(5))
        );
    }

    #[test]
    fn test_decimal_conversion() {
        let test_cases = [
            (BitString::new(None, BaseSpecifier::D, ""), ""),
            (BitString::new(None, BaseSpecifier::D, "0"), "0"),
            (BitString::new(None, BaseSpecifier::D, "00"), "0"),
            (BitString::new(None, BaseSpecifier::D, "000"), "0"),
            (BitString::new(None, BaseSpecifier::D, "1"), "1"),
            (BitString::new(None, BaseSpecifier::D, "01"), "1"),
            (BitString::new(None, BaseSpecifier::D, "10"), "1010"),
            (
                BitString::new(None, BaseSpecifier::D, "164824"),
                "101000001111011000",
            ),
            (
                BitString::new(None, BaseSpecifier::D, "123456781234567812345678"),
                "11010001001001001101100000011011011101100011101100101101101011110111101001110",
            ),
        ];

        for (bit_string, result_string) in test_cases {
            assert_eq!(
                bit_string_to_string(&bit_string).unwrap(),
                Latin1String::from_utf8_unchecked(result_string)
            )
        }
    }

    #[test]
    fn test_illegal_truncate_position() {
        assert_eq!(
            bit_string_to_string(&BitString::new(Some(8), BaseSpecifier::SX, "0FF")),
            Err(BitStringConversionError::IllegalTruncate(
                7,
                Latin1String::new(b"000011111111")
            ))
        );

        assert_eq!(
            bit_string_to_string(&BitString::new(Some(8), BaseSpecifier::SX, "1FF")),
            Err(BitStringConversionError::IllegalTruncate(
                8,
                Latin1String::new(b"000111111111")
            ))
        );

        assert_eq!(
            bit_string_to_string(&BitString::new(Some(8), BaseSpecifier::SX, "3FF")),
            Err(BitStringConversionError::IllegalTruncate(
                9,
                Latin1String::new(b"001111111111")
            ))
        );
    }

    // Examples defined in 15.8
    #[test]
    fn spec_examples() {
        let test_cases = [
            (
                BitString::new(None, BaseSpecifier::B, "1111_1111_1111"),
                "111111111111",
            ),
            (
                BitString::new(None, BaseSpecifier::X, "FFF"),
                "111111111111",
            ),
            (BitString::new(None, BaseSpecifier::O, "777"), "111111111"),
            (
                BitString::new(None, BaseSpecifier::X, "777"),
                "011101110111",
            ),
            (
                BitString::new(None, BaseSpecifier::B, "XXXX_01LH"),
                "XXXX01LH",
            ),
            (BitString::new(None, BaseSpecifier::UO, "27"), "010111"),
            // (BitString::new(None, BaseSpecifier::UO, "2C"), "011CCC"), // TODO: is this an error in the spec?
            (BitString::new(None, BaseSpecifier::SX, "3W"), "0011WWWW"),
            (BitString::new(None, BaseSpecifier::D, "35"), "100011"),
            (
                BitString::new(Some(12), BaseSpecifier::UB, "X1"),
                "0000000000X1",
            ),
            (
                BitString::new(Some(12), BaseSpecifier::SB, "X1"),
                "XXXXXXXXXXX1",
            ),
            (
                BitString::new(Some(12), BaseSpecifier::UX, "F-"),
                "00001111----",
            ),
            (
                BitString::new(Some(12), BaseSpecifier::SX, "F-"),
                "11111111----",
            ),
            (
                BitString::new(Some(12), BaseSpecifier::UX, "000WWW"),
                "WWWWWWWWWWWW",
            ),
            (
                BitString::new(Some(12), BaseSpecifier::SX, "FFFC00"),
                "110000000000",
            ),
        ];

        let error_cases = [
            BitString::new(Some(8), BaseSpecifier::D, "511"),
            BitString::new(Some(8), BaseSpecifier::UO, "477"),
            BitString::new(Some(8), BaseSpecifier::SX, "0FF"),
            BitString::new(Some(8), BaseSpecifier::SX, "FXX"),
        ];

        for bit_string in error_cases {
            assert!(bit_string_to_string(&bit_string).err().is_some());
        }

        for (bit_string, result_string) in test_cases {
            assert_eq!(
                bit_string_to_string(&bit_string).unwrap(),
                Latin1String::from_utf8_unchecked(result_string)
            )
        }
    }
}
