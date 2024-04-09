#[derive(Clone, Copy, PartialEq, Eq, Default, Debug, Ord, PartialOrd)]
pub enum VHDLStandard {
    VHDL1993,
    #[default]
    VHDL2008,
    VHDL2019,
}

#[test]
fn order_of_standards() {
    assert!(VHDLStandard::VHDL2008 > VHDLStandard::VHDL1993);
}

impl TryFrom<&str> for VHDLStandard {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use VHDLStandard::*;
        Ok(match value {
            "1993" | "93" => VHDL1993,
            "2008" | "08" => VHDL2008,
            "2019" | "19" => VHDL2019,
            _ => return Err(()),
        })
    }
}

impl AsRef<str> for VHDLStandard {
    fn as_ref(&self) -> &str {
        use VHDLStandard::*;
        match self {
            VHDL1993 => "1993",
            VHDL2008 => "2008",
            VHDL2019 => "2019",
        }
    }
}
