pub enum VhdlVersion {
    VHDL1993,
    VHDL2008,
    VHDL2019,
}

impl TryFrom<&str> for VhdlVersion {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use VhdlVersion::*;
        Ok(match value {
            "1993" => VHDL1993,
            "2008" => VHDL2008,
            "2019" => VHDL2019,
            _ => return Err(()),
        })
    }
}

impl AsRef<str> for VhdlVersion {
    fn as_ref(&self) -> &str {
        use VhdlVersion::*;
        match self {
            VHDL1993 => "1993",
            VHDL2008 => "2008",
            VHDL2019 => "2019",
        }
    }
}
