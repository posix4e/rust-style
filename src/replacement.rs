pub struct Replacement {
    pub offset: u32,
    pub length: u32,
    pub text: String,
}

impl Replacement {
    fn new(offset: u32, length: u32, text: String) -> Replacement {
        Replacement {
            offset: offset,
            length: length,
            text: text,
        }
    }
}
