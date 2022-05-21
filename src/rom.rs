pub struct Rom {
    pub cart: Vec<u8>
}

impl Rom {
    pub fn new(cart: Vec<u8>) -> Self { Self { cart } }
    pub fn load(&self, addr: u32) -> u8 {
        let off = self.addr_to_offset(addr);
        self.cart[off]
    }
    pub fn load_u16(&self, addr: u32) -> u16 {
        let off = self.addr_to_offset(addr);
        u16::from_le_bytes([self.cart[off], self.cart[off+1]])
    }
    pub fn load_u24(&self, addr: u32) -> u32 {
        let off = self.addr_to_offset(addr);
        u32::from_le_bytes([self.cart[off], self.cart[off+1], self.cart[off+2], 0])
    }
    fn addr_to_offset(&self, addr: u32) -> usize {
        let bank = addr >> 16;
        let addr = addr & 0x7FFF;
        (bank << 15 | addr) as _
    }
}

impl std::ops::Deref for Rom {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        &self.cart
    }
}
