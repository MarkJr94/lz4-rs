use self::ll::{State32};
// use self::ll;

mod ll;

pub struct Xxh32 {
    state: State32
}

impl Xxh32 {
    pub fn new(seed: u32) -> Xxh32 {
        Xxh32 {
            state: State32::new(seed)
        }
    }

    pub fn new_with_input(input: &[u8], seed: u32) -> Xxh32 {
        let mut x = Xxh32::new(seed);
        ll::update(&mut x.state, input.as_imm_buf(|buf, _| buf), input.len());
        x
    }

    pub fn update(&mut self, input: &[u8]) -> bool {
        ll::Okay == ll::update(&mut self.state, input.as_imm_buf(|buf, _| buf), input.len())
    }

    pub fn digest(&mut self) -> u32 {
        ll::intermediate_digest(&mut self.state)
    }

    pub fn reset(&mut self, seed: u32) {
        ll::reset_state(&mut self.state, seed);
    }
}

#[cfg(test)]
mod test {
    use super::Xxh32;
    use std::rand;

    static PRIME: u32 = 2654435761u32;
    static SAN_BUF_SIZE: uint = 101;

    fn test_sequence(sentence: &[u8], seed: u32, solution: u32) {
        let mut h = Xxh32::new(seed);
        h.update(sentence);
        let answer1 = h.digest();

        println!("{}", sentence.len());

        let mut h = Xxh32::new_with_input(sentence, seed);
        let answer2 = h.digest();


        println!("[HEX] Answer 1: 0x{:X}, Answer 2: 0x{:X}, Solution: 0x{:X}", answer1, answer2, solution);
        println!("[DECIMAL] Answer 1: {:u}, Answer 2: {:u}, Solution: {:u}", answer1, answer2, solution);

        assert_eq!(answer1, answer2);
        assert_eq!(answer1, solution);
        assert_eq!(answer2, solution);
    }

    #[test]
    fn test_correct() {
        let mut sanity = ~[0u8, ..SAN_BUF_SIZE];
        let mut random = PRIME;

        for byte in sanity.mut_iter() {
            *byte = (random >> 24) as u8;
            random *= random;
        }

        test_sequence(sanity, 0, 0x1F1AA412);
        test_sequence(sanity, PRIME, 0x498EC8E2);
    }
}