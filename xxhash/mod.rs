use self::ll::{State32};

mod ll;

/// Represents the xxhash32 state
#[deriving(ToStr)]
pub struct Xxh32 {
    priv state: State32
}

impl Xxh32 {
    /// Creates a new Xxh32
    /// # Arguments
    /// * `seed` - seed to use for xxhash32
    pub fn new(seed: u32) -> Xxh32 {
        Xxh32 {
            state: State32::new(seed)
        }
    }

    /// Creates a new Xxh32 and feeds it input immediately
    /// # Arguments
    /// * `seed` - seed to use for xxhash32
    pub fn new_with_input(input: &[u8], seed: u32) -> Xxh32 {
        let mut x = Xxh32::new(seed);
        ll::update(&mut x.state, input.as_imm_buf(|buf, _| buf), input.len());
        x
    }

    /// Feeds more data into the digest context
    /// # Arguments
    /// * `input` - data to feed
    /// # Return value
    /// if `true` update was successful
    pub fn update(&mut self, input: &[u8]) -> bool {
        do input.as_imm_buf |buf, buf_len| {
            ll::Okay == ll::update(&mut self.state, buf, buf_len)
        }
    }

    /// Generates the hash, but preserves the state, such that
    ///  further calls to `digest` and `update` are possible
    pub fn digest(&mut self) -> u32 {
        ll::intermediate_digest(&mut self.state)
    }

    /// Resets the XXh32 struct to blank state, with a new seeds
    /// # Arguments
    /// * `seed` - The new seed
    pub fn reset(&mut self, seed: u32) {
        ll::reset_state(&mut self.state, seed);
    }

    /// Hashes the entire slice in one shot
    /// # Arguments
    /// * `input` - the slice to be hashed
    /// * `seed` - the seed to be used in the hashing
    pub fn one_shot(input: &[u8], seed: u32) -> u32 {
        do input.as_imm_buf |buf, buf_len| {
            ll::xxh32(buf, buf_len, seed)
        }
    }
}



#[cfg(test)]
mod test {
    use super::{Xxh32};
    use std::rand;
    use std::rand::Rng;
    use extra::test::BenchHarness;

    static PRIME: u32 = 2654435761u32;
    static SAN_BUF_SIZE: uint = 101;

    fn test_sequence(sentence: &[u8], seed: u32, solution: u32) {
       let answer1 = Xxh32::one_shot(sentence, seed);

       let mut h = Xxh32::new(seed);
        h.update(sentence);
        let answer2 = h.digest();

        let mut h = Xxh32::new(seed);
        for x in sentence.iter().map(|x| *x) {
            h.update(&[x]);
        }
        let answer3 = h.digest();

        debug!("[HEX] Answer 1: 0x{:X}, Answer 2: 0x{:X}, Answer 3: 0x{:X}, Solution: 0x{:X}", answer1, answer2, answer3, solution);
        debug!("[DECIMAL] Answer 1: {:u}, Answer 2: {:u}, Answer 3: {:u}, Solution: {:u}", answer1, answer2, answer3, solution);

        assert!(answer1 == answer2 && answer2 == answer3);
        assert_eq!(answer1, solution);
        assert_eq!(answer2, solution);
        assert_eq!(answer3, solution);
    }

    #[test]
    fn test_correct() {
        let mut sanity = ~[0u8, ..SAN_BUF_SIZE];
        let mut random = PRIME;

        for byte in sanity.mut_iter() {
            *byte = (random >> 24) as u8;
            random *= random;
        }

        debug!("{}:{}: Sanity: {}", file!(), line!(), sanity.to_str());

        test_sequence(sanity, 0, 0x1F1AA412);
        test_sequence(sanity, PRIME, 0x498EC8E2);
    }

    #[bench]
    fn bench_hash_rand(b: &mut BenchHarness) {
        let mut rng = rand::rng();
        let data = rng.gen_vec(1024 * 32);
        let mut h = Xxh32::new(PRIME);

        do b.iter {
            h.update(data);
            h.digest();
            h.reset(PRIME);
        }

        b.bytes = 1024 * 32;
    }
}

