//! A compressed binary radix set. Arena links keep traversal and destruction stack-safe.

use alloc::vec::Vec;

/// Keys are prefix-free; byte access is constant time. Typed keys include their length before their payload.
pub(crate) trait Key {
    fn len(&self) -> usize;
    fn byte(&self, index: usize) -> u8;
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct Work {
    pub bytes: usize,
    pub branches: usize,
}

impl Work {
    fn plus(self, other: Self) -> Self {
        Self { bytes: self.bytes.saturating_add(other.bytes), branches: self.branches.saturating_add(other.branches) }
    }
}

// A byte position and an offset avoid overflowing a machine-sized bit index.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Bit {
    byte: usize,
    offset: u32,
}

impl Bit {
    fn right(self, key: &impl Key) -> bool {
        key.byte(self.byte) & (128u8 >> self.offset) != 0
    }
}

enum Node<'a, K> {
    Leaf(&'a K),
    Branch { bit: Bit, left: usize, right: usize, representative: &'a K },
}

impl<'a, K> Node<'a, K> {
    fn representative(&self) -> &'a K {
        match self {
            Self::Leaf(key) | Self::Branch { representative: key, .. } => key,
        }
    }
}

#[derive(Debug)]
pub(crate) struct InvalidIndex;

pub(crate) struct Index<'a, K> {
    nodes: Vec<Node<'a, K>>,
    root: Option<usize>,
}

fn differing(a: &impl Key, b: &impl Key) -> (Option<Bit>, Work) {
    let end = a.len().min(b.len());
    for i in 0..end {
        let xor = a.byte(i) ^ b.byte(i);
        if xor != 0 {
            return (
                Some(Bit { byte: i, offset: xor.leading_zeros() }),
                Work { bytes: i.saturating_add(1).saturating_mul(2), branches: 0 },
            );
        }
    }
    (None, Work { bytes: end.saturating_mul(2), branches: 0 })
}

impl<'a, K: Key> Index<'a, K> {
    pub fn new() -> Self {
        Self { nodes: Vec::new(), root: None }
    }

    fn node(&self, index: usize) -> Result<&Node<'a, K>, InvalidIndex> {
        self.nodes.get(index).ok_or(InvalidIndex)
    }

    fn candidate(&self, key: &K, mut index: usize) -> Result<(&'a K, Work), InvalidIndex> {
        let mut work = Work::default();
        loop {
            match self.node(index)? {
                Node::Leaf(candidate) => return Ok((candidate, work)),
                Node::Branch { bit, left, right, representative } => {
                    work.branches = work.branches.saturating_add(1);
                    if bit.byte >= key.len() {
                        return Ok((representative, work));
                    }
                    work.bytes = work.bytes.saturating_add(1);
                    index = if bit.right(key) { *right } else { *left };
                }
            }
        }
    }

    pub fn contains(&self, key: &K) -> Result<(bool, Work), InvalidIndex> {
        let Some(root) = self.root else { return Ok((false, Work::default())) };
        let (candidate, walked) = self.candidate(key, root)?;
        let (difference, compared) = differing(key, candidate);
        Ok((difference.is_none(), walked.plus(compared)))
    }

    pub fn add(&mut self, key: &'a K) -> Result<(bool, Work), InvalidIndex> {
        let Some(root) = self.root else {
            self.root = Some(self.nodes.len());
            self.nodes.push(Node::Leaf(key));
            return Ok((true, Work::default()));
        };
        let (candidate, walked) = self.candidate(key, root)?;
        let (difference, compared) = differing(key, candidate);
        let mut work = walked.plus(compared);
        let Some(bit) = difference else { return Ok((false, work)) };
        let mut index = root;
        let mut parent = None;
        while let Node::Branch { bit: at, left, right, .. } = self.node(index)? {
            if *at >= bit {
                break;
            }
            let goes_right = at.right(key);
            work = work.plus(Work { bytes: 1, branches: 1 });
            parent = Some((index, goes_right));
            index = if goes_right { *right } else { *left };
        }
        let representative = self.node(index)?.representative();
        let leaf = self.nodes.len();
        self.nodes.push(Node::Leaf(key));
        let split = self.nodes.len();
        let branch = if bit.right(key) {
            Node::Branch { bit, left: index, right: leaf, representative }
        } else {
            Node::Branch { bit, left: leaf, right: index, representative: key }
        };
        self.nodes.push(branch);
        match parent {
            None => self.root = Some(split),
            Some((p, goes_right)) => match self.nodes.get_mut(p).ok_or(InvalidIndex)? {
                Node::Branch { left, right, .. } => {
                    if goes_right {
                        *right = split;
                    } else {
                        *left = split;
                    }
                }
                Node::Leaf(_) => return Err(InvalidIndex),
            },
        }
        Ok((true, work.plus(Work { bytes: 1, branches: 0 })))
    }
}

#[cfg(test)]
mod tests {
    #![allow(
        clippy::panic_in_result_fn,
        reason = "test assertions report failures; Result propagates unexpected index errors"
    )]
    use super::{Index, InvalidIndex, Key, Work};
    use alloc::{vec, vec::Vec};

    struct Bytes(Vec<u8>);
    impl Key for Bytes {
        fn len(&self) -> usize {
            self.0.len()
        }
        fn byte(&self, index: usize) -> u8 {
            self.0.get(index).copied().unwrap_or(0)
        }
    }
    fn key(kind: u8, bytes: &[u8]) -> Bytes {
        let mut encoded = u64::try_from(bytes.len()).unwrap_or(u64::MAX).to_be_bytes().to_vec();
        encoded.push(kind);
        encoded.extend_from_slice(bytes);
        Bytes(encoded)
    }

    #[test]
    fn exact_empty_kind_and_zero_counts() -> Result<(), InvalidIndex> {
        let a = key(0, &[]);
        let b = key(1, &[]);
        let zero = key(0, &[0]);
        let mut index = Index::new();
        assert_eq!(index.add(&a)?, (true, Work::default()));
        assert_eq!(index.add(&a)?, (false, Work { bytes: 18, branches: 0 }));
        assert_eq!(index.add(&b)?, (true, Work { bytes: 19, branches: 0 }));
        assert_eq!(index.contains(&b)?, (true, Work { bytes: 19, branches: 1 }));
        assert!(!index.contains(&zero)?.0);
        assert!(index.add(&zero)?.0);
        assert!(index.contains(&zero)?.0);
        Ok(())
    }

    #[test]
    fn short_probe_stops_before_long_suffix() -> Result<(), InvalidIndex> {
        let keys: Vec<_> = (0u8..=255)
            .map(|n| {
                let mut bytes = vec![0; 4096];
                bytes.push(n);
                key(4, &bytes)
            })
            .collect();
        let short = key(4, &[]);
        let mut index = Index::new();
        for key in &keys {
            assert!(index.add(key)?.0);
        }
        let (found, work) = index.contains(&short)?;
        assert!(!found);
        assert_eq!(work, Work { bytes: 14, branches: 1 });
        assert_eq!(index.add(&short)?, (true, Work { bytes: 15, branches: 1 }));
        assert!(index.contains(&short)?.0);
        for key in &keys {
            assert!(index.contains(key)?.0);
        }
        Ok(())
    }

    #[test]
    fn exact_membership_and_duplicates_with_linear_work() -> Result<(), InvalidIndex> {
        let keys: Vec<_> = (0u16..1024).map(|n| key(4, &n.to_be_bytes())).collect();
        for reverse in [false, true] {
            let mut ordered: Vec<_> = keys.iter().collect();
            if reverse {
                ordered.reverse();
            }
            let mut index = Index::new();
            for key in ordered {
                let (added, work) = index.add(key)?;
                assert!(added);
                assert!(work.bytes <= key.len().saturating_mul(18).saturating_add(1));
                assert!(work.branches <= key.len().saturating_mul(16).saturating_add(1));
            }
            for key in &keys {
                let (found, work) = index.contains(key)?;
                assert!(found);
                assert!(work.bytes <= key.len().saturating_mul(10));
                assert!(work.branches <= key.len().saturating_mul(8).saturating_add(1));
                assert!(!index.add(key)?.0);
            }
            assert!(!index.contains(&key(4, &1024u16.to_be_bytes()))?.0);
        }
        Ok(())
    }
}
