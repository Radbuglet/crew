use std::collections::HashMap;
use std::hash::Hash;
use std::mem::swap;

/// A [Disjoint-set data structure](dsds), which enables amortized O(1) unioning of sets.
/// This is useful for implementing transitive term equality checks in the automated theorem prover.
///
/// [dsds]: https://en.wikipedia.org/wiki/Disjoint-set_data_structure
#[derive(Debug, Clone, Eq, PartialEq, Hash, Default)]
pub struct RawDisjointSet {
    nodes: Vec<Node>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Node {
    // Either an acyclic parent node reference or the node's own index.
    parent: usize,
    // Stores the number of descendant nodes when it is the root node of a set.
    size: usize,
}

impl RawDisjointSet {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push(&mut self) -> usize {
        let own_id = self.nodes.len();
        self.nodes.push(Node {
            parent: own_id,
            size: 1,
        });
        own_id
    }

    pub fn find_root(&mut self, mut id: usize) -> usize {
        loop {
            let parent = self.nodes[id].parent;
            if id == parent {
                break id;
            }
            id = parent;
            self.nodes[id].parent = self.nodes[parent].parent;
        }
    }

    pub fn size_of_root(&self, id: usize) -> usize {
        let node = &self.nodes[id];
        debug_assert!(node.parent == id);
        node.size
    }

    pub fn size_of_set(&mut self, id: usize) -> usize {
        let root = self.find_root(id);
        self.size_of_root(root)
    }

    pub fn union(&mut self, a: usize, b: usize) {
        // Find set roots
        let mut root_a = self.find_root(a);
        let mut root_b = self.find_root(b);

        // If the roots are the same, no-op.
        if root_a == root_b {
            return;
        }

        // Find the node states

        // Ensure that a.size >= b.size
        if self.nodes[root_a].size < self.nodes[root_b].size {
            swap(&mut root_a, &mut root_b);
        }

        // Parent "b" to "a" and increment size
        self.nodes[root_b].parent = root_a;
        self.nodes[root_a].size += self.nodes[root_b].size;
    }

    pub fn are_siblings(&mut self, a: usize, b: usize) -> bool {
        self.find_root(a) == self.find_root(b)
    }
}

#[derive(Debug, Clone)]
pub struct DisjointSet<T> {
    indices: HashMap<T, usize>,
    raw: RawDisjointSet,
}

impl<T> Default for DisjointSet<T> {
    fn default() -> Self {
        Self {
            indices: Default::default(),
            raw: Default::default(),
        }
    }
}

impl<T: Hash + Eq> DisjointSet<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn raw(&self) -> &RawDisjointSet {
        &self.raw
    }

    pub fn raw_mut(&mut self) -> &mut RawDisjointSet {
        &mut self.raw
    }

    pub fn push(&mut self, source: T) -> usize {
        *self
            .indices
            .entry(source)
            .or_insert_with(|| self.raw.push())
    }

    pub fn connect(&mut self, left: &T, right: &T) {
        self.raw.union(self.indices[left], self.indices[right])
    }

    pub fn connect_raw(&mut self, left: usize, right: usize) {
        self.raw.union(left, right)
    }

    pub fn are_connected(&mut self, left: &T, right: &T) -> bool {
        if let (Some(left), Some(right)) = (
            self.indices.get(left).copied(),
            self.indices.get(right).copied(),
        ) {
            self.raw.are_siblings(left, right)
        } else {
            false
        }
    }

    pub fn are_connected_raw(&mut self, left: usize, right: usize) -> bool {
        self.raw.are_siblings(left, right)
    }
}

#[test]
fn disjoint_set_test_1() {
    let mut set = DisjointSet::new();
    set.push("A");
    set.push("B");
    set.push("C");
    set.push("D");

    set.connect(&"A", &"B");
    set.connect(&"B", &"C");
    assert!(set.are_connected(&"A", &"C"));
    assert!(!set.are_connected(&"B", &"D"));
}
