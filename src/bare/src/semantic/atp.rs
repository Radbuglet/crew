use std::mem::swap;
use std::rc::Rc;

// === Core === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum Term {
    Free(u64),
    Concrete(u64),
}

impl Term {
    fn new_term_id() -> u64 {
        use std::sync::atomic::{AtomicU64, Ordering};
        static ID: AtomicU64 = AtomicU64::new(0);
        ID.fetch_add(1, Ordering::Relaxed)
    }

    pub fn new_free() -> Self {
        Self::Free(Self::new_term_id())
    }

    pub fn new_concrete() -> Self {
        Self::Concrete(Self::new_term_id())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Prop {
    Trivial(bool),
    Not(Rc<Prop>),
    Or(Rc<Prop>, Rc<Prop>),
    Is(Term, Term),
}

// TODO: Check for recursive formulae; improve contradiction searching algorithm
// TODO: Implement a bump database with interning
// TODO: We might need actual quantifiers to handle nested quantification
pub fn evaluate_prop(prop: Rc<Prop>) -> bool {
    fn can_refute(implications: &mut Vec<Rc<Prop>>, initial_heads: Vec<usize>) -> bool {
        use Prop::*;

        let mut dirty_heads = initial_heads;
        let mut dirty_heads_next = Vec::new();

        loop {
            println!("{:?}", implications);

            // Expand everything we can.
            for head in dirty_heads.iter().copied() {
                let head = implications[head].clone();

                // Check if this implication leads to a contradiction.
                // Note: we do this while expanding because contradictions are symmetric.
                // TODO: This should just be part of a Tableaux abstraction and is probably broken.
                for implication in implications.iter() {
                    if &*head == &Not(implication.clone()) {
                        // Refutation completed!
                        return true;
                    }

                    if &Not(head.clone()) == &**implication {
                        // Refutation completed!
                        return true;
                    }
                }

                // Expand all direct implications.
                match &*head {
                    // Handle truthy propositions
                    Trivial(true) => { /* cannot be expanded further */ }
                    Trivial(false) => {
                        // Implication of a falsehood constitutes refutation.
                        return true;
                    }
                    Or(left, right) => {
                        // Check left-only implication
                        {
                            let restore_to_len = implications.len();
                            implications.push(left.clone());
                            implications.push(Not(right.clone()).into());

                            if !can_refute(implications, vec![restore_to_len, restore_to_len + 1]) {
                                return false;
                            }
                            implications.truncate(restore_to_len);
                        }

                        // Check right-only implication
                        {
                            let restore_to_len = implications.len();
                            implications.push(Not(left.clone()).into());
                            implications.push(right.clone());

                            if !can_refute(implications, vec![restore_to_len, restore_to_len + 1]) {
                                return false;
                            }
                            implications.truncate(restore_to_len);
                        }

                        // Check both implication
                        {
                            let restore_to_len = implications.len();
                            implications.push(left.clone().into());
                            implications.push(right.clone());

                            if !can_refute(implications, vec![restore_to_len, restore_to_len + 1]) {
                                return false;
                            }
                            implications.truncate(restore_to_len);
                        }

                        // All cases have been refuted.
                        return true;
                    }
                    Is(_, _) => todo!(),

                    // Handle negative propositions
                    Not(neg) => match &**neg {
                        Not(inner) => {
                            // Expand double negation.
                            dirty_heads_next.push(implications.len());
                            implications.push(inner.clone());
                        }
                        Trivial(true) => {
                            // Expand double negation.
                            dirty_heads_next.push(implications.len());
                            implications.push(Trivial(false).into());
                        }
                        Trivial(false) => {
                            // Expand double negation.
                            dirty_heads_next.push(implications.len());
                            implications.push(Trivial(true).into());
                        }
                        Or(left, right) => {
                            // Push negative implications of both branches.
                            dirty_heads_next.push(implications.len());
                            implications.push(Not(left.clone()).into());
                            dirty_heads_next.push(implications.len());
                            implications.push(Not(right.clone()).into());
                        }
                        Is(_, _) => todo!(),
                    },
                }
            }

            // Are we at a dead-end?
            if dirty_heads.is_empty() {
                // If so, we failed to refute the proposition.
                return false;
            }

            // Otherwise, swap the queues.
            dirty_heads.clear();
            swap(&mut dirty_heads, &mut dirty_heads_next);
        }
    }

    can_refute(&mut vec![Prop::Not(prop).into()], vec![0])
}

// === Rewrite constructors === //

pub fn trivial(value: bool) -> Rc<Prop> {
    Prop::Trivial(value).into()
}

pub fn taut() -> Rc<Prop> {
    trivial(true)
}

pub fn bot() -> Rc<Prop> {
    trivial(false)
}

pub fn not(val: Rc<Prop>) -> Rc<Prop> {
    Prop::Not(val).into()
}

pub fn or(left: Rc<Prop>, right: Rc<Prop>) -> Rc<Prop> {
    Prop::Or(left, right).into()
}

pub fn nor(left: Rc<Prop>, right: Rc<Prop>) -> Rc<Prop> {
    not(or(left, right))
}

pub fn is(left: Term, right: Term) -> Rc<Prop> {
    Prop::Is(left, right).into()
}

pub fn nand(left: Rc<Prop>, right: Rc<Prop>) -> Rc<Prop> {
    or(not(left), not(right))
}

pub fn and(left: Rc<Prop>, right: Rc<Prop>) -> Rc<Prop> {
    not(nand(left, right))
}

pub fn implies(a: Rc<Prop>, b: Rc<Prop>) -> Rc<Prop> {
    or(not(a), b)
}

pub fn iff(a: Rc<Prop>, b: Rc<Prop>) -> Rc<Prop> {
    and(implies(a.clone(), b.clone()), implies(b, a))
}

#[test]
fn basic_impl() {
    dbg!(evaluate_prop(iff(taut(), taut())));
}
