use crate::util::fmt::tab;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
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

    pub fn guaranteed_neq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Free(a), Self::Free(b)) => a != b,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Prop {
    Trivial(bool),
    Not(Rc<Prop>),
    Or(Rc<Prop>, Rc<Prop>),
    Is(Term, Term),
}

// TODO: This could be replaced by a TermForest, also removing the need for prop equality checks.
#[derive(Debug, Clone, Default)]
pub struct Tableaux {
    head: usize,
    set: HashMap<Rc<Prop>, usize>,
    props: Vec<Option<Rc<Prop>>>,
    depth: u32,
}

impl Tableaux {
    pub fn new() -> Self {
        Self::default()
    }

    fn save(&self) -> usize {
        self.head
    }

    fn restore(&mut self, head: usize) {
        self.head = head;
    }

    pub fn fork<F, R>(&mut self, mut fn_: F) -> R
    where
        F: FnMut(&mut Self) -> R,
    {
        let state = self.save();
        println!("{}{{", tab(self.depth));
        self.depth += 1;
        let ret = fn_(self);
        self.depth -= 1;
        println!("{}}}", tab(self.depth));
        self.restore(state);
        ret
    }

    pub fn propose(&mut self, prop: Rc<Prop>) -> Result<(), ContradictionError> {
        use Prop::*;

        println!("{}{:?}", tab(self.depth), prop);

        // Check if we have this proposition in duplicate
        if self.contains(&prop) {
            // That previous instance of this proposition didn't conflict with anything and
            // contradictory propositions are never added to the table so this proposition won't
            // contradict anything.
            return Ok(());
        }

        // Check if the proposition conflicts with other propositions directly.
        match &*prop {
            Trivial(true) => { /* never conflicts with anything */ }
            Trivial(false) => {
                // Trivially false is always false, however.
                return Err(ContradictionError);
            }
            Is(a, b) => {
                if a.guaranteed_neq(b) {
                    return Err(ContradictionError);
                }

                if self.contains(&not(prop.clone())) {
                    return Err(ContradictionError);
                }
            }
            Or(_, _) => {
                if self.contains(&not(prop.clone())) {
                    return Err(ContradictionError);
                }
            }
            Not(negative) => match &**negative {
                Trivial(true) => {
                    // Trivially not true (i.e. false) is always false.
                    return Err(ContradictionError);
                }
                Trivial(false) => { /* Trivially not false is always true. */ }
                Is(a, b) => {
                    if a == b {
                        return Err(ContradictionError);
                    }

                    if self.contains(negative) {
                        return Err(ContradictionError);
                    }
                }
                Or(_, _) | Not(_) => {
                    if self.contains(negative) {
                        return Err(ContradictionError);
                    }
                }
            },
        }

        //> Introduce the new proposition

        // First, remove the previous proposition at this index so that it doesn't suddenly become
        // included in the tableaux.
        let slot = if let Some(slot) = self.props.get_mut(self.head) {
            let removed = slot.take().unwrap();
            self.set.remove(&removed);
            slot
        } else {
            self.props.push(None);
            &mut self.props[self.head]
        };

        // Update the list and the set
        *slot = Some(prop.clone());
        self.set.insert(prop, self.head);

        // Finally, move the head
        self.head += 1;

        Ok(())
    }

    pub fn contains(&self, prop: &Rc<Prop>) -> bool {
        match self.set.get(prop) {
            Some(id) => *id < self.head,
            None => false,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ContradictionError;

impl Error for ContradictionError {}

impl Display for ContradictionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "proposal caused a contradiction")
    }
}

pub fn evaluate_prop(prop: Rc<Prop>) -> bool {
    fn can_refute(
        tableaux: &mut Tableaux,
        initial_heads: Vec<Rc<Prop>>,
        // TODO: Optimize terminals data structure
        mut terminals: Vec<Rc<Prop>>,
    ) -> bool {
        use Prop::*;

        let mut dirty_heads = initial_heads;
        let mut dirty_heads_next = Vec::new();

        // Expand non-terminals
        // TODO: Expansion limits (prop checking is only semi-decidable)
        while !dirty_heads.is_empty() {
            for head in dirty_heads.drain(..) {
                match &*head {
                    Trivial(_) => { /* cannot be expanded */ }
                    Not(neg) => match &**neg {
                        Trivial(_) => { /* cannot be expanded */ }
                        Not(next) => {
                            // Handle double negation implication
                            if tableaux.propose(next.clone()).is_err() {
                                return true;
                            }
                            dirty_heads_next.push(next.clone());
                        }
                        Or(left, right) => {
                            let (neg_left, neg_right) = (not(left.clone()), not(right.clone()));

                            // Propose the negation of both operands
                            if tableaux.propose(neg_left.clone()).is_err() {
                                return true;
                            }

                            if tableaux.propose(neg_right.clone()).is_err() {
                                return true;
                            }

                            dirty_heads_next.push(neg_left);
                            dirty_heads_next.push(neg_right);
                        }
                        Is(_, _) => todo!(),
                    },
                    Or(_, _) => terminals.push(head),
                    Is(_, _) => todo!(),
                }
            }

            // TODO: do we need double buffering?
            swap(&mut dirty_heads, &mut dirty_heads_next);
        }

        // Expand terminals
        if let Some(terminal) = terminals.pop() {
            match &*terminal {
                Or(left, right) => {
                    // Assume left only case.
                    if !tableaux.fork(|tableaux| {
                        let neg_right = not(right.clone());

                        if tableaux.propose(left.clone()).is_err() {
                            return true;
                        }

                        if tableaux.propose(neg_right.clone()).is_err() {
                            return true;
                        }

                        can_refute(tableaux, vec![left.clone(), neg_right], terminals.clone())
                    }) {
                        return false;
                    }

                    // Assume right only case.
                    if !tableaux.fork(|tableaux| {
                        let neg_left = not(left.clone());

                        if tableaux.propose(neg_left.clone()).is_err() {
                            return true;
                        }

                        if tableaux.propose(right.clone()).is_err() {
                            return true;
                        }

                        can_refute(tableaux, vec![neg_left, right.clone()], terminals.clone())
                    }) {
                        return false;
                    }

                    // Assume both case.
                    if !tableaux.fork(|tableaux| {
                        if tableaux.propose(left.clone()).is_err() {
                            return true;
                        }

                        if tableaux.propose(right.clone()).is_err() {
                            return true;
                        }

                        can_refute(
                            tableaux,
                            vec![left.clone(), right.clone()],
                            terminals.clone(),
                        )
                    }) {
                        // We failed to refute this case.
                        return false;
                    }
                }
                _ => unreachable!(),
            }

            // All branches successfully refuted the proposition. Thus, this proposition cannot be
            // satisfied.
            true
        } else {
            // No non-terminal props could be expanded to disprove this branch.
            // Thus, this proposition is irrefutable.
            false
        }
    }

    let mut tableaux = Tableaux::new();
    let prop = not(prop);
    tableaux.propose(prop.clone()).unwrap();
    can_refute(&mut tableaux, vec![prop], Vec::new())
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
    dbg!(evaluate_prop(iff(iff(bot(), bot()), nand(bot(), taut()))));
}
