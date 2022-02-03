//! Bare Crew type-checking is founded off [first-order logic](fol) (FOL). This module implements an
//! automated FOL theorem prover which takes in a binary proposition and returns whether it's a
//! tautology.
//!
//! ## Representing Propositions
//!
//! TODO
//!
//! ## Analytic Tableaux
//!
//! Theorem checking in Crew is done using a variant of the [analytic tableaux] method.
//! TODO
//!
//! ## Quantification Semantics
//!
//! TODO
//!
//! ## Term Tableaux and Forking
//!
//! TODO
//!
//! ## Semi-Decidability
//!
//! Automated FOL proving is only [semidecidable], meaning that while all tautological propositions
//! can be proven as such, non-tautological propositions may either definitively halt with a rejection
//! or type check forever. Because the evaluation of non-tautological propositions is typically an
//! error, proposition evaluation is time limited to a per-module-item constant number of "steps".
//!
//! [analytic tableaux]: https://en.wikipedia.org/wiki/Method_of_analytic_tableaux
//! [fol]: https://en.wikipedia.org/wiki/First-order_logic
//! [semidecidable]: https://en.wikipedia.org/wiki/Decidability_(logic)#Semidecidability

use crate::util::disjoint_set::DisjointSet;
use crate::util::fmt::tab;
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

#[derive(Debug, Clone)]
pub enum Prop {
    Trivial(bool),
    Not(Rc<Prop>),
    Or(Rc<Prop>, Rc<Prop>),
    Is(Term, Term),
}

impl Prop {
    pub fn guaranteed_malformed(&self) -> bool {
        use Prop::*;

        match self {
            Trivial(false) => true,
            Is(Term::Concrete(a), Term::Concrete(b)) => a != b,
            Not(prop) => match &**prop {
                Trivial(true) => true,
                Is(a, b) => a == b,
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct TermTableaux {
    sets: DisjointSet<Term>,
    negatives: Vec<(usize, usize)>,
}

impl TermTableaux {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn propose_pos(&mut self, left: Term, right: Term) {
        let left_id = self.sets.push(left);
        let right_id = self.sets.push(right);
        self.sets.raw_mut().union(left_id, right_id);
    }

    pub fn propose_neg(&mut self, left: Term, right: Term) {
        let left_id = self.sets.push(left);
        let right_id = self.sets.push(right);
        self.negatives.push((left_id, right_id));
    }

    pub fn validate(&mut self) -> bool {
        for (left, right) in self.negatives.iter().copied() {
            if self.sets.raw_mut().are_siblings(left, right) {
                return false;
            }
        }

        true
    }
}

// TODO: optimize forking; support recursive propositions; better justification paths
pub fn evaluate_prop(prop: Rc<Prop>) -> bool {
    fn can_refute(
        mut tableaux: TermTableaux,
        initial_heads: Vec<Rc<Prop>>,
        mut terminals: Vec<Rc<Prop>>,
        log_depth: u32,
    ) -> bool {
        use Prop::*;

        let mut dirty_heads = initial_heads;

        // Expand non-terminals
        while let Some(head) = dirty_heads.pop() {
            log::trace!("{}{:?}", tab(log_depth), head);

            if head.guaranteed_malformed() {
                // We found a contradiction
                return true;
            }

            match &*head {
                Trivial(_) => { /* cannot be expanded */ }
                Not(neg_head) => match &**neg_head {
                    Trivial(_) => { /* cannot be expanded */ }
                    Not(next) => {
                        // Handle double negation implication
                        dirty_heads.push(next.clone());
                    }
                    Or(left, right) => {
                        let (neg_left, neg_right) = (not(left.clone()), not(right.clone()));
                        dirty_heads.push(neg_left);
                        dirty_heads.push(neg_right);
                    }
                    Is(left, right) => {
                        tableaux.propose_neg(*left, *right);
                    }
                },
                Or(_, _) => terminals.push(head),
                Is(left, right) => {
                    tableaux.propose_pos(*left, *right);
                }
            }
        }

        // Expand terminals
        if let Some(terminal) = terminals.pop() {
            match &*terminal {
                Or(left, right) => {
                    // Assume left only case.
                    if !can_refute(
                        tableaux.clone(),
                        vec![left.clone(), not(right.clone())],
                        terminals.clone(),
                        log_depth + 1,
                    ) {
                        return false;
                    }

                    // Assume right only case.
                    if !can_refute(
                        tableaux.clone(),
                        vec![not(left.clone()), right.clone()],
                        terminals.clone(),
                        log_depth + 1,
                    ) {
                        return false;
                    }

                    // Assume both case.
                    if !can_refute(
                        tableaux.clone(),
                        vec![left.clone(), right.clone()],
                        terminals.clone(),
                        log_depth + 1,
                    ) {
                        return false;
                    }
                }
                _ => unreachable!(),
            }

            // All branches successfully refuted the proposition. Thus, this proposition cannot be
            // satisfied.
            true
        } else {
            // We've expanded all the possible propositions. Time to check if they're valid.
            if !tableaux.validate() {
                // We found a contradiction to refute this proposition.
                return true;
            }

            // No non-terminal props could be expanded to disprove this branch.
            // Thus, this proposition is irrefutable.
            false
        }
    }

    log::trace!("Evaluating {:?}", prop);
    can_refute(TermTableaux::new(), vec![not(prop)], Vec::new(), 1)
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
    env_logger::init();

    let a = Term::new_free();
    let b = Term::new_free();
    let c = Term::new_free();

    assert_eq!(true, evaluate_prop(or(taut(), bot())));
    assert_eq!(false, evaluate_prop(and(taut(), bot())));
    assert_eq!(true, evaluate_prop(iff(is(a, b), is(b, a))));
    assert_eq!(
        true,
        evaluate_prop(implies(and(is(a, b), is(b, c)), is(a, c)))
    );
    assert_eq!(false, evaluate_prop(iff(and(is(a, b), is(b, c)), is(a, c))));

    // This fails because not all choices of a, b, and c will result in this equivalence relationship.
    assert_eq!(
        false,
        evaluate_prop(implies(and(not(is(a, b)), not(is(b, c))), not(is(a, c)))),
    );

    // This fails because not all choices of a, b, and c will result in this non-equivalence relationship.
    // Because of the way in which propositions are qualified, the negation of a tautological proposition
    // is not always unsatisfiable and vice-versa.
    assert_eq!(
        false,
        evaluate_prop(not(implies(
            and(not(is(a, b)), not(is(b, c))),
            not(is(a, c))
        ))),
    );
}
