module HW08 where

data False : Set where

not : Set → Set
not P = P → False

data _∨_ (P Q : Set) : Set where
  injₗ : (p : P) → P ∨ Q
  injᵣ : (q : Q) → P ∨ Q

data _∧_ (P Q : Set) : Set where
  _×_ : (p : P) → (q : Q) → P ∧ Q

_↔_ : (P Q : Set) → Set
P ↔ Q = (P → Q) ∧ (Q → P)

postulate
  excluded-middle : (P : Set) → P ∨ (not P)

exfalso : {P : Set} → False → P
exfalso ()

double-negation : {P : Set} → P ↔ not (not P)
double-negation {P} = (λ p ¬p → ¬p p) × prf
  where
  prf : not (not P) → P
  prf ¬¬p with excluded-middle P
  prf ¬¬p | injₗ p = p
  prf ¬¬p | injᵣ ¬p = exfalso (¬¬p ¬p)

modus-ponens : {P Q : Set} → (P → Q) → P → Q
modus-ponens f x = f x

modus-tollens : {P Q : Set} → (P → Q) → not Q → not P
modus-tollens f ¬q p = ¬q (f p)

material-implication : {P Q : Set} → (P → Q) ↔ (not P ∨ Q)
material-implication {P}{Q} = prfₗ × prfᵣ
  where
  prfₗ : (P → Q) → not P ∨ Q
  prfₗ f with excluded-middle P
  prfₗ f | injₗ p = injᵣ (f p)
  prfₗ f | injᵣ ¬p = injₗ ¬p

  prfᵣ : not P ∨ Q → (P → Q)
  prfᵣ (injₗ ¬p) p = exfalso (¬p p)
  prfᵣ (injᵣ q) _ = q

disjunctive-syllogism : {P Q : Set} → (P ∨ Q) → not P → Q
disjunctive-syllogism (injₗ p) ¬p = exfalso (¬p p)
disjunctive-syllogism (injᵣ q) ¬p = q

composition : {P Q R : Set} → (P → Q) ∨ (P → R) → P → Q ∨ R
composition (injₗ pq) p = injₗ (pq p)
composition (injᵣ pr) p = injᵣ (pr p)

transposition : {P Q : Set} → (P → Q) ↔ (not Q → not P)
transposition {P}{Q} = prfₗ × prfᵣ
  where
  prfₗ : (P → Q) → not Q → not P
  prfₗ f ¬q p = ¬q (f p)

  prfᵣ : (not Q → not P) → P → Q
  prfᵣ f p with excluded-middle Q
  prfᵣ f p | injₗ q = q
  prfᵣ f p | injᵣ ¬q = exfalso (f ¬q p)

de-morgan : {P Q : Set} → not (P ∨ Q) ↔ (not P ∧ not Q)
de-morgan {P}{Q} = prfₗ × prfᵣ
  where
  prfₗ : not (P ∨ Q) → not P ∧ not Q
  prfₗ f = (λ p → f (injₗ p)) × (λ q → f (injᵣ q))

  prfᵣ : not P ∧ not Q → not (P ∨ Q)
  prfᵣ (¬p × ¬q) (injₗ p) = ¬p p
  prfᵣ (¬p × ¬q) (injᵣ q) = ¬q q

data Nat : Set where
  z : Nat
  s : Nat → Nat

_+_ : Nat → Nat → Nat
z + y = y
s x + y = s (x + y)

data _≡_ (n : Nat) : Nat → Set where
  refl : n ≡ n

_≢_ : Nat → Nat → Set
n ≢ m = not (n ≡ m)

sym : ∀ {n m} → n ≡ m → m ≡ n
sym refl = refl

cong : ∀ {n m} (f : Nat → Nat) → n ≡ m → f n ≡ f m
cong f refl = refl

subst : ∀ {n m} → (P : Nat → Set) → n ≡ m → P n → P m
subst P refl p = p

data _<_ : Nat → Nat → Set where
  z<s : ∀ {n} → z < s n
  s<s : ∀ {n m} → n < m → s n < s m

_>_ _≤_ _≥_ : Nat → Nat → Set
n > m = m < n
n ≤ m = (n < m) ∨ (n ≡ m)
n ≥ m = m ≤ n

≢-weaken : ∀ {n m} → s n ≢ s m → n ≢ m
≢-weaken p refl = p refl

≢-<∨> : ∀ {n m} → n ≢ m -> (n < m) ∨ (n > m)
≢-<∨> {z} {z} z≢z = exfalso (z≢z refl)
≢-<∨> {z} {s m} z≢sm = injₗ z<s
≢-<∨> {s n} {z} sn≢z = injᵣ z<s
≢-<∨> {s n} {s m} sn≢sm with ≢-weaken sn≢sm
≢-<∨> {s n} {s m} sn≢sm | n≢m with ≢-<∨> n≢m
≢-<∨> {s n} {s m} sn≢sm | n≢m | injₗ n<m = injₗ (s<s n<m)
≢-<∨> {s n} {s m} sn≢sm | n≢m | injᵣ m<n = injᵣ (s<s m<n)

z+n : ∀ n → (z + n) ≡ n
z+n n = refl

n+z : ∀ n → (n + z) ≡ n
n+z z = refl
n+z (s n) = cong s (n+z n)

plus-z : ∀ n → (z + n) ≡ (n + z)
plus-z n = sym (n+z n)

n<sn : ∀ n → n < s n
n<sn z = z<s
n<sn (s n) = s<s (n<sn n)

data Even : Nat → Set where
  ez : Even z
  ess : ∀ {n} → Even n → Even (s (s n))

data Odd : Nat → Set where
  o1 : Odd (s z)
  oss : ∀ {n} → Odd n → Odd (s (s n))

even+1 : ∀ {n} → Even n → Odd (s n)
even+1 ez = o1
even+1 (ess en) = oss (even+1 en)

odd+1 : ∀ {n} → Odd n → Even (s n)
odd+1 o1 = ess ez
odd+1 (oss x) = ess (odd+1 x)

plus-s : ∀ n m → (s n + s m) ≡ s (s (n + m))
plus-s z m = refl
plus-s (s n) m = cong s (plus-s n m)

double-even : ∀ n → Even (n + n)
double-even z = ez
double-even (s n) = subst Even (sym (plus-s n n)) (ess (double-even n))
