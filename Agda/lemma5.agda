data Either (a : Set) (b : Set) : Set where
  Left  : a → Either a b
  Right : b → Either a b

-- Define the lemma
lemma5 : {A B : Set} → A → B → Either A B
lemma5 a b = Left a