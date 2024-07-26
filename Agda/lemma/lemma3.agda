open import Agda.Builtin.Sigma

-- Define the lemma
lemma3 : {A B : Set} → Σ A (λ _ → A → B) → B
lemma3 (a , a→b) = a→b a