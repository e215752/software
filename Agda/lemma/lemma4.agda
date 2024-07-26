open import Agda.Builtin.Sigma

-- Define the lemma
lemma4 : {A B : Set} → B → Σ B (λ b → A → B)
lemma4 b = (b , λ _ → b)