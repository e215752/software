open import Relation.Binary.PropositionalEquality
open import Data.Nat
open import Data.Nat.Properties
open ≡-Reasoning  -- この行を追加

add-sym : (x y : ℕ) → (x + y) ≡ (y + x)
add-sym zero y = 
  begin
    zero + y
  ≡⟨⟩
    y
  ≡⟨ sym (+-identityʳ y) ⟩
    y + zero
  ∎
add-sym (suc x) y = 
  begin
    suc x + y
  ≡⟨⟩
    suc (x + y)
  ≡⟨ cong suc (add-sym x y) ⟩
    suc (y + x)
  ≡⟨ sym (+-suc y x) ⟩
    y + suc x
  ∎